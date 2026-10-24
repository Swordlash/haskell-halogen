"""Register a layer of packages in the playground's rootfs (learn/).

    python3 -I toolchain/learn-inject.py --db DB --so-dir DIR --hi-dir DIR \\
      --present DB... --source DB... --ghc-version V --listing FILE ROOT...

Called by toolchain/build-learn-rootfs.sh. A layer is the closure of the
ROOT packages (package names, looked up in the --source DBs, the project's
own last) minus every unit a --present DB has already: the global package DB
and the layers below this one. For each unit in it this copies

  * its .so into --so-dir,
  * its .dyn_hi tree into <--hi-dir>/<unit id>,
  * its .conf into --db, with every path relative to ${pkgroot}, the
    directory --db is in, so that it holds wherever the archive is extracted.

The boot packages' .conf are written so too. try-miso writes ${topdir}, which
is the same directory to GHC for the global DB, but ghc-pkg does not expand
it, so ghc-pkg check could not check them, and it means nothing in a DB of
its own. The caller runs ghc-pkg recache and check afterwards.

--listing gets the layer's unit ids and the .so paths, for the size report.
"""

import argparse
import glob
import os
import re
import shutil
import sys

parser = argparse.ArgumentParser()
parser.add_argument("--db", required=True)
parser.add_argument("--so-dir", required=True)
parser.add_argument("--hi-dir", required=True)
parser.add_argument("--present", action="append", default=[])
parser.add_argument("--source", action="append", default=[])
parser.add_argument("--ghc-version", required=True)
parser.add_argument("--listing", required=True)
parser.add_argument("roots", nargs="+")
args = parser.parse_args()


def parse(path):
    """A .conf's fields, each a list of its words (the name a string)."""
    fields, key = {}, None
    for line in open(path, encoding="utf-8"):
        line = line.rstrip("\n")
        m = re.match(r"^([A-Za-z0-9-]+):\s*(.*)$", line)
        if m:
            key = m.group(1)
            fields[key] = m.group(2)
        elif key:
            fields[key] += " " + line.strip()
    return {k: v.strip() if k == "name" else v.split() for k, v in fields.items()}


def units(db):
    return {f["id"][0]: (path, f) for path in glob.glob(os.path.join(db, "*.conf")) for f in [parse(path)]}


present = {}
for db in args.present:
    present.update(units(db))
available, by_name = {}, {}
for db in args.source:
    found = units(db)
    available.update(found)
    by_name.update({f["name"]: uid for uid, (_, f) in found.items()})

closure, seen = [], set()


def visit(uid, needed_by):
    if uid in seen or uid in present:
        return
    seen.add(uid)
    if uid not in available:
        sys.exit(f"{uid}, which {needed_by} depends on, is in none of {', '.join(args.source)}")
    for dep in available[uid][1].get("depends", []):
        visit(dep, uid)
    closure.append(uid)


for name in args.roots:
    if name in {f["name"] for _, f in present.values()}:
        continue
    if name not in by_name:
        sys.exit(f"{name} is in none of {', '.join(args.source)}: is it built?")
    visit(by_name[name], "the layer")

pkgroot = os.path.dirname(os.path.abspath(args.db))


def relative(path):
    rel = os.path.relpath(os.path.abspath(path), pkgroot)
    return "${pkgroot}" if rel == "." else "${pkgroot}/" + rel


def strip(text, field):
    return re.sub(rf"(?m)^{field}:.*(\n[ \t].*)*\n?", "", text)


def put(text, field, value):
    return re.sub(rf"(?m)^{field}:.*(\n[ \t].*)*", f"{field}:\n    {value}", text)


os.makedirs(args.so_dir, exist_ok=True)
os.makedirs(args.db, exist_ok=True)
sos, so_total, hi_total = [], 0, 0
for uid in closure:
    path, f = available[uid]
    if f.get("extra-libraries"):
        sys.exit(f"{uid} links C libraries ({' '.join(f['extra-libraries'])}), which nothing here provides")
    so_bytes = hi_bytes = 0
    for lib in f.get("hs-libraries", []):
        name = f"lib{lib}-ghc{args.ghc_version}.so"
        dirs = f.get("dynamic-library-dirs", [])
        src = next((os.path.join(d, name) for d in dirs if os.path.exists(os.path.join(d, name))), None)
        if src is None:
            sys.exit(f"{uid}: no {name} in its dynamic-library-dirs; was it built with shared: True?")
        dst = os.path.join(args.so_dir, name)
        shutil.copyfile(src, dst)
        sos.append(dst)
        so_bytes += os.path.getsize(src)
    hidir = os.path.join(args.hi_dir, uid)
    os.makedirs(hidir, exist_ok=True)
    for importdir in f.get("import-dirs", []):
        for root, _, files in os.walk(importdir):
            for file in files:
                if file.endswith(".dyn_hi"):
                    src = os.path.join(root, file)
                    dst = os.path.join(hidir, os.path.relpath(src, importdir))
                    os.makedirs(os.path.dirname(dst), exist_ok=True)
                    shutil.copyfile(src, dst)
                    hi_bytes += os.path.getsize(src)
    text = open(path, encoding="utf-8").read().replace("\r", "")
    for field in ("import-dirs", "library-dirs", "library-dirs-static"):
        text = put(text, field, relative(hidir))
    text = put(text, "dynamic-library-dirs", relative(args.so_dir))
    # Nothing in the page reads documentation, data files or C headers, and
    # ghc-pkg check would fail on the build machine's paths they name.
    for field in ("haddock-interfaces", "haddock-html", "data-dir", "pkgroot", "include-dirs", "includes"):
        text = strip(text, field)
    with open(os.path.join(args.db, uid + ".conf"), "w", encoding="utf-8") as conf:
        conf.write(text)
    so_total += so_bytes
    hi_total += hi_bytes
    print(f"    {f['name']:32} {so_bytes / 1e6:7.2f} MB .so  {hi_bytes / 1e6:6.2f} MB .dyn_hi")
print(f"    {len(closure)} units, {so_total / 1e6:.1f} MB .so, {hi_total / 1e6:.1f} MB .dyn_hi")

with open(args.listing, "w", encoding="utf-8") as listing:
    listing.writelines(f"unit {uid}\n" for uid in closure)
    listing.writelines(f"so {so}\n" for so in sos)
