"""Print a hash of everything the rootfs (toolchain/build-learn-rootfs.sh) is
made from, which CI keys its cache of the rootfs directory by.

    python3 -I toolchain/learn-rootfs-key.py --plan PLAN --packages 'P Q ...'
        [--setting NAME=VALUE ...] PATH...

build-learn-rootfs.sh --key runs it after planning the wasm build. What goes
into the hash:

- the compiler, as the plan names it (its id and its ABI hash);
- every unit of the plan in the closure of the packages' libraries: a unit
  from cabal's store or from GHC by its id, which is already a hash of its
  own inputs (version, flags, dependencies), and one of this project's by its
  flags and the files of its package's directory, since its id says nothing
  of its sources;
- each PATH, a file or a directory's files: what the script reads besides
  the build (the wasi-sysroot's libraries, the page, the scripts themselves,
  material's stylesheet and the npm lock file its bundle is made with);
- the settings the script was run with.

So two runs print the same key exactly when they would pack the same
sources, though a library compiled again may not be byte for byte the same.
Directories and files starting with a dot, and dist-newstyle, are left out
of a directory's files.
"""

import argparse
import hashlib
import json
import os
import sys

parser = argparse.ArgumentParser()
parser.add_argument("--plan", required=True)
parser.add_argument("--packages", required=True)
parser.add_argument("--setting", action="append", default=[])
parser.add_argument("paths", nargs="*")
args = parser.parse_args()

plan = json.load(open(args.plan, encoding="utf-8"))
units = {u["id"]: u for u in plan["install-plan"]}
packages = args.packages.split()


def dependencies(unit):
    # A unit is a component (component-name) or, for a package cabal could
    # not split, the whole package with its components under "components".
    yield from unit.get("depends", [])
    yield from unit.get("exe-depends", [])
    for component in unit.get("components", {}).values():
        yield from component.get("depends", [])
        yield from component.get("exe-depends", [])


def is_library(unit):
    return unit.get("component-name") == "lib" or "lib" in unit.get("components", {})


todo = [i for i, u in units.items() if u["pkg-name"] in packages and is_library(u)]
missing = set(packages) - {units[i]["pkg-name"] for i in todo}
if missing:
    sys.exit(f"{args.plan} has no library of {', '.join(sorted(missing))}")
closure = set()
while todo:
    unit = todo.pop()
    if unit not in closure:
        closure.add(unit)
        todo.extend(dependencies(units.get(unit, {})))

digest = hashlib.sha256()


def feed(*parts):
    for part in parts:
        digest.update(part if isinstance(part, bytes) else str(part).encode())
        digest.update(b"\0")


def feed_file(path, name):
    with open(path, "rb") as f:
        feed(name, f.read())


def feed_tree(path):
    if os.path.isfile(path):
        feed_file(path, os.path.basename(path))
        return
    if not os.path.isdir(path):
        sys.exit(f"no such file or directory: {path}")
    for directory, subdirectories, files in os.walk(path):
        subdirectories[:] = sorted(d for d in subdirectories if not d.startswith((".", "dist-newstyle")))
        for name in sorted(files):
            if not name.startswith("."):
                full = os.path.join(directory, name)
                feed_file(full, os.path.relpath(full, path))


feed(plan["compiler-id"], plan["compiler-abi"], *sorted(args.setting))
for unit in sorted(closure):
    info = units.get(unit, {})
    feed(unit, json.dumps(info.get("flags", {}), sort_keys=True))
    if info.get("style") == "local":
        feed_tree(info["pkg-src"]["path"])
for path in args.paths:
    feed(path)
    feed_tree(path)
print(digest.hexdigest()[:32])
