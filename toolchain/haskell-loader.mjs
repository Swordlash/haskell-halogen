import { execa } from "execa";
import { readFileSync, rmSync } from "node:fs";
import { basename, dirname, extname, resolve } from "node:path";
import { validate } from "schema-utils";

const schema = {
  type: "object",
  additionalProperties: false,
  required: ["executable", "build-directory"],
  properties: {
    "build-directory": { type: "string" },
    executable: { type: "string" },
    "system-tools": { type: "boolean" },
    "with-compiler": { type: "string" },
    "with-hc-pkg": { type: "string" },
    "with-hsc2hs": { type: "string" },
  },
};

export default async function () {
  const options = this.getOptions();
  validate(schema, options, {
    name: "Haskell loader",
    baseDataPath: "options",
  });

  const projectDirectory = dirname(this.resourcePath);
  const buildDirectory = resolve(projectDirectory, options["build-directory"]);
  const projectOption =
    extname(this.resourcePath) === ".project"
      ? `--project-file=${basename(this.resourcePath)}`
      : "--project-dir=.";

  const cabalArgs = [
    `--builddir=${buildDirectory}`,
    projectOption,
    ...(options["with-compiler"]
      ? [`--with-compiler=${options["with-compiler"]}`]
      : []),
    ...(options["with-hc-pkg"]
      ? [`--with-hc-pkg=${options["with-hc-pkg"]}`]
      : []),
    ...(options["with-hsc2hs"]
      ? [`--with-hsc2hs=${options["with-hsc2hs"]}`]
      : []),
  ];
  const resetCompilerCache = () =>
    rmSync(resolve(buildDirectory, "cache/compiler"), { force: true });

  resetCompilerCache();
  await execa("cabal", ["build", "all", ...cabalArgs], {
    cwd: projectDirectory,
    stdio: "inherit",
  });
  resetCompilerCache();
  const { stdout } = await execa(
    "cabal",
    ["-v0", ...cabalArgs, "exec", "--", "which", options.executable],
    { cwd: projectDirectory },
  );
  return readFileSync(stdout);
}
