#!/usr/bin/env node
// The Elm compiler, for vite-plugin-elm. It writes the characters U+2028 and U+2029 into string
// literals as they are (elm-markdown matches them as whitespace), and the plugin parses the output
// as ES2015, where they end a string. This runs `elm` and writes them as escapes instead.
import { spawnSync } from "node:child_process";
import { readFileSync, writeFileSync } from "node:fs";

const args = process.argv.slice(2);
const { status, error } = spawnSync("elm", args, { stdio: "inherit" });
if (error) throw error;

const output = args[args.indexOf("--output") + 1];
if (status === 0 && args.includes("--output") && output.endsWith(".js")) {
  const js = readFileSync(output, "utf8");
  writeFileSync(output, js.replaceAll("\u2028", "\\u2028").replaceAll("\u2029", "\\u2029"));
}
process.exit(status ?? 1);
