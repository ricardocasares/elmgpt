import elm from "vite-plugin-elm";
import css from "@tailwindcss/vite";
import { defineConfig } from "vite";

export default defineConfig({
  plugins: [
    css(),
    elm({
      // See scripts/elm.mjs: it escapes characters the plugin can't parse. The path is from the
      // project root, where npm runs Vite.
      nodeElmCompilerOptions: {
        pathToElm: "./scripts/elm.mjs",
      },
    }),
  ],
});
