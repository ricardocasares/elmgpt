import elm from "vite-plugin-elm";
import css from "@tailwindcss/vite";
import { defineConfig } from "vite";

export default defineConfig({
  plugins: [css(), elm()]
});
