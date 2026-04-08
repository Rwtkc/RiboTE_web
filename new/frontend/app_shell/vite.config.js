import path from "node:path";
import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";

export default defineConfig({
  plugins: [react()],
  base: "/react/app_shell/",
  build: {
    outDir: path.resolve(__dirname, "../../www/react/app_shell"),
    emptyOutDir: true,
    assetsDir: "assets",
    rollupOptions: {
      output: {
        entryFileNames: "assets/app-shell.js",
        chunkFileNames: "assets/[name].js",
        assetFileNames: ({ name }) => {
          if (name && name.endsWith(".css")) {
            return "assets/app-shell.css";
          }

          return "assets/[name][extname]";
        }
      }
    }
  }
});
