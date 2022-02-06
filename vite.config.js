import { defineConfig } from "vite";
import elmPluginOptimized from "./vite-elm-fork"

export default defineConfig({
  devServer: {
    hot: false,
    liveReload: false,
  },
  plugins: [
    elmPluginOptimized({
      debug: false,
    }),
  ],
});
