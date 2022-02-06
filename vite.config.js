import { defineConfig } from "vite";
import elmPlugin from "vite-plugin-elm";

export default defineConfig({
  devServer: {
    hot: false,
    liveReload: false,
  },
  plugins: [
    elmPlugin({
      optimize: true,
      debug: false,
    }),
  ],
});
