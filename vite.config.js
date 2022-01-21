import { defineConfig } from "vite";
import elmPlugin from "vite-plugin-elm";

export default defineConfig({
  devServer: {
    hot: false,
    liveReload: true,
  },
  plugins: [
    elmPlugin({
      debug: false,
    }),
  ],
});
