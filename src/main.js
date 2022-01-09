import { Elm } from "./Main.elm";

const randomSeeds = new Uint32Array(1);
crypto.getRandomValues(randomSeeds);
const app = Elm.Main.init({
  flags: {
    seed0: randomSeeds[0],
  },
});
