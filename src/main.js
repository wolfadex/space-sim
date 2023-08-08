import VirtualAudioContext from "./elm-web-audio.js";
import { Elm } from "./Main.elm";

// HACKS -- BEGIN
Object.defineProperty(HTMLElement.prototype, "___capturePointer", {
  set(pointerId) {
    if (pointerId == null) {
      this.releasePointerCapture(this.___pointerId);
    } else {
      this.___pointerId = pointerId;
      this.setPointerCapture(pointerId);
    }
  },
});
Object.defineProperty(SVGElement.prototype, "___capturePointer", {
  set(pointerId) {
    if (pointerId == null) {
      this.releasePointerCapture(this.___pointerId);
    } else {
      this.___pointerId = pointerId;
      this.setPointerCapture(pointerId);
    }
  },
});

// HACKS -- END

const SETTINGS_STORAGE_KEY = "settings";

const randomSeeds = new Uint32Array(1);

crypto.getRandomValues(randomSeeds);

const savedSettings = localStorage.getItem(SETTINGS_STORAGE_KEY) || null;

const ctx = new AudioContext();
const virtualCtx = new VirtualAudioContext(ctx);

const app = Elm.Main.init({
  flags: {
    initialSeed: randomSeeds[0],
    settings:
      savedSettings === null ? savedSettings : JSON.parse(savedSettings),
  },
});

app.ports.saveSettings.subscribe(function(settings) {
  localStorage.setItem(SETTINGS_STORAGE_KEY, JSON.stringify(settings));
});

app.ports.toWebAudio &&
  app.ports.toWebAudio.subscribe((nodes) => {
    virtualCtx.update(nodes);
  });
