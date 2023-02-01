import VirtualAudioContext from './elm-web-audio.js'
import { Elm } from './Main.elm'

const SETTINGS_STORAGE_KEY = 'settings'

const randomSeeds = new Uint32Array(1)

crypto.getRandomValues(randomSeeds)

const savedSettings = localStorage.getItem(SETTINGS_STORAGE_KEY) || null

const ctx = new AudioContext()
const virtualCtx = new VirtualAudioContext(ctx)

const app = Elm.Main.init({
  flags: {
    initialSeed: randomSeeds[0],
    settings:
      savedSettings === null ? savedSettings : JSON.parse(savedSettings),
  },
})

app.ports.saveSettings.subscribe(function (settings) {
  localStorage.setItem(SETTINGS_STORAGE_KEY, JSON.stringify(settings))
})

app.ports.toWebAudio &&
  app.ports.toWebAudio.subscribe((nodes) => {
    virtualCtx.update(nodes)
  })
