# Space Sim

![pr status](https://github.com/wolfadex/space-sim/actions/workflows/test-pr.yml/badge.svg)

⚠️ WIP ⚠️

Playable at https://space-sim.netlify.app/

A 4X space game inspired by Dwarf Fortress.

<img src="./Screenshot 2022-01-23_galaxy-view.png" />
<img src="./Screenshot 2022-01-23_solar-view.png" />

## Features

- Galaxy generation
  - Solar system generation
    - Star generation
    - Planet generation
- Civilizations, with:
  - Population size
  - Reproduction rate
  - Names
  - Happiness
  - Knowledge (can be gained, not yet lost)
  - Logs of important events
  - Revolts (civs split when too unhappy)
  - Expansion into nearby planets/solar systems
- "Gameplay"
  - 2D (data first) or 3D (beauty first) view of the galaxy
  - 3D settings

## Get Started

### Development

```bash
npm install
npm run dev
```

```bash
npm run review-watch
```

```bash
npm run test-watch
```

### Production

```bash
npm ci
npm run build
```

To learn more about Elm, check out [Elm's official homepage](https://elm-lang.org/).
