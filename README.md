# Play [Space Sim](https://space-sim.netlify.app/)

## ⚠️ WIP ⚠️

[![Netlify Status](https://api.netlify.com/api/v1/badges/cb086d27-2785-46ec-b180-a4ce21231f58/deploy-status)](https://app.netlify.com/sites/space-sim/deploys)

A space game inspired by Dwarf Fortress.

<img src="./Screenshot 2022-01-25.png" />

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

## Development

If you're using Nix and Direnv:

```bash
direnv allow
npm run dev
```

If you're only using Nix and not Direnv

```bash
npm install
npm run dev
```

If you're only using Direnv and not Nix

- Install Node

```bash
direnv allow
npm run dev
```

If you're using neither Direnv or Nix

- Install Node

```bash
npm install
npm run dev
```

<br />
<br />

Built with ♥ using <a href="https://elm-lang.org/"><img alt="Elm" src="./elm-favicon.ico" width=20></a>
