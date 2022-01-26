# Changelog

I will not be using Semantic Versioning for this project. Instead, versioning will be a combination of settings changes, simulation changes, and changes that affect the save data in the form `<game play>.<save data>.<settings>`.

E.g. the first version will be `0.0.0`. If I then change the settings then the next version will be `0.0.1`. If I then change game play, then the next version will be `1.0.1`. If I then change how save data is formatted twice, we'll get `1.1.1` followed by `1.2.1`.

## v0.0.0 - WIP

### Game Play

- 2 modes of play, Observation and Participation.
- Observation allows you to view the entire galaxy and watch as the game plays itself.
- Participation allows you to create your own civilization and influence it.
  - You're view of the galaxy is also limited to what your civilization is aware of.
  - If there's a revolution in your civilization, then your retain control of the non-revolting populace.

### Save Data

- TBD

### Settings

- These are mostly to help with performance, but may also be beneficial to those with visual impairments
- Saved to localStorage
