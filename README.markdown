# mc-stats-sb - Tool to generate Minecraft statistic scoreboards

This is a command line utility for generating and updating statistic scoreboards in Minecraft.

## Features
### Supported
- Generates valid datapack for versions 1.13+
- Generates function to update all scoreboards for 1.13+
### Planned
- Whitelist feature to disregard bots
- Complete compatiblity with 1.7+ 
  - Ability to generate and update scoreboards directly with the `scoreboards.dat` file
  - Stat names pre-flattening

## Usage

```
usage: statsbtool <mode> [OPTIONS]
Available options:
  -h, --help           Get this help text
  -v, --mc-version ARG Specify the targeted version of Minecraft
  --no-limit           Disables scoreboard name limit (1.18+)
  -s, --stats ARG      /path/to/player/stats/
```

## Build Instructions
Load the `build.lisp` file with your Common Lisp compiler of choice (NOTE: All testing and usage of program have been solely with SBCL)

With SBCL:
```
sbcl --load "build.lisp"
```

## License

Licensed under the MIT License.
