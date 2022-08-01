# mc-stats-sb - Tool to generate Minecraft statistic scoreboards

This is a command line utility for generating and updating statistic scoreboards in Minecraft.

## Features
### Supported
- Generates valid datapack for versions 1.13+
- Generates function to generate scoreboards for versions 1.12+
- Generates function to update all scoreboards for 1.12+
- Whitelist feature to disregard bots
### Planned
- Complete compatiblity with 1.7+ 
  - Ability to generate and update scoreboards directly with the `scoreboards.dat` file

## Usage

```
usage: statsbtool <generate|update> [OPTIONS]
Available options:
  -h, --help           Get this help text
  -v, --mc-version ARG Specify the targeted version of Minecraft
  -s, --stats ARG      /path/to/player/stats/
```

To generate scoreboards you must specify the version and use the `generate` mode.
```
./statsbtool generate -v 1.19
```
This will create a datapack in the `output` directory called `statsb` which contains the function that creates all the scoreboards. The function is run automatically upon loading the datapack.

In case you do not generate the scoreboards upon creating the world, you can update them via `update`.
This requires you supply the `stats` folder which is located in the world save folder.
```
./statsbtool update -v 1.19 -s stats/
```
This creates `update_scoreboards.mcfunction` in the output directory, which you can then run on your server to update the scoreboards for all players.

If you wish to exclude certain players from gaining updates (e.g. carpet bots), you can input a whitelist like so:
```
./statsbtool update -v 1.19 -s stats/ -w whitelist.json
```
The whitelist file must follow the same structure as a regular whitelist.json found amongst minecraft server files, however, you may exclude the uuid.

## Build Instructions
Load the `build.lisp` file with your Common Lisp compiler of choice (NOTE: All testing and usage of program have been solely with SBCL)

With SBCL:
```
sbcl --load "build.lisp"
```

## License

Licensed under the MIT License.
