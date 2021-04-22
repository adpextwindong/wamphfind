# wamphfind

[Web Amp](https://github.com/captbaritone/webamp) Haskell Find Utility by George Takumi Crary

Generates the tracklist JSON seen [here](https://github.com/captbaritone/webamp/blob/master/examples/minimal/index.html#L16).

# Usage

See --help for more details

## Search local directory and output to target file
```
./wamphfind -o output.json
```

## Search local directory and output to STDOUT for piping to another process such as JQ.
```
./wamphfind
```

## Add a directory basename to the url metadata
```
./wamphfind -a tracks
```
RESULT: "url":"GoldenSpade.mp3" ==> "url":"tracks/GoldenSpade.mp3"

Other examples
```
./wamphfind -a track/
./wamphfind -a syndicate/tracks
```

NOTE: If the trailing slash is ommited it will be added

## Use filename as track title metadata
```
./wamphfind -n
```

## Use filename with extensions dropped as track title metadata
```
./wamphfind -e
```
NOTE: This will result in foo.mp3.zip -> foo

## Pretty print the JSON output
```
./wamphfind -p
```

# TODO

- Duration calculation
- Recursive directory searching
- ~~absolute file path handling with basenames~~
- Input filepath args
- Web Audio API extension list
- forkMapM parallelism
- Prompt on existing outputfile
- Exception Handling
- Symbolic Link Handling?

# BUILD DEPENDENCIES

[idiii (with the build process ported to Stack)](https://github.com/adpextwindong/idiiiFORK)
Look at stack.yaml to control where that goes.
