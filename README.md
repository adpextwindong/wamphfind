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

# TODO

- Duration calculation
- Recursive directory searching
- Absolute file path handling with basenames
- Input filepath args
- Web Audio API extension list
- forkMapM parallelism
- Prompt on existing outputfile
- Exception Handling
- Symbolic Link Handling?
