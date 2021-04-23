# wamphfind

[Web Amp](https://github.com/captbaritone/webamp) Haskell Find Utility by George Takumi Crary

Generates the tracklist JSON seen [here](https://github.com/captbaritone/webamp/blob/master/examples/minimal/index.html#L16).

Made for [Fresh2Fresh](http://fresh2fresh.info). Check him out.

# Example

```shell
takumi@~/dev/wamphfind/example
λ ls -l
total 86660
-rwxr-xr-x 1 takumi なし 13656675 Apr 20 11:30 'Timothy Seals - A New Dawn (Cover Edition) - 01 Pyre Light (Cover).mp3'
-rwxr-xr-x 1 takumi なし  7768447 Apr 20 11:30 'Timothy Seals - A New Dawn (Cover Edition) - 02 Silent Thunder (Cover).mp3'
```

Ran with ```./wamphfind -p``` will result in:

```json
[
    {
        "url": "Timothy Seals - A New Dawn (Cover Edition) - 02 Silent Thunder (Cover).mp3",
        "metaData": {
            "track": "2",
            "album": "A New Dawn (Cover Edition)",
            "year": null,
            "title": "Silent Thunder (Cover)",
            "artist": "Timothy Seals"
        }
    },
    {
        "url": "Timothy Seals - A New Dawn (Cover Edition) - 01 Pyre Light (Cover).mp3",
        "metaData": {
            "track": "1",
            "album": "A New Dawn (Cover Edition)",
            "year": null,
            "title": "Pyre Light (Cover)",
            "artist": "Timothy Seals"
        }
    }
]
```

Which you can drop in your into your WinAmp initaltracks. If you're sticking your tracks in a directory like 'tracks/' then use the -a flag to appropriately set the url.

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
    * [Handle errors like ENOTDIR and others listed here.](https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html)
- Symbolic Link Handling?

# BUILD DEPENDENCIES

[idiii (with the build process ported to Stack)](https://github.com/adpextwindong/idiiiFORK)
Look at stack.yaml to control where that goes.

# LICENSE

See [LICENSE](LICENSE). If you like it buy me a beer will ya.
