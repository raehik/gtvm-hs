# Golden Time Vivid Memories: Haskell Toolset
Various Haskell tools for working with the Golden Time Vivid Memories Vita game,
like parsing and reserializing assets.

No copyrighted material from the game stored.

## Usage
Invoke on command line. The CLI is intended to be a discoverable "toolbox", so
try invoking different commands and viewing the help.

As of 0.9.0, patching (`gtvm-hs patch`) is handled by another tool. See
[bytepatch](https://github.com/raehik/bytepatch).

## Building
Cabal, GHC 9.10 or 9.6. (9.8 does not work due to a `blake3` bug.)

Comes with a Nix flake that raehik uses for development and CI.

## License
Provided under the MIT license. See `LICENSE` for license text.
