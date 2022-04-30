# Golden Time Vivid Memories: Haskell Toolset
Various Haskell tools for working with the Golden Time Vivid Memories Vita game,
like parsing and reserializing assets.

No copyrighted material from the game stored.

## Obtaining
For the most recent "stable" release, grab the latest release from the sidebar
on the GitHub project. For the latest changes, check out the Actions tab on the
GitHub project and click on the top entry. Static Linux and Windows executables
are available.

## Usage
Invoke on command line. The CLI is intended to be a discoverable "toolbox", so
try invoking different commands and viewing the help.

As of 0.9.0, patching (`gtvm-hs patch`) is handled by another tool. See
[bytepatch](https://github.com/raehik/bytepatch).

## Building
Use Stack or Cabal, GHC 9.2. No real setup needed. (Send me a ping or make an
issue if you need more help.)

## Working with the codebase
Quick notes:

  * We do a lot of binary format parsing, specifically to JSON-friendly types.
    When I say "parse", I mean parsing binary data to a Haskell data type. When
    I say "serialize", I mean the reverse. ("Printing" is essentially covered
    via encoding to JSON.)
