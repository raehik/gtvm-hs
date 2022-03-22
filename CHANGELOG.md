## 0.9.0 (Unreleased)
  * new tool: SCP text replace (for translation)
  * new tool: SCPTL -- text replace made useful
  * remove patch feature: moved to own library+executable at
    https://github.com/raehik/bytepatch ,
    https://hackage.haskell.org/package/bytepatch
  * rewrite CLI, disabled some features while refactoring further
    * CSV feature temporarily disabled (need to bring bytepatch in again)
  * early SCPX functionality
  * rewrite flowchart handling code, schema
    * No more intermediate type. We get less and simpler code. If it were more
      useful (not just the index generation), I would add one again. That way
      we recover the original approach ("lexed" and "parsed" versions), but tons
      better behaviour, safety etc.
  * various cleanup, interesting ideas
    * pattern for using single type for binary and JSON (and Haskell) composed
      of two mini libraries, using for flowchart
  * more SCP format documentation

## 0.8.0 (2021-08-26)
  * patch offsets now work from a base offset
    * prompting story: by calculating the relevant base offset, a user patching
      an ELF can now write offsets as virtual addresses and have them translated
      to physical file offsets!
    * absolute offsets can also be specified to confirm correct offset
      calculation
  * further patcher refactoring: partially generic over patch/stream type,
    patch-time meta is reused instead of wrapped in pretty patch format

## 0.7.0 (2021-08-16)
  * fix patcher runtime checks
  * refactor patcher: internals more generic & useful, text/binary patch formats
    now the same (no "simplified" one offset format), better potential error
    reporting (not yet implemented)

## 0.6.0 (2021-08-09)
  * add CSV -> text patch file generator
  * bytestrings in JSON/YAML must now use format `00 01 EF FF` etc.
    *(I had no choice but to implement my own format, since JSON explicitly
    disallows encoding byte arrays.)*
  * refactor patcher internals (somewhat modular)

## 0.5.0 (2021-08-05)
I don't expect to alter the CLI in any more major ways, so go mad with scripts.

  * add binary/string patcher tool
  * some documentation; `sound_se.pak` repacking appears functional
  * partial CLI redesign (fewer options, more commands)

## 0.4.0 (2021-08-04)
  * CLI: partial redesign (internals overhauled)
  * add `SL01` LZO1x en/decoding tool (output not tested in game)
  * add initial `sound_se.pak` pack/unpack support (output not tested in game)

## 0.3.0 (2021-08-02)
Release primarily to ensure the latest version displayed on GitHub has the
correct executable name (`gtvm-hs`, not `gtvm`).

  * CLI: make prettify an option (still on by default)
  * CLI: fix: always allow printing text

## 0.2.0 (2021-08-02)
  * CLI interface to SCP manipulation

## 0.1.0 (2021-08-01)
Initial release.

  * various messy tools, primarily parsing/serializing
  * an initial CLI interface to `flow_chart.bin` manipulation
