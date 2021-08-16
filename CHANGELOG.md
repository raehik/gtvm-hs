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
