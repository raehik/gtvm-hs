# To-dos
## 2024 overhaul
* re-enable disabled code:
  * flowchart (very easy, just binrep changes)
  * SL01 (shouldn't be hard)
  * hash/check stuff (? delete? vestigial bytepatch remnants?)
* related, get bytepatch rewrite working xd

## SCPTL
  * Load flowchart in too, tell about script type (utage, kaiwa, ...)

## SCPX
So I looked into writing a generic scenario format. The main problem applying it
to GTVM is that this game focuses on commands separate to text, rather than
in-text commands/macros, and I can't think of much useful stuff I can do for the
"structure" parts like that.

By having SCPX build *over* the main scenario format, we get clarity and
flexibility and don't have to write as much transformation code. We potentially
have a busier user-facing format, but I have ideas for that.

I need to explore scripts more so I can build up more useful "named" SCPX
commands, rather than using the direct SCP commands all the time.

  * allow easy compile-time switching between `-` and `_`
  * better parsing for `Speaker`s (allow many styles)
  * text should have `Maybe Text` for ID, opens up automated checking against
    original scenarios

## CLI
  * [ ] Add `patch-dir` command that concatenates all `.bin.yaml` in a directory
    (recursively) and applies, then for `.text.yaml`. So no pain over patch
    ordering!
  * [ ] csv-patch: add switch to discard safety info when generating patch

### Better errors
Biiig story. I hardly try to report useful errors at all.

## Tools
### SL01
#### Improve compression
Use LZO1X-999 compression instead of LZO1X-1. Decompression *should* be the
same.

*Update:* I tried this. I successfully wrapped lzo2 into Haskell. But alas,
Golden Time isn't able to decompress. My minilzo decompressor is fine. I imagine
it's incomplete decompressor that for some reason can't handle LZO1X-999.

Now there *is* still hope, if this mattered the tiniest bit (it doesn't). I
could hook the decompression function and replace it with a better one. Or, I
could use an entirely different compression format. Some one wrote a *super*
speedy LZ4 decompression routine for various ARM platforms!
https://community.arm.com/developer/ip-products/processors/b/processors-ip-blog/posts/lz4-decompression-routine-for-cortex-m0-and-later

### `gb_param.bin`, `sd_param.bin`, `vj_param.bin`
Each one appears to be a simple `[(header, contents)]` list archive, with the
file header as following:

  * `0x04` Some sort of type indicator?
    * `gb_param.bin` uses `MGB0`, `MGB1`
    * `sd_param.bin` uses `MSD0` -> `MSD7`
    * `sd_param.bin` uses `MSD0` -> `MSD7`
    * figures since `gb_param.bin` is just tons of text, while `sd_param.bin` is
      a lot more mixed
  * `0x04` Length in bytes

Every time I see a string, I count 0x40 bytes seemingly allocated to it. With
that in mind, and the overall format, I think these are fine to edit via direct
replacement. My UTF8 string extractor should find *all* valid UTF-8 strings
(above 3 characters), plus gives info about likely available space without us
having to reverse the format. We could potentially overwrite some data that
begins with a null -- but chances are it's going to be an int, which means LE,
which means only theoretically only 1/256 to be a null.

## Refactoring
  * Fully refactor SCP to use my `WithRefine` layer - means we can cut out tons
    of boilerplate.
