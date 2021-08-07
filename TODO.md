# To-dos
## CLI
  * [ ] Add a few more docs in CLI (basic format explanations)
    * not too much & only facts, learner guide should be elsewhere

## Tools
### SL01
#### Improve compression
The `lzo` package on Hackage is minilzo built *into* Haskell (since it's pretty
much just `ghc minilzo.c`, doesn't need a Makefile). minilzo is apparently built
from lzo2, and only provides LZO1X-1 (fastest compression) and the LZO1X
decompressor (works for all LZO1X). I tested: `lzop -9` (= LZO1X-999) compresses
down to around LZ4's size.

I've tested: I'm able to bind to `lzo2`. However, only via dynamic linking,
which I'd prefer not to do while I don't know its Windows prevalence (and
generally I'd like to avoid dynamic linking if possible). It'd be nice if I
could build the whole library into Haskell? But unlikely.

In any case, they probably used `minilzo` for the game (my compressed files are
only 1-2% larger). So I could make my piece. But I'd like to try...

### Bytestring inserter
*2021-08-06: done*

Insert bytestrings at offsets in stream. Intended for inserting any binary into
any binary, but especially geared for C strings (null-terminated).

  * In-place edits only (relative to stream length, not I/O).
  * Non-linear, allowed to jump around file.
    * Could make a normal form, but it'd have to look a lot different. No
      offsets, just a "skip X", "write X" instruction list. (The normal form
      would do most of the safety checks for us too, without I/O.)
    * Actually that's sounding like an exceedingly Good Idea, if more effort

Example spec:

```yaml
- bytes: FF FF FF FF FF FF FF 00 # arbitrarily long
  length: 3 # optional
  offsets:
  - offset: 0x1234
    length: # optional
    null_terminates: # optional
    original_bytes: # optional
```

  * `bytes` is a plain bytestring. Must include null byte for C strings.
  * `length` is replacement bytestring length. Optional, if provided then
    checked against `bytes`.
  * `offsets` is a list of offsets to write the replacement bytestring at.

The `offsets` list:

  * `offset` is an address in the stream.
  * `length` is total length allowed to write for this offset.
  * `null_terminates` is the relative offset at which the original bytestring is
    expected to terminate, and there to only be nulls from this cursor point on.
  * `original_bytes` is the original bytes in the file, to check against while
    writing. *NOT FOR PUBLIC PATCH!*

Algorithm notes:

  * while writing, keep track of "have I actually changed any bytes for the
    current offset?", use this to attempt to determine if the offset has already
    been replaced
    * `null_terminates`: "well the next byte isn't a null but we haven't changed
      a byte yet so I will allow it until we do"
    * `original_bytes`: as regular but on non-match while unchanged, what
      happens depends on config (immediate fail or warning) -- also no need to
      re-run this after it fails, it can never recover after one difference

Normal form calculation:

  * Simplify to list of `(ByteString, offset_obj)`. For each:
    * Assert any provided replacement byte lengths and discard (`ByteString` stores length)
    * Expand `offsets` list
      * Assert any provided offset byte lengths against replacement and discard
        (normal form makes it useless other than for this assert)
  * Sort list by `offset.offset`
  * Replace each entry with `(consume_x :: Int, replace :: Replace)`, i.e.:
    * Keep track of file cursor, assert that next offset is past cursor
    * Remove offsets (keep the rest of the meta though)
    * essentially for every `(o1:o2:os)`, assert that `o1` doesn't clobber `o2`
      (`o1` offset+bytestring length < `o2` offset, though probably better to do
      it statefully)

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

## Code structure
  * [ ] Having ripped MonadReader out from my program, inject it back in
        (gradually).
  * [ ] Figure out JSON exceptions? MonadError (with no MonadIO)?
  * [ ] Separate binary parsing etc. stuff (most of GTVM.Common) into it's own
        Raehik.Binary set of modules or something
