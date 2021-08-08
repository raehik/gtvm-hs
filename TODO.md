# To-dos
## CLI
  * [ ] Add a few more docs in CLI (basic format explanations)
    * not too much & only facts, learner guide should be elsewhere
  * [ ] csv-patch: add switch to discard safety info when generating patch

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
  * [ ] QuickCheck tests for LinearPatch (would be very fun!)
