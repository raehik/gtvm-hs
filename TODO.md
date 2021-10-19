# To-dos
## Formats (JSON)
  * allow easy compile-time switching between `-` and `_`
  * Better parsing for `Speaker`s (allow many styles)

## CLI
  * [ ] Add `patch-dir` command that concatenates all `.bin.yaml` in a directory
    (recursively) and applies, then for `.text.yaml`. So no pain over patch
    ordering!
  * [ ] Add a few more docs in CLI (basic format explanations)
    * not too much & only facts, learner guide should be elsewhere
  * [ ] csv-patch: add switch to discard safety info when generating patch
  * [ ] patching: newtype for display-as-hex ints?

### Better errors
Biiig story. I hardly try to report useful errors at all.

## Tools
### Patching
#### Generalize, generalize, generalize
I'm slowly approaching a general "patch a stream S of type T" patch algorithm.
It's worth investigating further, see what I can figure out. I feel like it
could be massaged into a useful library+CLI tool. It gives fun safety guarantees
and enables pure and impure patcher implementations.

I feel like the logical conclusion to the algorithm is something that allows
arbitrary forwards-only stream operations. So:

```haskell
type PatchScript a = [Op a]
data Op a
  = OpCopy Int
  | OpAdd a
  | OpDel Int
```

My current approach essentially combines all these into a single step, where
OpDel is the length of OpAdd (thus in-place). I could keep them squished into
one step for type-enforced normalization, just would need to add an `Int` for
`OpDel` (instead of using the length from `OpAdd`). Buuuut at the end of the
day, there's no further use for a data type like this. What use other than
binary-based replacements would actually be useful? It'd be nice to maximally
generalize the stream concept, but I sadly don't see it being useful -- at least
not yet.

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

## Code structure
  * [ ] Having ripped MonadReader out from my program, inject it back in
        (gradually).
  * [ ] Figure out JSON exceptions? MonadError (with no MonadIO)?
  * [ ] Separate binary parsing etc. stuff (most of GTVM.Common) into it's own
        Raehik.Binary set of modules or something
  * [ ] QuickCheck tests for LinearPatch (would be very fun!)
