  * Have a parser wrapper:  
    `(MonadParsec e s m, Token s ~ Word8) => ? -> m (Either BS.ByteString a)`
  * Actually needs its own monad to gather unmatched bytes
  * Somehow has to detect if the inner parser works or not -- if so, it has to
    insert the unmatched bytes first
  * The way this parser works is that the unmatched bytes bytestring will
    continue to grow until a parser *fully matches*. Only when a parser returns
    successfully, can we then emit the unmatched bytes bytestring (if
    non-empty), empty it, then emit the wrapped parser output.

Bleh, honestly, too pointless. There's an easy alternative which is to emit
individual unmatched bytes, rather than a string. Less hassle and realistically
probably no slower, I doubt a `Either Word8 Segment -> Segment` fold (where
`Segment` has a `SegmentUnknown :: BS.ByteString -> Segment` constructor) is
that, like, slow. Much easier.

Wonder if I can grab the last Megaparsec error and stuff it into the unmatched
byte or something, lol.
