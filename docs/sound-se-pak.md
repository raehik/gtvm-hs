# `sound_se.pak`
## Format overview
*2021-08-03, raehik*
A short header, followed by a bunch of file info entries (filename, pointer,
length), followed by data.

### Header:
Total length 0x08.

```
0x04 W32LE          Number of entries in file table
0x04 W32LE          Unknown
```

### File table entry:
Total length 0x20.

```
0x04 W32LE          Absolute offset to start file
0x04 W32LE          File length in bytes
0x18 Bytestring     File name
```

I was able to extract the first file manually with just the offset and length.

## Actual file
  * 194 (0xC2) entries
