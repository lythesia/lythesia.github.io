title: id3
date: 2014-03-17 23:59:27
categories: wiki
tags: [id3tag, mus]
---

## ID3v1

size: 128 bytes at eof

  field  |   range    |  size  | extra
---------|------------|--------|-------
 header  |  00 -  02  | 3      | "TAG"
 title   |  03 -  32  | 30     |
 artist  |  33 -  62  | 30     |
 album   |  63 -  92  | 30     |
 year    |  93 -  96  | 4      | 4 digit
 comment |  97 - 126  | 30(28) | `track#` placed at last 2 byte, if comment over 28 byte then no `track#`
 0-pad   | 125 - 125  | 1      | *optional*: only set under `28`, if set `track#` placed `0`
 track   | 126 - 126  | 1      | *optional*: only set under `28`, placed `track#`; invalid when `0-pad` not `0`
 genre   | 127 - 127  | 1      | index in list of genres, or `255`



## ID3v2

* *variable* size
* usually place at start of file, yet allow end or combined, follow [id3.org](http://id3.org/id3v2.4.0-struture) spec:
  > 1. Prepend the tag.
  > 2. Prepend a tag with all vital information and add a second tag at 
       the end of the file, before tags from other tagging systems. The
       first tag is required to have a SEEK frame.
  > 3. Add a tag at the end of the file, before tags from other tagging
       systems. 
* detect approach:
  > 1. Look for a prepended tag using the pattern: `$49 44 33 yy yy xx zz zz zz zz`, `$49 44 33` - `'ID3'`, `$yy < $ff` - version number, `$xx` - flag, `$zz < $80` - size
  > 2. If a SEEK frame was found, use its values to guide further
        searching.
  > 3. Look for a tag footer, scanning from the back of the file.
* consist of *frame*

### ID3v2.2:

* considered obsolete

### ID3v2.3

* most widely used
* allow multi-values in a frame, `'/'` character to separate

### ID3v2.4

* utf-8 support
* separate character: from `'/'` to `null byte`

### tag header

size: 10 bytes

field     |range        |size |extra
----------|-------------|-----|------
header    | 00 - 02     | 3   | "ID3"
ver_major | 03 - 03     | 1   | V2.3: 03; V2.4: 04
ver_minor | 04 - 04     | 1   |
flag      | 05 - 05     | 1   | format as `abc00000`:</br> a -- if unsync</br> b -- if has extended head</br> c -- if only experimental
size      | 06 - 09     | 4   | include *frame* and *extended header*, but **NOT** include above 10 bytes of *tag header*</br> format as `(0xxxxxx){4}`, heighest bit of each byte is 0, **take off** when computing

### frame header

size: 10 bytes

field     |range    |size |extra
----------|---------|-----|------
frame ID  | 00 - 03 | 4   | frame indicator, referencing ID table
size      | 04 - 07 | 4   | **NOT** include frame header, **NOT** less than 1</br> format as `(xxxxxxxx){4}`, all 4 bytes used
flags     | 08 - 09 | 2   | format as `abc00000 ijk00000`</br> a -- if ignore this frame(by tag)</br> b -- if ignore this frame(by file)</br> c -- if readonly(generally no one cares)</br> i -- if compressed, one byte of two BCD codes</br> j -- if encrypt</br> k -- group indicator, if in same grousetp with other frame

### frame ID table

2.4.0:

ID    |field
------|------
TDRC  | recording date
TDRL  | release date
TDOR  | original release date
TSOP  | artist sort order
TSOA  | album sort order

\>= 2.3.0:

ID    |field
------|------
TPE1  | artist
TALB  | album
TIT2  | title
TYER  | year: ascii number
TCON  | genre: `'($genre_no)'`
TPOS  | disc number
TRCK  | trackno: `N(No.#)/M(total#)`, both ascii number(means that's string?)
TPE2  | albumartist
TSO2  | albumartist sort order
XSOP  | artist sort order(obsolete)
XSOA  | album sort order(obsolete)
TCMP  | compilation
TORY  | originaldate
TCOM  | composer
TPE3  | conductor
TEXT  | lyricist
TPE4  | remixer
TPUB  | publisher or label
TIT3  | subtitle
TMED  | media type
COMM  | comment: enc`$xx`, lang`$xx xx xx`, short desc ended with `$00`, actual text`<...>`
TXXX  | user defined text info(see below for details)

obsolete frames 2.2.0:

ID    |field
------|------
TP1   | artist
TP2   | albumartist
TAL   | album
TT2   | title
TYE   | date
TCO   | genre
TPA   | disc
TRK   | track
TSP   | artist sort order
TS2   | albumartist sort order
TSA   | album sort order
TCP   | compilation
COM   | comment: same as *COMM* in v2.3 or above

playback staffs:

* replay gain: labeld as `RGAD`, `RVA2(v2.4)` or `XRVA(v2.3 compatibility & experimental)` in lagacy format

**Preferred** means to stroe this sort of metadata is use of *TXXX* key/value pair frame, see below.

TXXX header:

field     |format
----------|-------
frame ID  | "TXXX"
size      | `$xx xx xx xx`
flags     | `$40 $00`

TXXX frame struct:

field     |format
----------|-------
encoding  | `$00` - iso-8859-1, `$01` - utf-16 with BOM, `$02` - utf-16 without BOM, `$03` - utf-8
desc      | <key string> ended with `$00`
value     | <value string>

meta              |key                      |value format
------------------|-------------------------|-------------
track replay gain | "REPLAYGAIN_TARCK_GAIN" | `[-]a.bb dB`: `a` - integal portion(整数部分) [0-9], `bb` - decimal portion(小数部分)
track peak        | "REPLAYGAIN_TARCK_PEAK" | `c.dddddd`: `c` - 0 or 1(1.000000: digital full scale)
album replay gain | "REPLAYGAIN_ALBUM_GAIN" | -
album peak        | "REPLAYGAIN_ALBUM_PEAK" | -

<!--- vim: set ft=mkd: -->
