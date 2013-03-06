A few helpful little utilities for Mathematica.

## Expressions
`ContainsQ` — the opposite of `FreeQ`

## Strings
`StringShort` — `Short` for strings

`StringContainsQ` — the opposite of `StringFreeQ`

`ToStandardName` — create a simple camel-cased variant of any string

`StringDecompose` — helpful little guy for splitting strings

## Dates
`DateInterval` — convert an arbitrary precision date into the second-level precision date range it specifies

`FromUnixTime` — translates Unix "epoch time" into a value compatible with `AbsoluteTime`

`FindDateDivisions` — a mediocore function for finding sensible date divisions

## Lists
## Graphics
## Archiving
There's a selection of archive functions that expand on `Import` functionality of webpages by "archiving" a local copy of the original source document. This is particularly useful for comparative studies.

`ArchiveImport` is the primary function, supplemented by `ArchiveQ`, `ArchiveFile`, `ArchiveSave`, `ArchiveData`, `ArchiveInfo`