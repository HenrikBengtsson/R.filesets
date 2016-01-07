library("R.filesets")

message("*** ChecksumFile / ChecksumFileSet")

## Empty / missing
dfZ <- ChecksumFile()
print(dfZ)

dfZ <- ChecksumFile(NA_character_)
print(dfZ)


## Example files
path <- system.file("exData", "dataSetA,original", package="R.filesets")
print(path)

## Setting up a file set
ds <- GenericDataFileSet$byPath(path)
print(ds)

## Create copy (so that we can write checksum files)
pathT <- tempdir()
dsC <- copyTo(ds, path=pathT, overwrite=TRUE)
print(dsC)

## Checksum set
dsCZ <- getChecksumFileSet(dsC)
print(dsCZ)

validate(dsCZ, verbose=TRUE)

print(readChecksums(dsCZ))

## Single checksum file
dfC <- dsC[[1]]
print(dfC)
dfCZ <- getChecksumFile(dfC)
print(dfCZ)

## Calling getChecksum() on an *.md5 file should not
## create an *.md5.md5 file
pathnameZZ <- sprintf("%s.md5", getPathname(dfCZ))
stopifnot(!isFile(pathnameZZ))
print(getChecksum(dfCZ))
stopifnot(!isFile(pathnameZZ))


message("*** ChecksumFile / ChecksumFileSet ... DONE")
