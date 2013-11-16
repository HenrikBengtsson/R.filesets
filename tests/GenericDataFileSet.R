library("R.filesets")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Setting up a file set
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
path <- system.file(package="R.filesets")
ds <- GenericDataFileSet$byPath(path)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data set
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cat("Path of data set:\n")
print(getPath(ds))

cat("Fullname of data set:\n")
print(getFullName(ds))

cat("Checksum of data set:\n")
print(getChecksum(ds))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data files
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cat("Pathnames:\n")
pathnames <- getPathnames(ds)
print(pathnames)

cat("Filenames:\n")
filenames <- sapply(ds, FUN=getFilename)
print(filenames)
stopifnot(all.equal(unname(filenames), basename(pathnames)))

cat("Extensions:\n")
exts <- sapply(ds, FUN=getExtension)
print(exts)

cat("Checksums:\n")
uids <- sapply(ds, FUN=getChecksum)
print(uids)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Subsetting
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
df0 <- getFile(ds, 2L)
df1 <- ds[[2L]]
stopifnot(identical(getPathname(df0), getPathname(df1)))

n <- length(ds)
ds2 <- extract(ds, 1:n)
print(ds2)

ds3 <- extract(ds, n:1)
print(ds3)

stopifnot(identical(rev(getPathnames(ds3)), getPathnames(ds2)))

ds4 <- ds[1:n]
print(ds4)
stopifnot(equals(ds4, ds2))


idxs <- c(1,2,NA,n,NA)
ds5 <- extract(ds, idxs, onMissing="NA")
print(ds5)
print(getFullNames(ds5))
print(getFiles(ds5))

stopifnot(identical(is.na(idxs), unname(is.na(getPathnames(ds5)))))

ds6 <- ds[idxs, onMissing="NA"]
print(ds6)
stopifnot(equals(ds6, ds5))

ds7 <- ds[c(1,2,NA_integer_), onMissing="dropall"]
stopifnot(length(ds7) == 0L)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Special cases
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
dsEmpty <- newInstance(ds)
stopifnot(length(dsEmpty) == 0L)

dsEmpty <- ds[c()]
stopifnot(length(dsEmpty) == 0L)

dsExpanded <- dsEmpty[rep(NA_integer_, times=5L)]
stopifnot(length(dsExpanded) == 5L)
