library("R.filesets")

message("*** GenericDataFileSet")

# Example files
path <- system.file("exData", "dataSetA,original", package="R.filesets")
print(path)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Setting up a file set
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ds <- GenericDataFileSet$byPath(path)
print(ds)

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

ds8 <- rep(ds, each=2L)
stopifnot(length(ds8) == 2*length(ds))
stopifnot(equals(ds8, ds[rep(seq_along(ds), each=2L)]))

ds9 <- rep(ds8, times=3L)
stopifnot(length(ds9) == 3*length(ds8))
stopifnot(equals(ds9, ds8[rep(seq_along(ds8), times=3L)]))

ds10 <- rep(ds9, length.out=1/2*length(ds9))
stopifnot(length(ds10) == 1/2*length(ds9))
stopifnot(equals(ds10, ds9[rep(seq_along(ds9), length.out=1/2*length(ds9))]))

ds11 <- rep(ds10, length.out=2*length(ds10))
stopifnot(length(ds11) == 2*length(ds10))
stopifnot(equals(ds11, ds10[rep(seq_along(ds10), length.out=2*length(ds10))]))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Special cases
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
dsEmpty <- R.oo::newInstance(ds)
stopifnot(length(dsEmpty) == 0L)

dsEmpty <- ds[c()]
stopifnot(length(dsEmpty) == 0L)

dsExpanded <- dsEmpty[rep(NA_integer_, times=5L)]
stopifnot(length(dsExpanded) == 5L)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Constructing data sets
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
files <- as.list(ds)
ds2 <- GenericDataFileSet(files)
print(ds2)
stopifnot(equals(ds2, ds))

ds3 <- GenericDataFileSet(ds)
print(ds3)
stopifnot(equals(ds3, ds))

ds4 <- GenericDataFileSet(c(ds,ds))
print(ds4)
stopifnot(equals(ds4, c(ds,ds)))



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Dataset A
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
path <- system.file("exData/dataSetA,original", package="R.filesets")
ds <- GenericDataFileSet$byPath(path)
print(ds)
names <- getNames(ds)
print(names)


# Exact matching
by <- "exact";
cat(sprintf("By: %s\n", paste(by, collapse=", ")))
for (name in names) {
  idxs <- indexOf(ds, name, by=by)
  cat(sprintf(" %s @ %s\n", name, paste(idxs, collapse=", ")))
  stopifnot(all(idxs == which(name == names)))
}

# Fixed regular expression matching
by <- "fixed";
cat(sprintf("By: %s\n", paste(by, collapse=", ")))
for (name in names) {
  idxs <- indexOf(ds, name, by=by)
  cat(sprintf(" %s @ %s\n", name, paste(idxs, collapse=", ")))
  stopifnot(all(idxs == grep(name, names)))
}

# Regular expression matching
by <- "regexp";
cat(sprintf("By: %s\n", paste(by, collapse=", ")))
for (name in names) {
  idxs <- indexOf(ds, name, by=by)
  pattern <- sprintf("^%s$", name)
  cat(sprintf(" %s @ %s\n", name, paste(idxs, collapse=", ")))
  stopifnot(all(idxs == grep(pattern, names)))
}

# First regular expression matching, then fixed
by <- c("regexp", "fixed");
cat(sprintf("By: %s\n", paste(by, collapse=", ")))
for (name in names) {
  idxs <- indexOf(ds, name, by=by)
  cat(sprintf(" %s @ %s\n", name, paste(idxs, collapse=", ")))
}

# First exact, then regular expression matching, then fixed
by <- c("exact", "regexp", "fixed");
cat(sprintf("By: %s\n", paste(by, collapse=", ")))
for (name in names) {
  idxs <- indexOf(ds, name, by=by)
  cat(sprintf(" %s @ %s\n", name, paste(idxs, collapse=", ")))
}

# The default, which operates as previous step
cat("By: <default>\n")
for (name in names) {
  idxs <- indexOf(ds, name)
  cat(sprintf(" %s @ %s\n", name, paste(idxs, collapse=", ")))
}

