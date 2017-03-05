isPackageInstalled <- R.utils::isPackageInstalled

# - - - - - - - - - - - - - - - - - - - - - - - -
# Setting up a file set
# - - - - - - - - - - - - - - - - - - - - - - - -
path <- system.file(package="R.filesets")
ds <- GenericDataFileSet$byPath(path)


# - - - - - - - - - - - - - - - - - - - - - - - -
# Get the size of each file
# - - - - - - - - - - - - - - - - - - - - - - - -
# Alt 1.
res1 <- lapply(ds, FUN=getFileSize)
print(res1)

# Alt 2. (according to current settings; see package startup message)
res2 <- dsApply(ds, FUN=getFileSize)
print(res2)
stopifnot(identical(res2, res1))

# Alt 3. (via an internal loop)
res2 <- dsApply(ds, FUN=getFileSize, .parallel="none")
print(res2)
stopifnot(identical(res2, res1))
