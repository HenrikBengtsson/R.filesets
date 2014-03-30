library("R.filesets")

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

# Alt 2. (via an internal loop)
res2 <- dsApply(ds, FUN=getFileSize, .parallel="none")
print(res2)
stopifnot(identical(res2, res1))

# - - - - - - - - - - - - - - - - - - - - - - - -
# Advanced alternatives
# - - - - - - - - - - - - - - - - - - - - - - - -
fullTest <- (Sys.getenv("_R_CHECK_FULL_") != "")
if (fullTest) {
isPackageInstalled <- R.utils::isPackageInstalled

# Alt 3. (via BatchJobs)
if (isPackageInstalled("BatchJobs")) {
  res3 <- dsApply(ds, FUN=getFileSize, .parallel="BatchJobs")
  print(res3)
  stopifnot(identical(res3, res1))
}

# Alt 4. (via BiocParallel + BatchJobs)
if (isPackageInstalled("BiocParallel") && isPackageInstalled("BatchJobs")) {
  res4 <- dsApply(ds, FUN=getFileSize, .parallel="BiocParallel::BatchJobs")
  print(res4)
  stopifnot(identical(res4, res1))
}

} # if (fullTest)
