library("R.filesets")
isPackageInstalled <- R.utils::isPackageInstalled
fullTest <- (Sys.getenv("_R_CHECK_FULL_") != "")

message("*** dsApply() on GenericDataFile")

# Example files
path <- system.file("exData", "dataSetA,original", package="R.filesets")
print(path)

# - - - - - - - - - - - - - - - - - - - - - - - -
# Setting up a file set
# - - - - - - - - - - - - - - - - - - - - - - - -
ds <- GenericDataFileSet$byPath(path)
print(ds)

# - - - - - - - - - - - - - - - - - - - - - - - -
# Get the size of each file
# - - - - - - - - - - - - - - - - - - - - - - - -
# Alt 1.
res1 <- lapply(ds, FUN=getFileSize)
str(res1)

# Alt 2. (via an internal loop)
res2 <- dsApply(ds, FUN=getFileSize, .parallel="none")
str(res2)
## FIXME: dsApply() returns "short" names whereas lapply() "full" names
stopifnot(all.equal(res2, res1, check.attributes=FALSE))

# Alt 3. (via BatchJobs)
if (fullTest && isPackageInstalled("BatchJobs")) {
  res3 <- dsApply(ds, FUN=getFileSize, .parallel="BatchJobs")
  print(res3)
  stopifnot(all.equal(res3, res1, check.attributes=FALSE))
}

# Alt 4. (via BiocParallel + BatchJobs)
if (fullTest && isPackageInstalled("BiocParallel") && isPackageInstalled("BatchJobs")) {
  res4 <- dsApply(ds, FUN=getFileSize, .parallel="BiocParallel::BatchJobs")
  print(res4)
  stopifnot(all.equal(res4, res1, check.attributes=FALSE))
}
