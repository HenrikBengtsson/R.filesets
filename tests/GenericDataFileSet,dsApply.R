source("incl/start.R")

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
message("**** lapply()")
res1 <- lapply(ds, FUN=getFileSize)
str(res1)

message("**** future_lapply()")
strategies <- future:::supportedStrategies()
strategies <- setdiff(strategies, c("lazy", "eager"))
for (strategy in strategies) {
  future::plan(strategy)
  res2a <- future::future_lapply(ds, FUN=getFileSize)
  str(res2a)
  stopifnot(all.equal(res2a, res1, check.attributes=FALSE))
}

# Alt 2. (via an internal loop)
message("**** dsApply(..., .parallel='none')")
res2 <- dsApply(ds, FUN=getFileSize, .parallel="none")
str(res2)
## FIXME: dsApply() returns "short" names whereas lapply() "full" names
stopifnot(all.equal(res2, res1, check.attributes=FALSE))
res1 <- res2 ## FIXME: Workaround trick


# Alt 3a. (via sequential futures)
message("**** dsApply(..., .parallel='future') with plan(sequential)")
future::plan("sequential")
res3a <- dsApply(ds, FUN=getFileSize, .parallel="future")
str(res3a)
stopifnot(all.equal(res3a, res1, check.attributes=FALSE))

# Alt 3c. (via multiprocess futures)
message("**** dsApply(..., .parallel='future') with plan(multiprocess)")
future::plan("multiprocess")
res3b <- dsApply(ds, FUN=getFileSize, .parallel="future")
str(res3b)
stopifnot(all.equal(res3b, res1, check.attributes=FALSE))

# Alt 4. (via BatchJobs futures)
if (fullTest && isPackageInstalled("future.BatchJobs")) {
  message("**** dsApply(..., .parallel='future') with plan(future.BatchJobs::batchjobs_local)")
  ns <- getNamespace("future.BatchJobs")
  batchjobs_local <- get("batchjobs_local", envir=ns)
  future::plan(batchjobs_local)
  res4 <- dsApply(ds, FUN=getFileSize, .parallel="future")
  str(res4)
  stopifnot(all.equal(res4, res1, check.attributes=FALSE))
}

source("incl/end.R")
