library("R.filesets")

message("*** sortBy() on GenericDataFile")

# Example files
path <- system.file("exData", "dataSetA,original", package="R.filesets")
print(path)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Setting up a file set
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ds <- GenericDataFileSet$byPath(path)
print(ds)

bys <- c("lexicographic", "filesize")
if (require("gtools")) by <- c(by, "mixedsort")
for (by in bys) {
  for (decreasing in c(FALSE, TRUE)) {
    dsS  <- sortBy(ds, by=by, decreasing=FALSE)
    print(as.list(dsS))
  }
}

