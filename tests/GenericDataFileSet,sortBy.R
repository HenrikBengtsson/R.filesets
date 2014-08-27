library("R.filesets")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Setting up a file set
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
path <- system.file(package="R.filesets")
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

