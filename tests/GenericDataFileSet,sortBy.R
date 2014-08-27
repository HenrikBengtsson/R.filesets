library("R.filesets")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Setting up a file set
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
path <- system.file(package="R.filesets")
ds <- GenericDataFileSet$byPath(path)
print(ds)

for (by in c("lexicographic", "mixedsort", "filesize")) {
  for (decreasing in c(FALSE, TRUE)) {
    dsS  <- sortBy(ds, by=by, decreasing=FALSE)
    print(as.list(dsS))
  }
}

