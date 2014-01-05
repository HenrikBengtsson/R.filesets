library("R.filesets")

# Setting up a file
path <- system.file(package="R.filesets")
ds <- GenericDataFileSet$byPath(path)
df <- ds[[1L]]
print(df)

# Copy it to a temporary directory
path <- tempdir()
dfC <- copyTo(df, path=path)

# Try to copy it without overwriting
ok <- tryCatch({
  copyTo(df, path=path, overwrite=FALSE)
  FALSE
}, error = function(ex) { TRUE })
stopifnot(ok)

# Copy it again by overwriting exiting output
copyTo(df, path=path, overwrite=TRUE)

# Cleanup
file.remove(getPathname(dfC))
