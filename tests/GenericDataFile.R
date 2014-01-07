library("R.filesets")

# Setting up a file
path <- system.file(package="R.filesets")
ds <- GenericDataFileSet$byPath(path)
df <- ds[[1L]]
print(df)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# copyTo()
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Copy it to a temporary directory
path <- tempdir()
dfC <- copyTo(df, path=path)

# Try to copy it without overwriting nor skipping
ok <- tryCatch({
  copyTo(df, path=path, overwrite=FALSE, skip=FALSE)
  FALSE
}, error = function(ex) { TRUE })
stopifnot(ok)

# Copy it again by overwriting exiting output
dfC <- copyTo(df, path=path, overwrite=TRUE)
print(dfC)
stopifnot(getChecksum(dfC) == getChecksum(df))

# Cleanup
file.remove(getPathname(dfC))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# linkTo()
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Link to it in a temporary directory
path <- tempdir()
dfL <- linkTo(df, path=path)
print(dfL)
stopifnot(getChecksum(dfL) == getChecksum(df))

# Copy file (via link)
if (packageVersion("R.utils") > "1.29.0") {
  dfLC <- copyTo(dfL, path=file.path(path, "foo"))
  stopifnot(getChecksum(dfLC) == getChecksum(df))
  # Cleanup
  file.remove(getPathname(dfLC))
}


# Cleanup
file.remove(getPathname(dfL))

