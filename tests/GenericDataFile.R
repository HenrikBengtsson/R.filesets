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

# Sanity check
stopifnot(getPathname(dfC) != getPathname(df))

# Try to copy it without overwriting nor skipping
fail <- tryCatch({
  copyTo(df, path=path, overwrite=FALSE, skip=FALSE)
  FALSE
}, error = function(ex) { TRUE })
stopifnot(fail)

# Copy it again by overwriting exiting output
dfC <- copyTo(df, path=path, overwrite=TRUE)
print(dfC)
# Sanity checks
stopifnot(getChecksum(dfC) == getChecksum(df))
stopifnot(getPathname(dfC) != getPathname(df))

# Cleanup
file.remove(getPathname(dfC))

# Sanity checks
stopifnot(!isFile(dfC))
stopifnot(isFile(df))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# linkTo()
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Link to it in a temporary directory
path <- tempdir()
dfL <- linkTo(df, path=path)
print(dfL)
isWindowsShortcut <- (.Platform$OS.type == "windows") && (getPathname(dfL) == getPathname(df))

# Sanity checks
stopifnot(getChecksum(dfL) == getChecksum(df))
stopifnot(isWindowsShortcut || (getPathname(dfL) != getPathname(df)))

# Copy file (via link)
if (!isWindowsShortcut && packageVersion("R.utils") > "1.29.0") {
  dfLC <- copyTo(dfL, path=file.path(path, "foo"))
  # Sanity checks
  stopifnot(getChecksum(dfLC) == getChecksum(df))
  stopifnot(getPathname(dfLC) != getPathname(df))
  # Cleanup
  file.remove(getPathname(dfLC))
  # Sanity checks
  stopifnot(!isFile(dfLC))
  stopifnot(isFile(dfL))
  stopifnot(isFile(df))
}

# Cleanup
if (!isWindowsShortcut) file.remove(getPathname(dfL))

# Sanity checks
stopifnot(isWindowsShortcut || !isFile(dfL))
stopifnot(isFile(df))


