library("R.filesets")

message("*** GenericDataFile")

# Example files
path <- system.file("exData", "dataSetA,original", package="R.filesets")
print(path)

# Setting up a file set
ds <- GenericDataFileSet$byPath(path)
print(ds)

df <- ds[[1L]]
print(df)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# copyTo()
# - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("*** copyTo() on GenericDataFile")

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
message("*** linkTo() on GenericDataFile")

# Link to it in a temporary directory
path <- tempdir()

# On Windows, necessary privileges are required.  If not
# available, generate a warning and not an error.
if (.Platform$OS.type == "windows") {
  dfL <- NULL
  tryCatch({
    dfL <- linkTo(df, path=path)
  }, error = function(ex) {
    print(ex)
    cat("The above exception was caught but ignored for this package system test\n")
  })

  if (!is.null(dfL)) {
    print(dfL)

    # Sanity checks
    stopifnot(getChecksum(dfL) == getChecksum(df))

    # Copy file (via link) - unless a Windows Shortcut link
    isWindowsShortcut <- (getPathname(dfL) == getPathname(df))
    if (!isWindowsShortcut) {
      dfLC <- copyTo(dfL, path=file.path(path, "foo"), validate=FALSE)
      # Sanity checks
      stopifnot(getChecksum(dfLC) == getChecksum(df))
      stopifnot(getPathname(dfLC) != getPathname(df))
      # Cleanup
      file.remove(getPathname(dfLC))
      # Sanity checks
      stopifnot(!isFile(dfLC))
      stopifnot(isFile(dfL))
      stopifnot(isFile(df))
      # Cleanup
      file.remove(getPathname(dfL))
    } else {
      # Done with the Windows Shortcut link
      dfL <- NULL
    }
  }
} else {
  dfL <- linkTo(df, path=path)
  print(dfL)

  # Sanity checks
  stopifnot(getChecksum(dfL) == getChecksum(df))
  stopifnot(getPathname(dfL) != getPathname(df))

  # Copy file (via link)
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

  # Cleanup
  file.remove(getPathname(dfL))
} # if (.Platform$OS.type == "windows")

# Sanity checks
stopifnot(!isFile(dfL))
stopifnot(isFile(df))



