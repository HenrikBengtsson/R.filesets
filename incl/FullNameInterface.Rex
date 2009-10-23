# Setup a file set
path <- system.file(package="R.filesets")
ds <- GenericDataFileSet$byPath(path)

# List the fullnames of all files
cat("Default fullnames:\n")
print(getFullNames(ds))

# Translate fullnames to lower case
setFullNamesTranslator(ds, function(names, ...) tolower(names))
cat("Lower-case fullnames:\n")
print(getFullNames(ds))

# Append a translator that reverse the order of the letters
revStr <- function(names, ...) {
  names <- strsplit(names, split="", fixed=TRUE)
  names <- lapply(names, FUN=rev)
  names <- sapply(names, FUN=paste, collapse="")
  names
}
appendFullNamesTranslator(ds, revStr)
cat("Reversed lower-case fullnames:\n")
fn3 <- getFullNames(ds)
print(fn3)


# Alternative for setting up a sequence of translators
setFullNamesTranslator(ds, list(function(names, ...) tolower(names), revStr))
cat("Reversed lower-case fullnames:\n")
fn3b <- getFullNames(ds)
print(fn3b)
stopifnot(identical(fn3b, fn3))

# Reset
clearFullNamesTranslator(ds)
cat("Default fullnames (after resetting):\n")
print(getFullNames(ds))
