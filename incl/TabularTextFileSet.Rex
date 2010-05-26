# Setup a file set
path <- system.file("exData", package="R.filesets")

ds <- TabularTextFileSet$byName("dataSetA,original", pattern="[.]dat$", paths=path)
print(ds)

dfList <- lapply(ds, readDataFrame)
print(dfList)

rows <- c(3:5, 8, 2);
dfList <- lapply(ds, readDataFrame, rows=rows)
print(dfList)

data <- extractMatrix(ds, column="y", colClass="integer", rows=rows)
print(data)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Translation of fullnames
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
fnts <- TabularTextFileSet$byPath(getPath(ds), pattern=",fullnames[.]txt$")
appendFullNamesTranslator(ds, as.list(fnts))

verbose && cat(verbose, "Default fullnames:");
verbose && print(verbose, head(getFullNames(ds, translate=FALSE)));
verbose && cat(verbose, "Translated fullnames:");
verbose && print(verbose, head(getFullNames(ds)));

cat("Default fullnames:\n")
print(getFullNames(ds, translate=FALSE))
cat("Translated fullnames:\n")
print(getFullNames(ds))
