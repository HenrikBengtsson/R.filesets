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
ft <- TabularTextFile("all,fullnames.txt", path=getPath(ds))
print(ft)
print(readDataFrame(ft))
appendFullNamesTranslator(ds, ft)

cat("Default fullnames:\n")
print(getFullNames(ds, translate=FALSE))
cat("Translated fullnames:\n")
print(getFullNames(ds))
