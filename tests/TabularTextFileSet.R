source("incl/start.R")

message("*** TabularTextFileSet ...")

# Setup a file set consisting of all *.dat tab-delimited files
# in a particular directory
pathA <- system.file("exData", "dataSetA,original", package="R.filesets")
ds <- TabularTextFileSet$byPath(pathA, pattern="[.]dat$")
print(ds)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Extract one column with a particular name (one per file)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Read column 'y' and a subset of the rows from each of the
# tab-delimited files and combine into a matrix
rows <- c(3:5, 8, 2)
data <- extractMatrix(ds, column="y", colClasses="integer", rows=rows, drop=TRUE)
print(data)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Read data frames from each of the files
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
dataList <- lapply(ds, FUN=readDataFrame)
print(dataList)

rows <- c(3:5, 8, 2)
dataList <- lapply(ds, FUN=readDataFrame, rows=rows)
print(dataList)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Read common columns and stack into one data frame
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
colNames <- Reduce(intersect, lapply(ds, getColumnNames))
cat("Common column names:\n")
print(colNames)

# Read the *common* columns "as is" (hence 'NA')
colClasses <- rep(NA, times=length(colNames))
names(colClasses) <- colNames
cat("Column class patterns:\n")
print(colClasses)

data <- readDataFrame(ds, colClasses=colClasses, verbose=TRUE)
print(data)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Translate column names on the fly
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
lapply(ds, FUN=setColumnNamesTranslator, function(names, ...) toupper(names))
data <- readDataFrame(ds, colClasses=c("(X|Y)"="integer", "CHAR"="character"))
print(data)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# ADVANCED: Translation of fullnames
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
message("- Translation of fullnames")

## Extra sanity checks to troubleshoot stall on CRAN MS Windows servers
path <- getPath(ds)
cat(sprintf("Data set path: %s\n", sQuote(path)))
stopifnot(length(path) > 0)
pattern <- ",fullnames[.]txt$"
cat(sprintf("Pattern: %s\n", sQuote(path)))
files <- dir(pattern = pattern, path = path, full.names = TRUE, all.files = TRUE)
cat(sprintf("Data set files [n = %d]:\n", length(files)))
print(files)
stopifnot(length(files) > 0)

fnts <- TabularTextFileSet$byPath(path, pattern = pattern)
print(fnts)
str(as.list(fnts))

cat("Data set before applying fullname translator:\n")
print(ds)
appendFullNamesTranslator(ds, as.list(fnts))

cat("Data set after applying fullname translator:\n")
print(ds)

cat("Default fullnames:\n")
print(head(getFullNames(ds, translate=FALSE)))
cat("Translated fullnames:\n")
print(head(getFullNames(ds)))

cat("Default fullnames:\n")
print(getFullNames(ds, translate=FALSE))
cat("Translated fullnames:\n")
print(getFullNames(ds))

message("*** TabularTextFileSet ... DONE")

source("incl/end.R")
