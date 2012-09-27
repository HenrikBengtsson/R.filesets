# Setup a file set consisting of all *.dat tab-delimited files
# in a particular directory
path <- system.file("exData/dataSetA,original", package="R.filesets")
ds <- TabularTextFileSet$byPath(path, pattern="[.]dat$")
print(ds)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Read data frames from each of the files
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
dataList <- lapply(ds, readDataFrame)
print(dataList)

rows <- c(3:5, 8, 2)
dataList <- lapply(ds, readDataFrame, rows=rows)
print(dataList)



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Read common columns and stack into one data frame
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
colNames <- Reduce(intersect, lapply(ds, getColumnNames))
cat("Common column names:\n")
print(colNames)

# Read the *common* columns "as is" (hence 'NA')
colClassPatterns <- rep(NA, times=length(colNames))
names(colClassPatterns) <- colNames
cat("Column class patterns:\n")
print(colClassPatterns)

data <- readDataFrame(ds, colClassPatterns=colClassPatterns)
print(data)
