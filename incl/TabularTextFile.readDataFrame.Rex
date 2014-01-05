path <- system.file("exData/dataSetA,original", package="R.filesets")

db <- TabularTextFile("fileA,20100112.dat", path=path)
print(db)

# Read all data
data <- readDataFrame(db)
print(data)

# Read columns 'x', 'y', and 'char'
data <- readDataFrame(db, colClasses=c("(x|y)"="integer", "char"="character"))
print(data)

# Translate column names on the fly
db <- setColumnNamesTranslator(db, function(names, ...) toupper(names))
data2 <- readDataFrame(db, colClasses=c("(X|Y)"="integer", "CHAR"="character"))
print(data2)
