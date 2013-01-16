path <- system.file("exData/dataSetA,original", package="R.filesets")

db <- TabularTextFile("fileA,20100112.dat", path=path)
print(db)

# Read all data
df <- readDataFrame(db)
print(df)

# Read columns 'x', 'y', and 'char'
df <- readDataFrame(db, colClasses=c("(x|y)"="integer", "char"="character"))
print(df)
