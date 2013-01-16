path <- system.file("exData/dataSetA,original", package="R.filesets")
pathname <- file.path(path, "fileA,20100112.dat")

# Read all data
df <- readDataFrame(pathname)
print(df)

# Read columns 'x', 'y', and 'char'
df <- readDataFrame(pathname, colClasses=c("(x|y)"="integer", "char"="character"))
print(df)
