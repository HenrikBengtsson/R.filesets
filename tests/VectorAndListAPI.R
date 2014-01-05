library("R.filesets")

# Tests adapted from the IRanges package and http://www.bioconductor.org/help/course-materials/2012/SeattleFeb2012/GenomicRanges_slides.pdf

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Setting up a file set
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
path <- system.file(package="R.filesets")
ds <- GenericDataFileSet$byPath(path)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Vector operations on data set
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Single-bracket subsetting
dsHead <- ds[1:2]
dsTail <- ds[-(1:2)]

# Combining: c()
ds2 <- c(dsHead, dsTail)
stopifnot(all.equal(ds2, ds))

# Comparing: ==, !=, duplicated(), unique()
# FIXME

# Ordering: <=, >=, <, >, order(), sort(), rank()
# FIXME


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# List operations on data set
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Double-bracket subsetting
dfA <- ds[[1]]
dfB <- ds[[2]]


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Vector and list operations on data set
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
dsA <- extract(ds, c(seq_along(ds), 1, 3))

dsB <- c(ds, ds[[1]], ds[[3]])
stopifnot(all.equal(dsB, dsA))

dsC <- c(ds, list(ds[[1]], ds[[3]]))
stopifnot(all.equal(dsC, dsA))

dsD <- c(ds, ds[c(1,3)])
stopifnot(all.equal(dsD, dsA))
