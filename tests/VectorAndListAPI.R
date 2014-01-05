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



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# List operations on data set
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Double-bracket subsetting
dfA <- ds[[1]]
dfB <- ds[[2]]

# equals()
stopifnot(equals(dfA, dfA))
stopifnot(equals(dfB, dfB))
stopifnot(!equals(dfA, dfB))

# Comparing: ==, !=
# FIXME

# Ordering: <=, >=, <, >, order(), sort(), rank()
# FIXME


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

# Comparing: duplicated()
dups <- duplicated(dsA)
print(dups)
hasDups <- anyDuplicated(dsA)
print(hasDups)
stopifnot(identical(any(dups), hasDups))

dsT <- dsA[!dups]
stopifnot(!anyDuplicated(dsT))
stopifnot(equals(dsT, ds))

# Comparing: unique()
dsU <- unique(dsA)
stopifnot(equals(dsU, dsT))
