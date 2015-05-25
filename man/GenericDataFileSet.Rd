%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFileSet.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{GenericDataFileSet}
\docType{class}
\alias{GenericDataFileSet}


\title{The GenericDataFileSet class}

\description{
 Package:  R.filesets \cr
\bold{Class GenericDataFileSet}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link[R.filesets]{FullNameInterface}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{GenericDataFileSet}\cr

\bold{Directly known subclasses:}\cr
\link[R.filesets]{ChecksumFileSet}, \link[R.filesets]{GenericTabularFileSet}, \link[R.filesets]{RDataFileSet}, \link[R.filesets]{RdsFileSet}, \link[R.filesets]{TabularTextFileSet}\cr

public static class \bold{GenericDataFileSet}\cr
extends \emph{\link[R.filesets]{FullNameInterface}}\cr



 A GenericDataFileSet object represents a set of \code{\link{GenericDataFile}}s.
}

\usage{
GenericDataFileSet(files=NULL, tags="*", depth=NULL, ...,
  .onUnknownArgs=c("error", "warning", "ignore"))
}

\arguments{
  \item{files}{A \code{\link[base]{list}} of \code{\link{GenericDataFile}}:s or
     a \code{\link{GenericDataFileSet}}.}
  \item{tags}{A \code{\link[base]{character}} \code{\link[base]{vector}} of tags to be used for this file set.
     The string \code{"*"} indicates that it should be replaced by the
     tags part of the file set pathname.}
  \item{depth}{An non-negative \code{\link[base]{integer}}.}
  \item{...}{Not used.}
  \item{.onUnknownArgs}{A \code{\link[base]{character}} string specifying what should occur
     if there are unknown arguments in \code{...}.}
}

\section{Fields and Methods}{
 \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{anyDuplicated} \tab  -\cr
 \tab \code{anyNA} \tab  -\cr
 \tab \code{append} \tab  -\cr
 \tab \code{appendFiles} \tab  -\cr
 \tab \code{as.list} \tab  -\cr
 \tab \code{byName} \tab  -\cr
 \tab \code{byPath} \tab  -\cr
 \tab \code{duplicated} \tab  -\cr
 \tab \code{equals} \tab  -\cr
 \tab \code{extract} \tab  -\cr
 \tab \code{getChecksum} \tab  -\cr
 \tab \code{getChecksumFileSet} \tab  -\cr
 \tab \code{getDefaultFullName} \tab  -\cr
 \tab \code{getFile} \tab  -\cr
 \tab \code{getFileClass} \tab  -\cr
 \tab \code{getFileSize} \tab  -\cr
 \tab \code{getFullNames} \tab  -\cr
 \tab \code{getNames} \tab  -\cr
 \tab \code{getOneFile} \tab  -\cr
 \tab \code{getPath} \tab  -\cr
 \tab \code{getPathnames} \tab  -\cr
 \tab \code{gunzip} \tab  -\cr
 \tab \code{gzip} \tab  -\cr
 \tab \code{hasFile} \tab  -\cr
 \tab \code{indexOf} \tab  -\cr
 \tab \code{is.na} \tab  -\cr
 \tab \code{na.omit} \tab  -\cr
 \tab \code{sortBy} \tab  -\cr
 \tab \code{unique} \tab  -\cr
 \tab \code{validate} \tab  -\cr
}


 \bold{Methods inherited from FullNameInterface}:\cr
appendFullNameTranslator, appendFullNameTranslatorByNULL, appendFullNameTranslatorByTabularTextFile, appendFullNameTranslatorByTabularTextFileSet, appendFullNameTranslatorBycharacter, appendFullNameTranslatorBydata.frame, appendFullNameTranslatorByfunction, appendFullNameTranslatorBylist, clearFullNameTranslator, clearListOfFullNameTranslators, getDefaultFullName, getFullName, getFullNameTranslator, getListOfFullNameTranslators, getName, getTags, hasTag, hasTags, resetFullName, setFullName, setFullNameTranslator, setListOfFullNameTranslators, setName, setTags, updateFullName

 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clearLookupCache, clone, detach, equals, extend, finalize, getEnvironment, getFieldModifier, getFieldModifiers, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, save


}

\examples{
# Setup a file set
path <- system.file(package="R.filesets")
ds <- GenericDataFileSet$byPath(path)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data set
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cat("Path of data set:\n")
print(getPath(ds))

cat("Fullname of data set:\n")
print(getFullName(ds))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Data files
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
cat("Pathnames:\n")
print(getPathnames(ds))

cat("Filenames:\n")
print(sapply(ds, getFilename))

cat("Extensions:\n")
print(sapply(ds, getExtension))


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Subsetting
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
n <- length(ds)
ds2 <- extract(ds, 1:n)
print(ds2)

ds3 <- extract(ds, n:1)
print(ds3)

stopifnot(identical(rev(getPathnames(ds3)), getPathnames(ds2)))

idxs <- c(1,2,NA,n,NA)
ds4 <- extract(ds, idxs, onMissing="NA")
print(ds4)
print(getFullNames(ds4))
print(getFiles(ds4))

stopifnot(identical(is.na(idxs), unname(is.na(getPathnames(ds4)))))
}

\author{Henrik Bengtsson}
\keyword{classes}
