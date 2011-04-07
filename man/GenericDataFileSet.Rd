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
\link[R.filesets]{GenericTabularFileSet}, \link[R.filesets]{TabularTextFileSet}\cr

public static class \bold{GenericDataFileSet}\cr
extends \emph{\link[R.filesets]{FullNameInterface}}\cr



  A GenericDataFileSet object represents a set of \code{\link{GenericDataFile}}s.
 }

 \usage{GenericDataFileSet(files=NULL, tags="*", alias=NULL, ..., .onUnknownArgs=c("error", "warning", "ignore"))}

 \arguments{
   \item{files}{A \code{\link[base]{list}} of \code{\link{GenericDataFile}}:s.}
   \item{tags}{A \code{\link[base]{character}} \code{\link[base]{vector}} of tags to be used for this file set.
      The string \code{"*"} indicates that it should be replaced by the
      tags part of the file set pathname.}
   \item{alias}{A \code{\link[base]{character}} string specifying a name alias overriding the
      name inferred from the pathname.}
   \item{...}{Not used.}
   \item{.onUnknownArgs}{A \code{\link[base]{character}} string specifying what should occur
      if there are unknown arguments in \code{...}.}
 }

 \section{Fields and Methods}{
  \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{\link[R.filesets:append.GenericDataFileSet]{append}} \tab Appends one data set to an existing one.\cr
 \tab \code{\link[R.filesets:appendFiles.GenericDataFileSet]{appendFiles}} \tab Appends a list of files to a data set.\cr
 \tab \code{\link[R.filesets:as.list.GenericDataFileSet]{as.list}} \tab Returns the files of the file set.\cr
 \tab \code{\link[R.filesets:byName.GenericDataFileSet]{byName}} \tab Locates and sets up a file set by its name.\cr
 \tab \code{\link[R.filesets:byPath.GenericDataFileSet]{byPath}} \tab Defines a GenericDataFileSet by searching for files in a directory.\cr
 \tab \code{clearCache} \tab  -\cr
 \tab \code{equals} \tab  -\cr
 \tab \code{\link[R.filesets:extract.GenericDataFileSet]{extract}} \tab Extract a subset of the file set.\cr
 \tab \code{\link[R.filesets:getAlias.GenericDataFileSet]{getAlias}} \tab Gets the alias of the file set.\cr
 \tab \code{getChecksum} \tab  -\cr
 \tab \code{\link[R.filesets:getDefaultFullName.GenericDataFileSet]{getDefaultFullName}} \tab Gets the default full name of the file set.\cr
 \tab \code{\link[R.filesets:getFile.GenericDataFileSet]{getFile}} \tab Get a particular file of the file set.\cr
 \tab \code{getFileClass} \tab  -\cr
 \tab \code{getFileSize} \tab  -\cr
 \tab \code{getFullNames} \tab  -\cr
 \tab \code{\link[R.filesets:getNames.GenericDataFileSet]{getNames}} \tab Gets the names (or fullnames) of the files in the file set.\cr
 \tab \code{\link[R.filesets:getPath.GenericDataFileSet]{getPath}} \tab Gets the path (directory) of the file set.\cr
 \tab \code{\link[R.filesets:getPathnames.GenericDataFileSet]{getPathnames}} \tab Gets the pathnames of the files in the file set.\cr
 \tab \code{hasFile} \tab  -\cr
 \tab \code{\link[R.filesets:indexOf.GenericDataFileSet]{indexOf}} \tab Finds indices of a subset of files.\cr
 \tab \code{\link[R.filesets:lapply.GenericDataFileSet]{lapply}} \tab Applies a function to each of the data files.\cr
 \tab \code{nbrOfFiles} \tab  -\cr
 \tab \code{sapply} \tab  -\cr
 \tab \code{\link[R.filesets:seq.GenericDataFileSet]{seq}} \tab Gets an vector of file indices.\cr
 \tab \code{\link[R.filesets:setAlias.GenericDataFileSet]{setAlias}} \tab Sets the alias of the file set.\cr
 \tab \code{\link[R.filesets:sortBy.GenericDataFileSet]{sortBy}} \tab Sorts the set.\cr
 \tab \code{validate} \tab  -\cr
}


 \bold{Methods inherited from FullNameInterface}:\cr
appendFullNameTranslator, appendFullNameTranslatorBycharacter, appendFullNameTranslatorBydata.frame, appendFullNameTranslatorByfunction, appendFullNameTranslatorBylist, appendFullNameTranslatorByNULL, appendFullNameTranslatorByTabularTextFile, appendFullNameTranslatorByTabularTextFileSet, clearFullNameTranslator, clearListOfFullNameTranslators, getDefaultFullName, getFullName, getFullNameTranslator, getListOfFullNameTranslators, getName, getTags, hasTag, hasTags, setFullName, setFullNameTranslator, setListOfFullNameTranslators, setName, setTags, updateFullName

 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clearLookupCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFieldModifier, getFieldModifiers, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save


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

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}
\keyword{classes}
