%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  TabularTextFileSet.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{TabularTextFileSet}
\docType{class}
\alias{TabularTextFileSet}


 \title{The TabularTextFileSet class}

 \description{
  Package:  R.filesets \cr
\bold{Class TabularTextFileSet}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link[R.filesets]{FullNameInterface}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{\link[R.filesets]{GenericDataFileSet}}\cr
\code{~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~+--}\code{\link[R.filesets]{GenericTabularFileSet}}\cr
\code{~~~~~~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~~~~~~+--}\code{TabularTextFileSet}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{TabularTextFileSet}\cr
extends \link[R.filesets]{GenericTabularFileSet}\cr



  An TabularTextFileSet object represents a set of \code{\link{TabularTextFile}}s.
 }

 \usage{
TabularTextFileSet(...)
}

 \arguments{
   \item{...}{Arguments passed to \code{\link{GenericTabularFileSet}}.}
 }

 \section{Fields and Methods}{
  \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{\link[R.filesets:readDataFrame.TabularTextFileSet]{readDataFrame}} \tab Reads the tabular data from all files as data frames.\cr
}


 \bold{Methods inherited from GenericTabularFileSet}:\cr
extractMatrix

 \bold{Methods inherited from GenericDataFileSet}:\cr
append, appendFiles, appendFullNamesTranslator, appendFullNamesTranslatorBydata.frame, appendFullNamesTranslatorByfunction, appendFullNamesTranslatorBylist, appendFullNamesTranslatorByNULL, appendFullNamesTranslatorByTabularTextFile, appendFullNamesTranslatorByTabularTextFileSet, as.character, as.list, byName, byPath, clearCache, clearFullNamesTranslator, clone, copyTo, equals, extract, findByName, getChecksum, getChecksumObjects, getDefaultFullName, getFile, getFileClass, getFiles, getFileSize, getFullNames, getNames, getOneFile, getPath, getPathnames, getSubdirs, hasFile, indexOf, nbrOfFiles, resetFullNames, seq, setFullNamesTranslator, sortBy, update2, updateFullName, updateFullNames, validate

 \bold{Methods inherited from FullNameInterface}:\cr
appendFullNameTranslator, appendFullNameTranslatorBycharacter, appendFullNameTranslatorBydata.frame, appendFullNameTranslatorByfunction, appendFullNameTranslatorBylist, appendFullNameTranslatorByNULL, appendFullNameTranslatorByTabularTextFile, appendFullNameTranslatorByTabularTextFileSet, clearFullNameTranslator, clearListOfFullNameTranslators, getDefaultFullName, getFullName, getFullNameTranslator, getListOfFullNameTranslators, getName, getTags, hasTag, hasTags, resetFullName, setFullName, setFullNameTranslator, setListOfFullNameTranslators, setName, setTags, updateFullName

 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clearLookupCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFieldModifier, getFieldModifiers, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save


 }

 \examples{
# Setup a file set consisting of all *.dat tab-delimited files
# in a particular directory
path <- system.file("exData/dataSetA,original", package="R.filesets")
ds <- TabularTextFileSet$byPath(path, pattern="[.]dat$")
print(ds)


# Read column 'y' and a subset of the rows from each of the
# tab-delimited files and combine into a matrix
rows <- c(3:5, 8, 2)
data <- extractMatrix(ds, column="y", colClass="integer", rows=rows)
print(data)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# See also help("readDataFrame.TabularTextFileSet")
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# ADVANCED: Translation of fullnames
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
fnts <- TabularTextFileSet$byPath(getPath(ds), pattern=",fullnames[.]txt$")
appendFullNamesTranslator(ds, as.list(fnts))

cat("Default fullnames:\n")
print(head(getFullNames(ds, translate=FALSE)))
cat("Translated fullnames:\n")
print(head(getFullNames(ds)))

cat("Default fullnames:\n")
print(getFullNames(ds, translate=FALSE))
cat("Translated fullnames:\n")
print(getFullNames(ds))
}

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}
\keyword{classes}
