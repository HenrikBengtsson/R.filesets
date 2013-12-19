%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  TabularTextFile.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{TabularTextFile}
\docType{class}
\alias{TabularTextFile}


\title{The TabularTextFile class}

\description{
 Package:  R.filesets \cr
\bold{Class TabularTextFile}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link[R.filesets]{FullNameInterface}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{\link[R.filesets]{GenericDataFile}}\cr
\code{~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~+--}\code{\link[R.filesets]{ColumnNamesInterface}}\cr
\code{~~~~~~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~~~~~~+--}\code{\link[R.filesets]{GenericTabularFile}}\cr
\code{~~~~~~~~~~~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~~~~~~~~~~~+--}\emph{\code{TabularTextFile}}\cr

\bold{Directly known subclasses:}\cr
\cr

public abstract static class \bold{TabularTextFile}\cr
extends \emph{\link[R.filesets]{GenericTabularFile}}\cr



 A TabularTextFile is an object refering to a tabular text file
 on a file system containing data in a tabular format.
 Methods for reading all or a subset of the tabular data exist.
}

\usage{
TabularTextFile(..., sep=c("\t", ","), quote="\"", fill=FALSE, skip=0L, columnNames=NA,
  commentChar="#", .verify=TRUE, verbose=FALSE)
}

\arguments{
  \item{...}{Arguments passed to \code{\link{GenericTabularFile}}.}
  \item{sep}{A \code{\link[base]{character}} specifying the symbol used to separate the
    cell entries.  If more than one symbol is specified, it will try to
    select the correct one by peeking into the file.}
  \item{quote}{A \code{\link[base]{character}} specifying the quote symbol used, if any.}
  \item{fill}{As in \code{\link[utils]{read.table}}.}
  \item{skip}{As in \code{\link[utils]{read.table}}.}
  \item{columnNames}{A \code{\link[base]{logical}} or a \code{\link[base]{character}} \code{\link[base]{vector}}. If \code{\link[base:logical]{TRUE}},
     then column names are inferred from the file.  If a \code{\link[base]{character}}
     \code{\link[base]{vector}}, then the column names are given by this argument.}
  \item{commentChar}{A single \code{\link[base]{character}} specifying which symbol
     should be used for comments, cf. \code{\link[utils]{read.table}}.}
  \item{.verify, verbose}{(Internal only) If \code{\link[base:logical]{TRUE}}, the file is
     verified while the object is instantiated by the constructor.
     The verbose argument is passed to the verifier function.}
}

\section{Fields and Methods}{
 \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{\link[R.filesets:getHeader.TabularTextFile]{getHeader}} \tab Gets the file header.\cr
 \tab \code{\link[R.filesets:nbrOfLines.TabularTextFile]{nbrOfLines}} \tab Counts the number of lines in the data file.\cr
 \tab \code{\link[R.filesets:nbrOfRows.TabularTextFile]{nbrOfRows}} \tab Counts the number of data rows.\cr
 \tab \code{\link[R.filesets:readDataFrame.TabularTextFile]{readDataFrame}} \tab Reads the tabular data as a data frame.\cr
 \tab \code{\link[R.filesets:readLines.TabularTextFile]{readLines}} \tab Reads the lines of the data file as strings.\cr
}


 \bold{Methods inherited from GenericTabularFile}:\cr
[, as.character, dim, extractMatrix, head, nbrOfColumns, nbrOfRows, readColumns, readDataFrame, tail, writeColumnsToFiles

 \bold{Methods inherited from ColumnNamesInterface}:\cr
appendColumnNamesTranslator, appendColumnNamesTranslatorByNULL, appendColumnNamesTranslatorBycharacter, appendColumnNamesTranslatorByfunction, appendColumnNamesTranslatorBylist, clearColumnNamesTranslator, clearListOfColumnNamesTranslators, getColumnNames, getColumnNamesTranslator, getDefaultColumnNames, getListOfColumnNamesTranslators, nbrOfColumns, setColumnNames, setColumnNamesTranslator, setListOfColumnNamesTranslators, updateColumnNames

 \bold{Methods inherited from GenericDataFile}:\cr
as.character, clone, compareChecksum, copyTo, equals, fromFile, getAttribute, getAttributes, getChecksum, getChecksumFile, getCreatedOn, getDefaultFullName, getExtension, getExtensionPattern, getFileSize, getFileType, getFilename, getFilenameExtension, getLastAccessedOn, getLastModifiedOn, getOutputExtension, getPath, getPathname, gunzip, gzip, hasBeenModified, isFile, isGzipped, readChecksum, renameTo, renameToUpperCaseExt, setAttribute, setAttributes, setAttributesBy, setAttributesByTags, setExtensionPattern, testAttributes, validate, validateChecksum, writeChecksum

 \bold{Methods inherited from FullNameInterface}:\cr
appendFullNameTranslator, appendFullNameTranslatorByNULL, appendFullNameTranslatorByTabularTextFile, appendFullNameTranslatorByTabularTextFileSet, appendFullNameTranslatorBycharacter, appendFullNameTranslatorBydata.frame, appendFullNameTranslatorByfunction, appendFullNameTranslatorBylist, clearFullNameTranslator, clearListOfFullNameTranslators, getDefaultFullName, getFullName, getFullNameTranslator, getListOfFullNameTranslators, getName, getTags, hasTag, hasTags, resetFullName, setFullName, setFullNameTranslator, setListOfFullNameTranslators, setName, setTags, updateFullName

 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clearLookupCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFieldModifier, getFieldModifiers, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, save


}

\examples{
path <- system.file("exData/dataSetA,original", package="R.filesets")

db <- TabularTextFile("fileA,20100112.dat", path=path)
print(db)

# Read all data
df <- readDataFrame(db)
print(df)

# Read columns 'x', 'y', and 'char'
df <- readDataFrame(db, colClasses=c("(x|y)"="integer", "char"="character"))
print(df)
}

\author{Henrik Bengtsson}

\seealso{
  An object of this class is typically part of an \code{\link{TabularTextFileSet}}.
}
\keyword{classes}
