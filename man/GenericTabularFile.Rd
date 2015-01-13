%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericTabularFile.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{GenericTabularFile}
\docType{class}
\alias{GenericTabularFile}


\title{The abstract GenericTabularFile class}

\description{
 Package:  R.filesets \cr
\bold{Class GenericTabularFile}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link[R.filesets]{FullNameInterface}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{\link[R.filesets]{GenericDataFile}}\cr
\code{~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~+--}\code{\link[R.filesets]{ColumnNamesInterface}}\cr
\code{~~~~~~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~~~~~~+--}\emph{\code{GenericTabularFile}}\cr

\bold{Directly known subclasses:}\cr
\emph{\link[R.filesets]{TabularTextFile}}\cr

public abstract static class \bold{GenericTabularFile}\cr
extends \emph{\link[R.filesets]{ColumnNamesInterface}}\cr



 A TabularTextFile is an object refering to a tabular text file
 on a file system containing data in a tabular format.
 Methods for reading all or a subset of the tabular data exist.
}

\usage{
GenericTabularFile(..., .verify=TRUE, verbose=FALSE)
}

\arguments{
  \item{...}{Arguments passed to \code{\link{GenericDataFile}}.}
  \item{.verify, verbose}{(Internal only) If \code{\link[base:logical]{TRUE}}, the file is
     verified while the object is instantiated by the constructor.
     The verbose argument is passed to the verifier function.}
}

\section{Fields and Methods}{
 \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{dim} \tab  -\cr
 \tab \code{extractMatrix} \tab  -\cr
 \tab \code{head} \tab  -\cr
 \tab \code{nbrOfColumns} \tab  -\cr
 \tab \code{nbrOfRows} \tab  -\cr
 \tab \code{readColumns} \tab  -\cr
 \tab \code{readDataFrame} \tab  -\cr
 \tab \code{tail} \tab  -\cr
 \tab \code{writeColumnsToFiles} \tab  -\cr
}


 \bold{Methods inherited from ColumnNamesInterface}:\cr
appendColumnNamesTranslator, appendColumnNamesTranslatorByNULL, appendColumnNamesTranslatorBycharacter, appendColumnNamesTranslatorByfunction, appendColumnNamesTranslatorBylist, clearColumnNamesTranslator, clearListOfColumnNamesTranslators, getColumnNames, getColumnNamesTranslator, getDefaultColumnNames, getListOfColumnNamesTranslators, nbrOfColumns, setColumnNames, setColumnNamesTranslator, setListOfColumnNamesTranslators, updateColumnNames

 \bold{Methods inherited from GenericDataFile}:\cr
as.character, clone, compareChecksum, copyTo, equals, fromFile, getAttribute, getAttributes, getChecksum, getChecksumFile, getCreatedOn, getDefaultFullName, getExtension, getExtensionPattern, getFileSize, getFileType, getFilename, getFilenameExtension, getLastAccessedOn, getLastModifiedOn, getOutputExtension, getPath, getPathname, gunzip, gzip, hasBeenModified, is.na, isFile, isGzipped, linkTo, readChecksum, renameTo, renameToUpperCaseExt, setAttribute, setAttributes, setAttributesBy, setAttributesByTags, setExtensionPattern, testAttributes, validate, validateChecksum, writeChecksum

 \bold{Methods inherited from FullNameInterface}:\cr
appendFullNameTranslator, appendFullNameTranslatorByNULL, appendFullNameTranslatorByTabularTextFile, appendFullNameTranslatorByTabularTextFileSet, appendFullNameTranslatorBycharacter, appendFullNameTranslatorBydata.frame, appendFullNameTranslatorByfunction, appendFullNameTranslatorBylist, clearFullNameTranslator, clearListOfFullNameTranslators, getDefaultFullName, getFullName, getFullNameTranslator, getListOfFullNameTranslators, getName, getTags, hasTag, hasTags, resetFullName, setFullName, setFullNameTranslator, setListOfFullNameTranslators, setName, setTags, updateFullName

 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clearLookupCache, clone, detach, equals, extend, finalize, getEnvironment, getFieldModifier, getFieldModifiers, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, save


}

\author{Henrik Bengtsson}

\seealso{
  An object of this class is typically part of an
  \code{\link{GenericTabularFileSet}}.
}
\keyword{classes}
