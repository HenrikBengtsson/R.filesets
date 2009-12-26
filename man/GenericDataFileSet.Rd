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



  An GenericDataFileSet object represents a set of \code{\link{GenericDataFile}}s.
 }

 \usage{GenericDataFileSet(files=NULL, tags="*", alias=NULL, ...)}

 \arguments{
   \item{files}{A \code{\link[base]{list}} of \code{\link{GenericDataFile}}:s.}
   \item{tags}{A \code{\link[base]{character}} \code{\link[base]{vector}} of tags to be used for this file set.
      The string \code{"*"} indicates that it should be replaced by the
      tags part of the file set pathname.}
   \item{alias}{A \code{\link[base]{character}} string specifying a name alias overriding the
      name inferred from the pathname.}
   \item{...}{Not used.}
 }

 \section{Fields and Methods}{
  \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{append} \tab  -\cr
 \tab \code{appendFiles} \tab  -\cr
 \tab \code{\link[R.filesets:as.list.GenericDataFileSet]{as.list}} \tab Returns the files of the file set.\cr
 \tab \code{byName} \tab  -\cr
 \tab \code{\link[R.filesets:byPath.GenericDataFileSet]{byPath}} \tab Defines a GenericDataFileSet by searching for files in a directory.\cr
 \tab \code{clearCache} \tab  -\cr
 \tab \code{equals} \tab  -\cr
 \tab \code{\link[R.filesets:extract.GenericDataFileSet]{extract}} \tab Extract a subset of the file set.\cr
 \tab \code{findByName} \tab  -\cr
 \tab \code{\link[R.filesets:getAlias.GenericDataFileSet]{getAlias}} \tab Gets the alias of the file set.\cr
 \tab \code{\link[R.filesets:getDefaultFullName.GenericDataFileSet]{getDefaultFullName}} \tab Gets the default full name of the file set.\cr
 \tab \code{getFile} \tab  -\cr
 \tab \code{getFileClass} \tab  -\cr
 \tab \code{getFileSize} \tab  -\cr
 \tab \code{getFullNames} \tab  -\cr
 \tab \code{\link[R.filesets:getNames.GenericDataFileSet]{getNames}} \tab Gets the names of the files in the file set.\cr
 \tab \code{\link[R.filesets:getPath.GenericDataFileSet]{getPath}} \tab Gets the path (directory) of the file set.\cr
 \tab \code{\link[R.filesets:getPathnames.GenericDataFileSet]{getPathnames}} \tab Gets the pathnames of the files in the file set.\cr
 \tab \code{hasFile} \tab  -\cr
 \tab \code{\link[R.filesets:indexOf.GenericDataFileSet]{indexOf}} \tab Finds indices of a subset of files.\cr
 \tab \code{\link[R.filesets:lapply.GenericDataFileSet]{lapply}} \tab Applies a function to each of the data files.\cr
 \tab \code{nbrOfFiles} \tab  -\cr
 \tab \code{sapply} \tab  -\cr
 \tab \code{\link[R.filesets:seq.GenericDataFileSet]{seq}} \tab Gets an vector of file indices.\cr
 \tab \code{\link[R.filesets:setAlias.GenericDataFileSet]{setAlias}} \tab Sets the alias of the file set.\cr
 \tab \code{validate} \tab  -\cr
}


 \bold{Methods inherited from FullNameInterface}:\cr
appendFullNameTranslator, appendFullNameTranslatorBycharacter, appendFullNameTranslatorByfunction, appendFullNameTranslatorBylist, appendFullNameTranslatorByNULL, clearFullNameTranslator, clearListOfFullNameTranslators, getDefaultFullName, getFullName, getFullNameTranslator, getListOfFullNameTranslators, getName, getTags, hasTag, hasTags, setFullName, setFullNameTranslator, setListOfFullNameTranslators, setName, setTags, updateFullName

 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save


 }

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}
\keyword{classes}
