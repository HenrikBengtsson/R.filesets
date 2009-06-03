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
\code{~~+--}\code{\link[R.filesets]{GenericDataFileSet}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{\link[R.filesets]{GenericTabularFileSet}}\cr
\code{~~~~~~~~~~~~|}\cr
\code{~~~~~~~~~~~~+--}\code{TabularTextFileSet}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{TabularTextFileSet}\cr
extends \link[R.filesets]{GenericTabularFileSet}\cr



  An TabularTextFileSet object represents a set of \code{\link{TabularTextFile}}s.
 }

 \usage{TabularTextFileSet(...)}

 \arguments{
   \item{...}{Arguments passed to \code{\link{GenericTabularFileSet}}.}
 }

 \section{Fields and Methods}{
  \bold{Methods:}\cr
\emph{No methods defined}.


 \bold{Methods inherited from GenericTabularFileSet}:\cr
extractMatrix

 \bold{Methods inherited from GenericDataFileSet}:\cr
append, appendFiles, as.list, byName, clearCache, copyTo, equals, extract, findByName, fromFiles, getAlias, getFile, getFileClass, getFileSize, getFullName, getFullNames, getFullNameTranslator, getName, getNames, getPath, getPathnames, getTags, hasFile, hasTag, hasTags, indexOf, lapply, nbrOfFiles, sapply, seq, setAlias, setFullName, setFullNamesTranslator, setFullNameTranslator, setName, setTags, update2, validate

 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, save


 }

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}
\keyword{classes}
