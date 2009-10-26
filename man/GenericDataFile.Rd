%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFile.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{GenericDataFile}
\docType{class}
\alias{GenericDataFile}


 \title{The abstract GenericDataFile class}

 \description{
  Package:  R.filesets \cr
\bold{Class GenericDataFile}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link[R.filesets]{FullNameInterface}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\emph{\code{GenericDataFile}}\cr

\bold{Directly known subclasses:}\cr
\emph{\link[R.filesets]{GenericTabularFile}}, \emph{\link[R.filesets]{TabularTextFile}}\cr

public abstract static class \bold{GenericDataFile}\cr
extends \emph{\link[R.filesets]{FullNameInterface}}\cr



  A GenericDataFile is an object refering to a data file on a file system.
  Note that this class is abstract and can not be instanciated, but
  instead you have to use one of the subclasses or the generic
  \code{\link[R.filesets:fromFile.GenericDataFile]{*fromFile}()} method.
 }

 \usage{GenericDataFile(filename=NULL, path=NULL, mustExist=TRUE, ...)}

 \arguments{
   \item{filename}{The filename of the file.}
   \item{path}{An optional path to the file.}
   \item{mustExist}{If \code{\link[base:logical]{TRUE}}, an exception is thrown if the file does
     not exists, otherwise not.}
   \item{...}{Not used.}
 }

 \section{Fields and Methods}{
  \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{compareChecksum} \tab  -\cr
 \tab \code{equals} \tab  -\cr
 \tab \code{fromFile} \tab  -\cr
 \tab \code{getAlias} \tab  -\cr
 \tab \code{getChecksum} \tab  -\cr
 \tab \code{getExtension} \tab  -\cr
 \tab \code{getExtensionPattern} \tab  -\cr
 \tab \code{\link[R.filesets:getFilename.GenericDataFile]{getFilename}} \tab Gets the filename of the file.\cr
 \tab \code{getFileSize} \tab  -\cr
 \tab \code{\link[R.filesets:getFileType.GenericDataFile]{getFileType}} \tab Gets the file type of a file.\cr
 \tab \code{\link[R.filesets:getPath.GenericDataFile]{getPath}} \tab Gets the path (directory) of the file.\cr
 \tab \code{\link[R.filesets:getPathname.GenericDataFile]{getPathname}} \tab Gets the pathname of the file.\cr
 \tab \code{isFile} \tab  -\cr
 \tab \code{readChecksum} \tab  -\cr
 \tab \code{setAlias} \tab  -\cr
 \tab \code{setExtensionPattern} \tab  -\cr
 \tab \code{validateChecksum} \tab  -\cr
 \tab \code{writeChecksum} \tab  -\cr
}


 \bold{Methods inherited from FullNameInterface}:\cr
appendFullNameTranslator, appendFullNameTranslatorBycharacter, appendFullNameTranslatorByfunction, appendFullNameTranslatorBylist, appendFullNameTranslatorByNULL, clearFullNameTranslator, clearListOfFullNameTranslators, getDefaultFullName, getFullName, getFullNameTranslator, getListOfFullNameTranslators, getName, getTags, hasTag, hasTags, setFullName, setFullNameTranslator, setListOfFullNameTranslators, setName, setTags, updateFullName

 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save


 }

 \section{Filename convention}{
   The filename of an \code{GenericDataFile} is structured as follows:
   \itemize{
    \item{filename}{\code{"sample001,a,b,c.CEL"}
       (this follows the \R convention (but not the Unix convention)}
    \item{fullname}{\code{"sample001,a,b,c"}}
    \item{name}{\code{"sample001"}}
    \item{tags}{\code{c("a", "b", "c")}}
    \item{extension}{\code{"CEL"}}
   }
 }

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

 \seealso{
   An object of this class is typically part of an \code{\link{GenericDataFileSet}}.
 }
\keyword{classes}
