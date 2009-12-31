%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFileSetList.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{GenericDataFileSetList}
\docType{class}
\alias{GenericDataFileSetList}


 \title{The GenericDataFileSetList class}

 \description{
  Package:  R.filesets \cr
\bold{Class GenericDataFileSetList}\cr

\code{\link[R.oo]{Object}}\cr
\code{~~|}\cr
\code{~~+--}\code{\link[R.filesets]{FullNameInterface}}\cr
\code{~~~~~~~|}\cr
\code{~~~~~~~+--}\code{GenericDataFileSetList}\cr

\bold{Directly known subclasses:}\cr
\cr

public static class \bold{GenericDataFileSetList}\cr
extends \emph{\link[R.filesets]{FullNameInterface}}\cr



  A GenericDataFileSetList object represents a list of
  \code{\link{GenericDataFileSet}}s.
 }

 \usage{GenericDataFileSetList(dsList=list(), tags="*", ..., .setClass="GenericDataFileSet")}

 \arguments{
   \item{dsList}{A single or a \code{\link[base]{list}} of \code{\link{GenericDataFileSet}}:s.}
   \item{tags}{A \code{\link[base]{character}} \code{\link[base]{vector}} of tags.}
   \item{...}{Not used.}
   \item{.setClass}{A \code{\link[base]{character}} string specifying a name of the
     class that each data set must be an instance of.}
 }

 \section{Fields and Methods}{
  \bold{Methods:}\cr
\tabular{rll}{
 \tab \code{as.character} \tab  -\cr
 \tab \code{as} \tab  -\cr
 \tab \code{as.GenericDataFileSetList} \tab  -\cr
 \tab \code{as.list} \tab  -\cr
 \tab \code{clone} \tab  -\cr
 \tab \code{getDefaultFullName} \tab  -\cr
 \tab \code{getFileList} \tab  -\cr
 \tab \code{getFullNames} \tab  -\cr
 \tab \code{getNames} \tab  -\cr
 \tab \code{getSet} \tab  -\cr
 \tab \code{getSets} \tab  -\cr
 \tab \code{indexOf} \tab  -\cr
 \tab \code{length} \tab  -\cr
 \tab \code{nbrOfFiles} \tab  -\cr
 \tab \code{nbrOfSets} \tab  -\cr
 \tab \code{setTags} \tab  -\cr
}


 \bold{Methods inherited from FullNameInterface}:\cr
appendFullNameTranslator, appendFullNameTranslatorBycharacter, appendFullNameTranslatorByfunction, appendFullNameTranslatorBylist, appendFullNameTranslatorByNULL, clearFullNameTranslator, clearListOfFullNameTranslators, getDefaultFullName, getFullName, getFullNameTranslator, getListOfFullNameTranslators, getName, getTags, hasTag, hasTags, setFullName, setFullNameTranslator, setListOfFullNameTranslators, setName, setTags, updateFullName

 \bold{Methods inherited from Object}:\cr
$, $<-, [[, [[<-, as.character, attach, attachLocally, clearCache, clone, detach, equals, extend, finalize, gc, getEnvironment, getFields, getInstantiationTime, getStaticInstance, hasField, hashCode, ll, load, objectSize, print, registerFinalizer, save


 }

 \examples{
# Setup a file set
path1 <- system.file(package="R.filesets")
ds1 <- GenericDataFileSet$byPath(path1)

path2 <- system.file(package="R.utils")
ds2 <- GenericDataFileSet$byPath(path2)

dsl <- GenericDataFileSetList(list(ds1, ds2), tags=c("*", "CustomTag"))
print(dsl)

df <- as.data.frame(dsl)
print(df)

print(df["DESCRIPTION","R.filesets"])

}

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}
\keyword{classes}
