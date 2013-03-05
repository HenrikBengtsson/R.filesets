%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFileSet.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{findByName.GenericDataFileSet}
\alias{findByName.GenericDataFileSet}
\alias{GenericDataFileSet.findByName}
\alias{findByName.GenericDataFileSet}
\alias{findByName,GenericDataFileSet-method}


 \title{Locates all file sets that match the requested name}

 \description{
   Locates all file sets that match the requested name, tags, and sub directories, in any of the root paths.
 }

 \usage{
## Static method (use this):
## GenericDataFileSet$findByName(name, tags=NULL, subdirs=NULL, paths=NULL, firstOnly=TRUE, mustExist=FALSE, ..., verbose=FALSE)

## Don't use the below:
\method{findByName}{GenericDataFileSet}(static, name, tags=NULL, subdirs=NULL, paths=NULL, firstOnly=TRUE, mustExist=FALSE, ..., verbose=FALSE)
}

 \arguments{
  \item{name, tags}{The name and the tags of the file set to be located.}
  \item{subdirs}{A \code{\link[base]{character}} \code{\link[base]{vector}} of the subpath where the file
     set is located.}
  \item{paths}{A \code{\link[base]{character}} \code{\link[base]{vector}} of root paths where to look for
     the file set.}
  \item{firstOnly}{If \code{\link[base:logical]{TRUE}}, only the first path found, if any, is returned,
     otherwise all found paths are returned.}
  \item{mustExist}{If \code{\link[base:logical]{TRUE}}, an exception is thrown if the file set was
     not found, otherwise not.}
  \item{...}{Not used.}
  \item{verbose}{...}
 }

 \value{
   Returns a \code{\link[base]{character}} \code{\link[base]{vector}} of paths.
   If no file sets were found, \code{\link[base]{NULL}} is returned.
 }

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

 \seealso{
   For more information see \code{\link{GenericDataFileSet}}.
 }
\keyword{internal}
\keyword{methods}
