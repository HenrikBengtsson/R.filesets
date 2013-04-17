%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFileSet.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{GenericDataFileSet$byName}
\alias{GenericDataFileSet$byName}
\alias{byName.GenericDataFileSet}
\alias{GenericDataFileSet.byName}
\alias{byName,GenericDataFileSet-method}

 \title{Locates and sets up a file set by its name}

 \description{
   Locates and sets up a file set by its name, tags, root and sub directories.
 }

 \usage{
## Static method (use this):
## GenericDataFileSet$byName(name, tags=NULL, subdirs=NULL, paths=NULL, ..., verbose=FALSE)

## Don't use the below:
\method{byName}{GenericDataFileSet}(static, name, tags=NULL, subdirs=NULL, paths=NULL, ..., verbose=FALSE)
}

 \arguments{
  \item{name, tags}{The name and the tags of the file set to be located.}
  \item{subdirs}{A \code{\link[base]{character}} \code{\link[base]{vector}} of the subpath where the file
     set is located.}
  \item{paths}{A \code{\link[base]{character}} \code{\link[base]{vector}} of root paths where to look for
     the file set.}
  \item{...}{Not used.}
  \item{verbose}{...}
 }

 \value{
   Returns a \code{\link{GenericDataFileSet}}.
   If not found, an exception is thrown.
 }

 \author{Henrik Bengtsson}

 \seealso{
   Internally, \code{\link[R.filesets:findByName.GenericDataFileSet]{*findByName}()} is used to locate the data set,
   and \code{\link[R.filesets:byPath.GenericDataFileSet]{*byPath}()} to then set it up.
   For more information see \code{\link{GenericDataFileSet}}.
 }
\keyword{internal}
\keyword{methods}
