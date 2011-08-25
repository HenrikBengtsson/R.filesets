%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFileSet.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getDefaultFullName.GenericDataFileSet}
\alias{getDefaultFullName.GenericDataFileSet}
\alias{GenericDataFileSet.getDefaultFullName}
\alias{getDefaultFullName.GenericDataFileSet}
\alias{getDefaultFullName,GenericDataFileSet-method}


\title{Gets the default full name of the file set}

\description{
  Gets the default full name of the file set, that is the name of the directory without parent directories.
}

\usage{\method{getDefaultFullName}{GenericDataFileSet}(this, parent=getDepth(this), ...)}

\arguments{
 \item{parent}{The number of generations up in the directory tree the
   directory name should be retrieved.  By default the current directory
   is used.}
 \item{...}{Not used.}
}

\value{
  Returns a \code{\link[base]{character}}.
}

\details{
 By default, the full name of a file set is the name of the directory
 containing all the files, e.g. the name of file set
 \code{path/foo,c/to,a,b/*} is \code{to,a,b}.
 Argument \code{parent=1} specifies that the parent directory should be
 used, e.g. \code{foo,c}.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\seealso{
  For more information see \code{\link{GenericDataFileSet}}.
}
\keyword{internal}
\keyword{methods}
