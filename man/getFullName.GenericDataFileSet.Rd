%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFileSet.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getFullName.GenericDataFileSet}
\alias{getFullName.GenericDataFileSet}
\alias{GenericDataFileSet.getFullName}
\alias{getFullName.GenericDataFileSet}
\alias{getFullName,GenericDataFileSet-method}


\title{Gets the full name of the file set}

\description{
  Gets the full name of the file set, that is the name of the directory without parent directories.
}

\usage{\method{getFullName}{GenericDataFileSet}(this, parent=1, translate=TRUE, ...)}

\arguments{
 \item{parent}{The number of generations up in the directory tree the
   directory name should be retrieved.  By default the current directory
   is used.}
 \item{translate}{If \code{\link[base:logical]{TRUE}}, an a fullname translator is set, the fullname
    is translated before returned.}
 \item{...}{Not used.}
}

\value{
  Returns a \code{\link[base]{character}}.
}

\details{
 By default, the full name of a file set is the name of the directory
 containing all the files, e.g. the name of file set \code{path/to,a,b/*}
 is \code{to,a,b}.
 Argument \code{parent=1} specifies that the parent directory should be
 used, and so on.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\seealso{
  For more information see \code{\link{GenericDataFileSet}}.
}
\keyword{internal}
\keyword{methods}
