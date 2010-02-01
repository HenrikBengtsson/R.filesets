%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFileSet.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getNames.GenericDataFileSet}
\alias{getNames.GenericDataFileSet}
\alias{GenericDataFileSet.getNames}
\alias{getNames.GenericDataFileSet}
\alias{getNames,GenericDataFileSet-method}

\alias{GenericDataFileSet.getFullNames}
\alias{getFullNames.GenericDataFileSet}
\alias{getFullNames,GenericDataFileSet-method}


\title{Gets the names (or fullnames) of the files in the file set}

\description{
  Gets the names (or fullnames) of the files in the file set.
}

\usage{\method{getNames}{GenericDataFileSet}(this, ...)}

\usage{\method{getFullNames}{GenericDataFileSet}(this, ...)}

\arguments{
 \item{...}{Arguments passed to \code{getName()} (\code{getFullName()})
   of each file.}
}

\value{
  Returns a \code{\link[base]{character}} \code{\link[base]{vector}} of length equal to the number of files
  in the set.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\seealso{
  \code{\link[R.filesets:indexOf.GenericDataFileSet]{*indexOf}()}
  For more information see \code{\link{GenericDataFileSet}}.
}
\keyword{internal}
\keyword{methods}
