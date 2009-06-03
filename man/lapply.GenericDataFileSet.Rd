%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFileSet.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{lapply.GenericDataFileSet}
\alias{lapply.GenericDataFileSet}
\alias{GenericDataFileSet.lapply}
\alias{lapply.GenericDataFileSet}
\alias{lapply,GenericDataFileSet-method}


\title{Applies a function to each of the data files}

\description{
  Applies a function to each of the data files.
}

\usage{\method{lapply}{GenericDataFileSet}(this, ...)}

\arguments{
 \item{...}{Not used.}
}

\value{
  Returns a named \code{\link[base]{list}} where the names are the names of the date files
  according to \code{\link[R.filesets:getNames.GenericDataFileSet]{*getNames}()}.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\seealso{
  For more information see \code{\link{GenericDataFileSet}}.
}
\keyword{internal}
\keyword{methods}
