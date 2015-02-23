%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFileSet.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getFile.GenericDataFileSet}
\alias{getFile.GenericDataFileSet}
\alias{GenericDataFileSet.getFile}
\alias{getFile,GenericDataFileSet-method}
\alias{GenericDataFileSet.[[}
\alias{[[.GenericDataFileSet}
\alias{[[,GenericDataFileSet-method}


\title{Get a particular file of the file set}

\description{
 Get a particular file of the file set.
}

\usage{
\method{getFile}{GenericDataFileSet}(this, idx, ...)
}

\arguments{
  \item{idx}{A \code{\link[base]{numeric}} index or a \code{\link[base]{character}} string specifying the
   file to be returned.}
  \item{...}{Not used.}
}

\value{
  Returns a @GenericDataFile.
}

\author{Henrik Bengtsson}

\seealso{
  For more information see \code{\link{GenericDataFileSet}}.
  If argument \code{idx} is a \code{\link[base]{character}}, then internally
  \code{\link[R.filesets:indexOf.GenericDataFileSet]{*indexOf}()} is used to identify what to return.
}



\keyword{internal}
\keyword{methods}
\keyword{IO}
\keyword{programming}
