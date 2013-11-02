%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFileSet.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{gzip.GenericDataFileSet}
\alias{gzip.GenericDataFileSet}
\alias{GenericDataFileSet.gzip}
\alias{gzip,GenericDataFileSet-method}
\alias{GenericDataFileSet.gunzip}
\alias{gunzip.GenericDataFileSet}
\alias{gunzip,GenericDataFileSet-method}


\title{Compresses/uncompresses a set of files}

\description{
  Compresses/uncompresses a set of files using gzip compression.
  When compressing (uncompressing), each of the \code{\link{GenericDataFile}}
  of the file set are compressed (uncompressed).
}

\usage{
 \method{gzip}{GenericDataFileSet}(this, ...)
 \method{gunzip}{GenericDataFileSet}(this, ...)
}

\arguments{
 \item{...}{Arguments passed to \code{gzip()/gunzip()} on each
   of the GenericDataFile entries.}
}

\value{
  Returns (invisibly) itself.
}

\author{Henrik Bengtsson}

\seealso{
  Internally \code{\link[R.utils]{gzip}} and \code{\link[R.utils]{gunzip}} are used.
  For more information see \code{\link{GenericDataFileSet}}.
}
\keyword{internal}
\keyword{methods}
