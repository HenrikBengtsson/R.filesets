%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFile.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{gzip.GenericDataFile}
\alias{gzip.GenericDataFile}
\alias{GenericDataFile.gzip}
\alias{gzip,GenericDataFile-method}
\alias{GenericDataFile.gunzip}
\alias{gunzip.GenericDataFile}
\alias{gunzip,GenericDataFile-method}

\alias{GenericDataFile.isGzipped}
\alias{isGzipped.GenericDataFile}
\alias{isGzipped,GenericDataFile-method}


\title{Compresses/uncompresses a file}

\description{
  Compresses/uncompresses a file using gzip compression.
  When compressing (uncompressing), the new filename has suffix \code{.gz}
  appended (removed), which is also used to test if a file is gzip'ed
  or not.
}

\usage{
 \method{gzip}{GenericDataFile}(this, ...)
 \method{gunzip}{GenericDataFile}(this, ...)
 \method{isGzipped}{GenericDataFile}(this, ...)
}

\arguments{
 \item{...}{Arguments passed to "R.utils::gzip" and "R.utils::gunzip",
   respectively.}
 \item{verbose}{...}
}

\value{
  Returns (invisibly) the updated pathname.
  When compressing (uncompressing), an exception is thrown if the file
  is already compressed (not compressed).
  An exception is thrown if the file does not exist.
}

\author{Henrik Bengtsson}

\seealso{
  Internally \code{\link[R.utils]{gzip}} is used.
  For more information see \code{\link{GenericDataFile}}.
}
\keyword{internal}
\keyword{methods}
