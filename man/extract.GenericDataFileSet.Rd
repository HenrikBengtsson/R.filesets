%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFileSet.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{extract.GenericDataFileSet}
\alias{extract.GenericDataFileSet}
\alias{GenericDataFileSet.extract}
\alias{extract,GenericDataFileSet-method}
\alias{extract}
\alias{GenericDataFileSet.[}
\alias{[.GenericDataFileSet}
\alias{[,GenericDataFileSet-method}


\title{Extract a subset of the file set}

\description{
  Extract a subset of the file set.
}

\usage{
\method{extract}{GenericDataFileSet}(this, files, ..., onMissing=c("NA", "error", "drop", "dropall"),
  onDuplicates=c("ignore", "drop", "error"))
}

\arguments{
 \item{files}{An \code{\link[base]{integer}} or a \code{\link[base]{logical}} \code{\link[base]{vector}} indicating which data files
   to be extracted.  Negative indices are excluded.}
 \item{...}{Not used.}
 \item{onMissing}{A \code{\link[base]{character}} specifying the action if a requested file
   does not exist.  If \code{"error"}, an error is thrown.  If \code{"NA"},
   a \code{\link{GenericDataFile}} referring to an \code{\link[base]{NA}} pathname is used in place.
   If \code{"drop"}, the missing file is dropped.
   If \code{"dropall"}, an empty data set is return if one or more
   missing files are requested.
 }
}

\value{
  Returns a \code{\link{GenericDataFileSet}} with zero of more
  \code{\link{GenericDataFile}}:s.
}

\author{Henrik Bengtsson}

\seealso{
  \code{\link[stats]{na.omit}} for dropping missing files from a fileset.
  For more information see \code{\link{GenericDataFileSet}}.
}
\keyword{internal}
\keyword{methods}
