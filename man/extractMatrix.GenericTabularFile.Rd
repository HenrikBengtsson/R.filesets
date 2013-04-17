%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericTabularFile.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{extractMatrix.GenericTabularFile}
\alias{extractMatrix.GenericTabularFile}
\alias{GenericTabularFile.extractMatrix}
\alias{extractMatrix,GenericTabularFile-method}

\title{Reads one of the columns}

\description{
 Reads one of the columns.
}

\usage{
\method{extractMatrix}{GenericTabularFile}(this, column=1, drop=FALSE, ..., verbose=FALSE)
}

\arguments{
  \item{column}{An \code{\link[base]{integer}} specifying the column to read.}
  \item{drop}{If \code{\link[base:logical]{TRUE}}, a \code{\link[base]{vector}} is returned,
    otherwise a one-column \code{\link[base]{matrix}}.}
  \item{...}{Additional arguments passed to \code{\link[R.filesets:readColumns.GenericTabularFile]{*readColumns}()}.}
  \item{verbose}{A \code{\link[base]{logical}} or a \code{\link[R.utils]{Verbose}} object.}
}

\value{
  Returns a Jx1 \code{\link[base]{matrix}}, or if \code{drop=TRUE} a \code{\link[base]{vector}} of length J.
}

\author{Henrik Bengtsson}

\seealso{
  For more information see \code{\link{GenericTabularFile}}.
}



\keyword{internal}
\keyword{methods}
\keyword{IO}
\keyword{programming}
