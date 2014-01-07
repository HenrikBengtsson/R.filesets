%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFile.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{linkTo.GenericDataFile}
\alias{linkTo.GenericDataFile}
\alias{GenericDataFile.linkTo}
\alias{linkTo,GenericDataFile-method}
\alias{linkFrom}

\title{Creates a symbolic file link}

\description{
  Creates a symbolic file link to a \code{\link{GenericDataFile}} at/from a given
  destination pathname.
}

\usage{
\method{linkTo}{GenericDataFile}(this, filename=getFilename(this), path=NULL, ...)
}

\arguments{
 \item{filename, path}{The filename and the path for the link.
  The default is to use the same filename as the source file.}
 \item{...}{Additional arguments passed to \code{\link[R.utils]{createLink}}.}
}

\value{
  Returns a \code{\link{GenericDataFile}} (of the same class as the source file)
  refering to the file via the link.
}

\author{Henrik Bengtsson}

\seealso{
  To copy a \code{\link{GenericDataFile}}, see \code{\link[R.filesets:copyTo.GenericDataFile]{*copyTo}()}.
  Internally \code{\link[R.utils]{copyFile}} is used.
  For more information see \code{\link{GenericDataFile}}.
}
\keyword{internal}
\keyword{methods}
