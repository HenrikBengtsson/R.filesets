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
\method{linkTo}{GenericDataFile}(this, filename=getFilename(this), path=NULL, skip=!overwrite, overwrite=FALSE,
  ...)
}

\arguments{
 \item{filename, path}{The filename and the path for the link.
  The default is to use the same filename as the source file.}
  \item{skip}{If \code{\link[base:logical]{TRUE}} and a file with the same name as argument
    \code{link} already exists, then the nothing is done.}
  \item{overwrite}{If \code{\link[base:logical]{TRUE}}, an existing link file is overwritten,
    otherwise not.}
 \item{...}{Additional arguments passed to \code{\link[R.utils]{createLink}}.}
}

\value{
  Returns a \code{\link{GenericDataFile}} (of the same class as the source file)
  refering to the file via the link.
}

\section{Required privileges on Windows}{
  In order for this method to succeed on Windows,
  the client/R session must run with sufficient privileges.
  See \code{\link[R.utils]{createLink}} for more details.
}

\author{Henrik Bengtsson}

\seealso{
  To copy a \code{\link{GenericDataFile}}, see \code{\link[R.filesets:copyTo.GenericDataFile]{*copyTo}()}.
  Internally \code{\link[R.utils]{createLink}} is used.
  For more information see \code{\link{GenericDataFile}}.
}
\keyword{internal}
\keyword{methods}
