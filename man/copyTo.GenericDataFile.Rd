%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFile.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{copyTo.GenericDataFile}
\alias{copyTo.GenericDataFile}
\alias{GenericDataFile.copyTo}
\alias{copyTo,GenericDataFile-method}

\title{Safely copies a file to a new pathname}

\description{
  Safely copies a file to a new pathname.
}

\usage{
\method{copyTo}{GenericDataFile}(this, filename=getFilename(this), path=NULL, ...)
}

\arguments{
 \item{filename, path}{The filename and the path for the destination file.
  The default is to use the same filename as the source file.}
 \item{...}{Additional arguments passed to \code{\link[R.utils]{copyFile}}.}
}

\value{
  Returns a \code{\link{GenericDataFile}} (of the same class as the source file)
  referring to the new file copy.
}

\details{
  In order to minimize the risk for corrupt copies, the
  \code{\link[R.utils]{copyFile}} method of \pkg{R.utils} is used, which
  provides several protection against user, system and file errors.
}

\author{Henrik Bengtsson}

\seealso{
  To link to a \code{\link{GenericDataFile}}, see \code{\link[R.filesets:linkTo.GenericDataFile]{*linkTo}()}.
  To rename a \code{\link{GenericDataFile}}, see \code{\link[R.filesets:renameTo.GenericDataFile]{*renameTo}()}.
  Internally \code{\link[R.utils]{copyFile}} is used.
  For more information see \code{\link{GenericDataFile}}.
}
\keyword{internal}
\keyword{methods}
