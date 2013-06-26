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
\method{copyTo}{GenericDataFile}(this, filename=getFilename(this), path=NULL, overwrite=FALSE, ..., verbose=TRUE)
}

\arguments{
 \item{filename, path}{The filename and the path for the destination file.
  The default is to use the same filename as the source file.
  The destination pathname must not be the same as the source file,
  otherwise an exception is thrown.}
 \item{overwrite}{If \code{\link[base:logical]{TRUE}}, existing files are overwritten, otherwise not.
  If \code{\link[base:logical]{FALSE}} and the file already exists, an exception is thrown.}
 \item{...}{Not used.}
 \item{recursive}{If TRUE, ...}
 \item{verbose}{...}
}

\value{
  Returns a \code{\link{GenericDataFile}} (of the same class as the source file)
  refering to the new filname.
  If the source and destination pathnames are identical, an exception
  is thrown.
}

\details{
  In order to minimize the risk for corrupt copies, the
  \code{\link[R.utils]{copyFile}} method of \pkg{R.utils} is used.
  That method first copies the file to a temporary file, which is then
  renamed.  This minimizes the risk of incomplete files.
  It also asserts that the file sizes of the source file and the copy
  are identical.
}

\author{Henrik Bengtsson}

\seealso{
  \code{\link[R.filesets:renameTo.GenericDataFile]{*renameTo}()}.
  Internally \code{\link[R.utils]{copyFile}} is used.
  For more information see \code{\link{GenericDataFile}}.
}
\keyword{internal}
\keyword{methods}
