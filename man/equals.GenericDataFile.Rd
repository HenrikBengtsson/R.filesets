%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFile.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{equals.GenericDataFile}
\alias{equals.GenericDataFile}
\alias{GenericDataFile.equals}
\alias{equals,GenericDataFile-method}

\title{Checks if a file equals another}

\description{
 Checks if a file equals another.
}

\usage{
\method{equals}{GenericDataFile}(this, other, ...)
}

\arguments{
  \item{other}{The other \code{\link{GenericDataFile}} to be compared to.}
  \item{...}{Not used.}
}

\value{
  Returns \code{\link[base:logical]{TRUE}} if the file equals the other, otherwise \code{\link[base:logical]{FALSE}}.
  If \code{\link[base:logical]{FALSE}}, attributes are added specifying the pathnames of the two
  files compared, and the reason for them being different.
}

\details{
  The two files compared are equal if they have the same pathname.

  The two files compared are \emph{not} equal if:
  \itemize{
   \item Argument \code{other} is not a \code{\link{GenericDataFile}}, or
   \item their file sizes differ, or
   \item their file checksums differ.
  }

  If none of the above occurs, the two files are considered equal.

  Note that subclasses use refined rules.
}

\author{Henrik Bengtsson}

\seealso{
  \code{\link[R.filesets:getFileSize.GenericDataFile]{*getFileSize}()}.
  \code{\link[R.filesets:getChecksum.GenericDataFile]{*getChecksum}()}.
  For more information see \code{\link{GenericDataFile}}.
}



\keyword{internal}
\keyword{methods}
\keyword{IO}
\keyword{programming}
