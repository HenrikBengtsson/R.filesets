%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFileSet.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{append.GenericDataFileSet}
\alias{append.GenericDataFileSet}
\alias{GenericDataFileSet.append}
\alias{append.GenericDataFileSet}
\alias{append,GenericDataFileSet-method}


\title{Appends one data set to an existing one}

\description{
  Appends one data set to an existing one.
  The fullname of the merged data set is that of the first data set.
}

\usage{
\method{append}{GenericDataFileSet}(x, values, ...)
}

\arguments{
 \item{values}{The data set to be appended to this data set.}
 \item{...}{Additional arguments passed to \code{\link[R.filesets:appendFiles.GenericDataFileSet]{*appendFiles}()}.}
}

\value{
  Returns a merged \code{\link{GenericDataFileSet}} of the same class as the
  first data set.
}

\details{
  By default, in order to avoid downstream side effects on the data set
  being appended, each of its file is cloned before being appended.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\seealso{
  To append one or more files, see \code{\link{appendFiles}}().
  For more information see \code{\link{GenericDataFileSet}}.
}
\keyword{internal}
\keyword{methods}
