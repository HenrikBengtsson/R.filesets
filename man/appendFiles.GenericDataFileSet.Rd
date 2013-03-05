%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFileSet.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{appendFiles.GenericDataFileSet}
\alias{appendFiles.GenericDataFileSet}
\alias{GenericDataFileSet.appendFiles}
\alias{appendFiles,GenericDataFileSet-method}

\title{Appends a list of files to a data set}

\description{
  Appends a list of files to a data set.
}

\usage{
\method{appendFiles}{GenericDataFileSet}(this, files, clone=TRUE, ..., .fileClass=getFileClass(this), .assertSameClass=TRUE, verbose=FALSE)
}

\arguments{
 \item{files}{A single \code{\link{GenericDataFile}} or a \code{\link[base]{list}} of such to
   be appended.}
 \item{clone}{If \code{\link[base:logical]{TRUE}}, each file is cloned before being appened.}
 \item{...}{Additional arguments passed to \code{\link[base]{append}}().}
 \item{.fileClass}{A \code{\link[base]{character}} string specifying the class that
   all files must inherit from.
   If \code{\link[base]{NULL}}, \code{\link[R.filesets:getFileClass.GenericDataFileSet]{*getFileClass}()} is used.}
 \item{.assertSameClass}{If \code{\link[base:logical]{TRUE}}, the files to be appended must inherit
   from the same class as the existing files (the first file).}
 \item{verbose}{...}
}

\value{
  Returns (invisible) the appended data set (itself).
}

\details{
  The files appended must inherit the same class as the first file
  of the data set, otherwise an exception is thrown.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\seealso{
  To append a data set, see \code{\link{append}}().
  For more information see \code{\link{GenericDataFileSet}}.
}
\keyword{internal}
\keyword{methods}
