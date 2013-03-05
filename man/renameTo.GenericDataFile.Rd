%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFile.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{renameTo.GenericDataFile}
\alias{renameTo.GenericDataFile}
\alias{GenericDataFile.renameTo}
\alias{renameTo,GenericDataFile-method}

 \title{Renames/moves a file}

 \description{
   Renames/moves a file.
 }

 \usage{
\method{renameTo}{GenericDataFile}(this, filename=getFilename(this), path=NULL, ..., verbose=TRUE)
}

 \arguments{
  \item{filename, path}{The filename and the path for the destination file.
   The default is to use the same filename as the source file.
   The destination pathname must not be the same as the source file,
   otherwise an exception is thrown.}
  \item{...}{Not used.}
  \item{verbose}{...}
 }

 \value{
   Returns the soure \code{\link{GenericDataFile}}.
 }

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

 \seealso{
   \code{\link[R.filesets:copyTo.GenericDataFile]{*copyTo}()}.
   \code{\link[base]{file.rename}}().
   For more information see \code{\link{GenericDataFile}}.
 }
\keyword{internal}
\keyword{methods}
