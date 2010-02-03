%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFileSet.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

 \name{copyTo.GenericDataFileSet}
\alias{copyTo.GenericDataFileSet}
\alias{GenericDataFileSet.copyTo}
\alias{copyTo.GenericDataFileSet}
\alias{copyTo,GenericDataFileSet-method}


 \title{Copies a data set to another directory}

 \description{
   Copies a data set to another directory.
 }

 \usage{\method{copyTo}{GenericDataFileSet}(this, path=NULL, ..., verbose=FALSE)}

 \arguments{
  \item{path}{The destination directory.  If missing, it is created.}
  \item{...}{Not used.}
  \item{verbose}{...}
 }

 \value{
   Returns a \code{\link{GenericDataFileSet}} consisting the new file copies.
 }

 \details{
   Each file is copied safely, but if this method is interrupted, it
   may results in a data set consisting of fewer than the original
   data set.
   FUTURE: In order to minimize the risk for this, we may consider to
   first copy to a temporary directory, which is then renamed, cf. how
   individual files are safely copied.
 }

 \author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

 \seealso{
   For more information see \code{\link{GenericDataFileSet}}.
 }
\keyword{internal}
\keyword{methods}
