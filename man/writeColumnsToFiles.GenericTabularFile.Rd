%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericTabularFile.writeColumnsToFiles.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\name{writeColumnsToFiles.GenericTabularFile}
\alias{writeColumnsToFiles.GenericTabularFile}
\alias{GenericTabularFile.writeColumnsToFiles}
\alias{writeColumnsToFiles,GenericTabularFile-method}

\title{Read each column from a data file and exports it to a separate file}

\description{
 Read each column from a data file and exports it to a separate file.
 Since each column is processed independently of the others, this method
 is memory efficient and can handle very large data files.
}

\usage{
\method{writeColumnsToFiles}{GenericTabularFile}(this, destPath, filenameFmt="\%s.txt", tags=NULL, columnName=NULL,
  header=NULL, ..., verbose=FALSE)
}

\arguments{
  \item{destPath}{The output directory where to write the files.}
  \item{filenameFmt}{An \code{\link[base]{sprintf}}() format string used to generate
   filenames given the fullnames (column names plus tags).}
  \item{tags}{An optional \code{\link[base]{character}} \code{\link[base]{vector}} of tags added to the fullnames.}
  \item{columnName}{...}
  \item{header}{An optional file header.}
  \item{...}{Not used.}
  \item{verbose}{See \code{\link[R.utils]{Verbose}}.}
}

\value{
 Returns (invisibly) a \code{\link[base]{character}} \code{\link[base]{vector}} of all output files.
}

\details{
 Each file generated is written atomically by first writing to a temporary
 file which is then renamed if successfully written.  This minimizes the
 risk for creating incomplete files, which otherwise may occur if for
 instance an interrupt occurred.
}

\author{Henrik Bengtsson}

\seealso{
  For more information see \code{\link{GenericTabularFile}}.
}



\keyword{internal}
\keyword{methods}
\keyword{IO}
\keyword{programming}
