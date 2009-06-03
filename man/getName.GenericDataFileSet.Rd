%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  GenericDataFileSet.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{getName.GenericDataFileSet}
\alias{getName.GenericDataFileSet}
\alias{GenericDataFileSet.getName}
\alias{getName.GenericDataFileSet}
\alias{getName,GenericDataFileSet-method}


\title{Gets the name of the file set}

\description{
  Gets the name of the file set.
}

\usage{\method{getName}{GenericDataFileSet}(this, ...)}

\arguments{
 \item{...}{Additional arguments passed to \code{\link[R.filesets:getFullName.GenericDataFileSet]{*getFullName}()}, if
   the name is inferred from the pathname.}
}

\value{
  Returns a \code{\link[base]{character}}.
}

\details{
 If a name alias has not been set explicitly, the name is inferred from
 the pathname of the file set.
 The \emph{name} of a file set is the part of the directory name that
 preceeds the first comma, if any.
 For instance, the name of the file set named \code{foo,a,b} is \code{foo}.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\seealso{
  \code{\link[R.filesets:getAlias.GenericDataFileSet]{*getAlias}()}.
  \code{\link[R.filesets:getFullName.GenericDataFileSet]{*getFullName}()}.
  \code{\link[R.filesets:getTags.GenericDataFileSet]{*getTags}()}.
  For more information see \code{\link{GenericDataFileSet}}.
}
\keyword{internal}
\keyword{methods}
