%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Do not modify this file since it was automatically generated from:
% 
%  FullNameInterface.R
% 
% by the Rdoc compiler part of the R.oo package.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\name{hasTags.FullNameInterface}
\alias{hasTags.FullNameInterface}
\alias{FullNameInterface.hasTags}
\alias{hasTags,FullNameInterface-method}
\alias{FullNameInterface.hasTag}
\alias{hasTag.FullNameInterface}
\alias{hasTag,FullNameInterface-method}


\title{Checks whether the fullname contains a given set of tag(s)}

\description{
  Checks whether the fullname contains a given set of tag(s).
}

\usage{
 \method{hasTags}{FullNameInterface}(this, tags, ...)
 \method{hasTag}{FullNameInterface}(this, tag, ...)
}

\arguments{
 \item{tags, tag}{A \code{\link[base]{character}} \code{\link[base]{vector}} or scalar, of tags to looked for.}
 \item{...}{Additional arguments passed to \code{\link[R.filesets:getTags.FullNameInterface]{*getTags}()}.}
}

\value{
  Returns \code{\link[base:logical]{TRUE}} if \emph{all} tags are part of the fullname, otherwise
  \code{\link[base:logical]{FALSE}}.
}

\author{Henrik Bengtsson (\url{http://www.braju.com/R/})}

\seealso{
  \code{\link[R.filesets:getTags.FullNameInterface]{*getTags}()}.
  \code{\link[R.filesets:getName.FullNameInterface]{*getName}()}.
  For more information see \code{\link{FullNameInterface}}.
}
\keyword{internal}
\keyword{methods}
