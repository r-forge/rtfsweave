\name{render_rtf}
\alias{render_rtf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Set \pkg{knitr} output hooks for RTF format
}
\description{
  This is analogous to \code{render_sweave} in \pkg{knitr},
  defining hook functions of the form \code{rtf.hook.something}
  and then setting them via \code{knit_hooks$set(\dots)}.
}
\usage{
render_rtf()
}
%- maybe also 'usage' for other objects documented here.
% \details{
% %%  ~~ If necessary, more details than the description above ~~
% }
\value{
  \code{NULL} but corresponding hooks are set as a side effect.
}
\references{
  The \pkg{knitr} homepage is \url{http://yihui.name/knitr/} and
  in particular the hooks page is \url{http://yihui.name/knitr/hooks/}.
}
\author{
  Stephen Weigand \email{Weigand.Stephen@mayo.edu}
  adopting code originally from Yihui Xie.
}
\note{
  This function is in very early stages of development and I do not
  know what I am doing.
}

\seealso{
  %% ~~objects to See Also as \code{\link{help}}, ~~~
  \link[knitr]{render_sweave} and related functions. Also see
  \link[knitr]{knit_hooks}.
}
\examples{
## Put this in an early chunk in your .Rrtf document
## When you call 'knit' you need to specify 'output' otherwise
## the output is 'foo.txt' rather than 'foo.rtf'
\donttest{render_rtf()} %% <- I don't want users to have to have knitr
}                       %%    and may not have NAMESPACE or DESCRIPTION right
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

