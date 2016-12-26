\name{rtf.options}
\alias{rtf.options}
\alias{.RTF.Options}
\alias{.RTF.Options.default}
\alias{rtf.getOption}

\title{
  Get or Set RTF Options
}

\description{
  This is based on a combination of
  \code{\link{ps.options}}/\code{\link{pdf.options}} and
  \code{\link{lattice.options}}/\code{\link{lattice.getOption}}
    and allows the user to get or set options for RTF output. There is
    no checking or validating options since the user may want to
    define new options.
}

\usage{
rtf.options(..., reset = FALSE)
rtf.getOption(name)
}

\arguments{
  \item{\dots}{
    new options can be defined, or existing ones modified, using
    one or more arguments of the form \code{name = value}.
}
  \item{reset}{
    %%     ~~Describe \code{reset} here~~
    logical: should the defaults be reset to their
    \dQuote{factory-fresh} values? Currently this does not work
    perhaps because I do not understand \code{\link{assign}}.
}
%   \item{override.check}{
%     %%     ~~Describe \code{override.check} here~~
%     logical argument passed to \code{grDevices::check.options}.
% }
\item{name}{length one character vector giving the name of an option}
}
\details{
  There are a number of ways to set options in \R and I do not
  know how best to set RTF options. The functions
  \code{\link{options}} and \code{\link{getOption}} can be used
  to set and get user-defined opitons. There is a function
  called \code{\link{strOptions}} which is described as
  a convenience function for setting \code{options(str = .)}.

  Outside the \code{base::options} mechanism, there are the functions
  \code{\link{ps.options}} and \code{\link{pdf.options}} which set
  options for PostScript and PDF devices. Similar to these are
  \code{\link{lattice.options}} and \code{\link{lattice.getOption}}.
  These are what \code{rtf.options} and \code{rtf.getOption} are based
  on.   Perhaps it would have been better to use
  an approach based on \code{options(rtf = .)}, especially since
  I do not understand why the \code{reset = TRUE} argument
  does not work for me.
  
}
\value{
A list containing all the options that are set.
}
% \references{
%   The RTF 1.5 specification is available on line
%   at \url{http://www.biblioscape.com/rtf15_spec.htm}.
% 
% }
\author{
Stephen Weigand \email{Weigand.Stephen@mayo.edu}.
}
% \note{
% %%  ~~further notes~~
% }

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{options}}, \code{\link{ps.options}}, \code{\link{pdf.options}},
  and \code{\link{lattice.options}}.
}
\examples{
## New option
rtf.options(landscape = TRUE)

## Show the option
rtf.getOption("landscape")
           
## Show all the options
str(rtf.options())

## Does not do a reset
rtf.options(reset = TRUE)

## See?
str(rtf.options())

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

