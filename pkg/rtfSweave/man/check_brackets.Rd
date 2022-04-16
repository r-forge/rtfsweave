\name{check_brackets}
\alias{check_brackets}

\title{
  Check an RTF file for Balanced Curly Brackets
}
\description{
  Checks an RTF file for balanced curly brackets. The RTF file must
  start with \code{\{\\rtf1} and ends at the first closing \code{\}}.
}
\usage{
check_brackets(filename)
}
\arguments{
  \item{filename}{a length-one character vector indicating the RTF
  filename to check}
}
\value{
  This function returns a \dQuote{status code} of zero if
  the RTF file checks out and has balanced curly brackets.
  Non-zero status codes indicate a problem. These non-zero
  status codes are as follows:

  \tabular{ll}{
    \strong{Code} \tab \strong{Meaning}\cr
    1 \tab Cannot open the file to check curly brackets\cr
    2 \tab File did not begin with the required \code{\{\\rtf1}\cr
    3 \tab The curly brackets in the file nest too deeply for this program\cr
    4 \tab A closing \code{\}} was found without a prior \code{\{}\cr
    5 \tab The opening \code{\{\\rtf1} was never closed\cr
    6 \tab There is an opening \code{\{} that was never closed
  }
}
\details{
  An RTF file must begin with \code{\{\\rtf1} and ends as soon as that
  opening curly bracket is closed. (Usually anything after the closing
  curly bracket is ignored by RTF readers.)

  Curly brackets appearing in an RTF document need to be
  escaped with a backslash; this function ignores these.

}


\author{
Stephen Weigand <weigand.stephen@mayo.edu>
}

\examples{

## A valid RTF file
check_brackets(system.file("examples",
                           "hello-world.rtf",
                           package = "rtfSweave"))

## An RTF file missing a closing curly bracket
check_brackets(system.file("examples",
                           "unbalanced.rtf",
                           package = "rtfSweave"))

}

\keyword{files}
