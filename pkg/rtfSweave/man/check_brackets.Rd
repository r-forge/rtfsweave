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
  This function returns nothing and is useful only for the messages
  it prints to the console about the RTF file.
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
