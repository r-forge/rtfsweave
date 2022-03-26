\name{dotC}
\alias{dotC}

\title{
  Test my use of the .C interface function
}
\description{
  This function tests my use of the .C interface.
}
\usage{
dotC(filename)
}
\arguments{
  \item{filename}{a filename}
}
\value{
  Nothing. This function only prints to the console.
}

\author{
Stephen Weigand <weigand.stephen@mayo.edu>
}

\examples{

tfile <- tempfile()
writeLines(LETTERS, tfile)
dotC(tfile)

}

\keyword{files}
