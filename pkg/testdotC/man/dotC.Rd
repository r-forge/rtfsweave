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
  A status value for the RTF file. A status of 0 means the
  RTF file was OK in terms of curly brackets. 
}

\author{
Stephen Weigand <weigand.stephen@mayo.edu>
}

\examples{

file.rtf <- tempfile(fileext = ".rtf")
cat("Hello, RTF", file = file.rtf)
dotC(file.rtf)
file.remove(file.rtf)

}

\keyword{files}
