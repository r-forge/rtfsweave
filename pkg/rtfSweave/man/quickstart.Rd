\name{quickstart}
\alias{quickstart}

\title{
  Prompt the user for information in order to generate an rtfSweave
  compatible
  Sweave file
  skeleton to help a user get started using rtfSweave
}
\description{
  Interactively ask the user a series of questions to generate
  a file which can be weaved using rtfSweave. The questions
  include the file name, document orientation, margins, etc.
}
\usage{
quickstart()
}
%% \arguments{
%% }
\value{
  This function returns nothing but generates the following files and
  directories:
  \itemize{
    \item A documentation + code \dQuote{skeleton} file usually with file extension
      \code{Rrtf}. This file includes a code chunk with a \code{build}
      function that can be evaluated to weave the file.
      
    \item A header file containing RTF boilerplate based on the answers
      to the prompts. It is \dQuote{included} into the documentation +
      code file to reduce clutter in the main file.

    \item A user-specified subdirectory defaulting to \code{figures}
      which holds the figures. (This applies only if the subdirectory
      does not already exist.)
  }

}
\details{

  Assuming the named the skeleton file \code{report.Rrtf}, it
  would look something like this:
  \preformatted{\{\\rtf1
\{\\SweaveInput report-header.rtf\}

<<information, engine=none>>=

This code chunk will be ignored since 'engine' is neither 'R' nor 'S'

@


<<build-function, eval=false, echo=false>>=
## You can use this function to "build" the RTF file
build <- function() \{
    Sweave("test.Rrtf",
           RweaveRtf(),
           SweaveSyntaxRtf,
           prefix.string = "figures/fig")
\}
@

\}
}
  
}


\author{
Stephen Weigand <weigand.stephen@mayo.edu>
}

\examples{
quickstart()
}

\keyword{files}
