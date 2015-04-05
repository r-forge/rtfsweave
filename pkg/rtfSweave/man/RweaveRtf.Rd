\name{RweaveRtf} % typically basename of the *.Rd file
\alias{RweaveRtf}
\alias{rtfSweave-package}
\alias{rtfSweave}
\alias{SweaveSyntaxRtf}
%\alias{rtfSweaveDriver}
%\alias{rtfTangleDriver}


\title{An Sweave driver for rich text format (RTF) documents}
\description{
  An add-on driver to allow Sweave to process RTF documents
}
\usage{
RweaveRtf()
SweaveSyntaxRtf
%rtfSweaveDriver()
%rtfTangleDriver()
}

\details{

  The \code{Sweave} function uses by default the \code{RweaveLatex}
  driver to process LaTeX documents. Yet \code{Sweave} was designed to
  allow authors to write their own drivers. The \code{rtfSweave} driver
  documented here is essentially a copy of the \code{RweaveLatex} driver
  with some necessary changes to make it work for rich text format (RTF)
  documents.

  RTF documents are plain text documents that have some similarities to
  LaTeX. For one thing, RTF documents can be opened and edited using
  Emacs or another text editor. While the RTF files written by Microsoft
  Word are difficult to read, without loss of functionality one can
  write very readable RTF documents which can be opened in Microsoft
  Word. One can write RTF documents with headings, subheadings, headers,
  footers, complex tables, figures, etc. It may be helpful to think of
  RTF as a text-based interface to nearly all of Microsoft Word's
  functionality.
  
  My motivation for creating an RTF driver for \code{Sweave} was that my
  non-statistician colleagues, collaborators, and clients almost
  exclusively use Microsoft Word.  Rather than converting a LaTeX report
  created with \code{Sweave} to RTF, it is arguably simpler to write the
  document in RTF directly, and use \code{Sweave} to create the final
  report which can be sent to colleagues as is or after saving in
  a native Microsoft Word format (e.g., \code{*.doc} or \code{*.docx}.
  
}

\note{ This driver is unlikely to work with documents written directly
  in Microsoft Word and saved as RTF prior to processing with
  \code{Sweave}. The problem is that code chunks
  that what might look like pure R code on screen in Word will end
  up with random linebreaks and a good bit of invisible RTF markup in
  the actual text file. It is difficult to repair the lines and remove
  the extraneous markup in these code chunks before they are
  passed to R for parsing.}


\author{Stephen Weigand \email{Weigand.Stephen@mayo.edu}}

\seealso{
  \code{\link{Sweave}}
}

\examples{
testfilepath <- system.file("examples", "rtfSweave-test-1.rtf",
                            package = "rtfSweave")
tempfilepath <- paste(tempfile(), ".rtf", sep = "")

## 'syntax' argument can be unspecified since per it's help
## Sweave can find syntax objects matching pattern 'SweaveSyntax.*'
Sweave(testfilepath, driver = RweaveRtf, syntax = SweaveSyntaxRtf,
       output = tempfilepath)
       
}

\keyword{utilities}% at least one, from doc/KEYWORDS
