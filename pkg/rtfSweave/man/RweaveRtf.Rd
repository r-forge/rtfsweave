\name{RweaveRtf} % typically basename of the *.Rd file
\alias{RweaveRtf} %% This is the driver
%% \alias{rtfSweave-package} Now I have my own rtfSweave-package.Rd file
\alias{rtfSweave}
\alias{SweaveSyntaxRtf}
%\alias{rtfSweaveDriver}
%\alias{rtfTangleDriver}


\title{An Sweave driver for rich text format (RTF) documents}
\description{
  An add-on driver to enable Sweave to process RTF documents
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
  write very human-readable RTF documents which can be opened in Microsoft
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
  a native Microsoft Word format (e.g., \code{*.doc} or \code{*.docx}).
  
}

\section{Important options for \pkg{rtfSweave}}{
\describe{
  \item{\code{png}}{The default for figures.}
  \item{\code{jpeg}}{JPEG figures are supported.}
  \item{\code{wmf}}{WMF figures are supported. This will work only on Windows as it relies on
    \code{win.metafile}. There is no checking whether the platform is
    actually Windows but there should be.}
  \item{\code{hex}}{This controls whether the figure is embedded into
    the RTF document as hexadecimal characters. The default is
    \code{TRUE}. If \code{FALSE} then the output file \dQuote{includes}
    the file with an \dQuote{INCLUDEPICTURE} RTF field instruction. This
    produces small files since the figures are not embedded but it
    takes some Word commands to convert the \dQuote{included} picture
    into an embedded image.}
  \item{\code{resolution}}{While not specific to \pkg{rtfSweave} this
    options is used enough often. It controls the figure resolution and
    defaults to 300 dpi.}
  \item{\code{pointsize}}{Controls pointsize for the figures.}
  \item{\code{rtf.Schunk}}{RTF \emph{paragraph} formatting commands for the R input and
    output. The default is Sweave like. Since the R input and output are
    part of the same paragraph (with lines broken by \verb{\line}), this
    options controls formatting for both input and output.}
  \item{\code{rtf.Sinput}}{RTF \emph{text} formatting commands for R
    input.}
  \item{\code{rtf.Soutput}}{RTF \emph{text} formatting commands for R output.}
  \item{\code{results}}{Set this to \code{"rtf"} if the Soutput is
    \dQuote{native} RTF code.}
  }
}


\note{ This driver is unlikely to work with documents written directly
  in Microsoft Word and saved as RTF prior to processing with
  \code{Sweave}. %%The problem is that code chunks
  %%that might look like pure R code on screen in Word will end
  The problem is that what might look like a valid code chunk on the
  screen will, once saved as RTF, end
  up with random linebreaks and a good bit of RTF markup in
  the actual text file.%% It is difficult to repair the lines and remove
  %%the extraneous markup in these code chunks before they are
  %%passed to R for parsing.}

  The Microsoft WordPad program writes cleaner RTF and it may be
  relatively straightforward to update the rtfSweave driver to remove
  the hidden RTF commands that get added to a code chunk when the file
  is saved and then parse the code chunk. In other words, it may not be
  possible to \dQuote{sanitize} a code chunk saved in Word but it
  should be pretty easy to sanitize a code chunk saved in WordPad.
}
\author{Stephen Weigand \email{Weigand.Stephen@mayo.edu} adapted the
  code of Friedrich Leisch and others}

\seealso{
  \code{\link{Sweave}}
}

\examples{
testfilepath <- system.file("examples", "rtfSweave-test-1.rtf",
                            package = "rtfSweave")
tempfilepath <- paste(tempfile(), ".rtf", sep = "")

## 'syntax' argument can be unspecified since per its help
## Sweave can find syntax objects matching pattern 'SweaveSyntax.*'
Sweave(testfilepath, driver = RweaveRtf, syntax = SweaveSyntaxRtf,
       output = tempfilepath)
       
}

\keyword{utilities}% at least one, from doc/KEYWORDS
