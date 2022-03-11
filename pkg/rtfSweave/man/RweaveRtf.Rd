\name{RweaveRtf} % typically basename of the *.Rd file
\alias{RweaveRtf} %% This is the driver
%% \alias{rtfSweave-package} Now I have my own rtfSweave-package.Rd file
\alias{rtfSweave}
\alias{SweaveSyntaxRtf}
\alias{RtangleRtf}
%\alias{rtfSweaveDriver}
%\alias{rtfTangleDriver}


\title{An Sweave driver for rich text format (RTF) documents}

\description{
  An add-on driver for \code{\link{Sweave}} that translates \R code
  chunks in RTF files to form an RTF output document
}
\usage{
RweaveRtf()
RtangleRtf()
SweaveSyntaxRtf
%rtfSweaveDriver()
%rtfTangleDriver()
}

\details{

  By default \code{\link{Sweave}} uses the \code{RweaveLatex} driver to
  convert \file{*.Rnw} input files to LaTeX output files. However,
  \code{\link{Sweave}} was designed to allow authors to write their own
  drivers. The \pkg{rtfSweave} drivers documented here are essentially a
  copies of the \code{\link{RweaveLatex}} and \code{\link{RtangleLatex}}
  drivers with some necessary changes to make them work for rich text
  format (RTF) documents.

  RTF documents are plain text documents that have some similarities to
  LaTeX. For one thing, RTF documents can be opened and edited using
  Emacs or another text editor. While the RTF files written by Microsoft
  Word are difficult to read, without loss of functionality one can
  write very human-readable RTF documents which can be opened in
  Microsoft Word and then saved as a native DOC or DOCX file. One can
  write RTF documents with headings, subheadings, headers, footers,
  complex tables, figures, etc. It may be helpful to think of RTF as a
  text-based interface to nearly all of Microsoft Word's document
  formatting functionality.
  
  My motivation for creating RTF drivers for \code{\link{Sweave}} was
  that my non-statistician colleagues, collaborators, and clients almost
  exclusively use Microsoft Word.  Rather than converting a LaTeX report
  created with \code{\link{Sweave}} to RTF, I prefer to write the RTF
  myself to have full control over the output format.
  
}

\section{Options for \code{RweaveRtf}}{

  \code{RweaveRtf} supports the following options for code chunks.  If
  the option is used in the header of a code chunk (i.e., between
  \code{<<} and \code{>>=}) then options do not need to be quoted.
  Options can also be passed as arguments to \code{\link{Sweave}} and in
  this case character string values should be quoted.

  For options taking a logical value, \code{Sweave} understands upper or
  lowercase versions of \code{TRUE} and \code{FALSE} when the option is
  used in a code chunk header.
  
\subsection{Most often used options}{

  These are the most often used options that most users need to
  understand.
  
\describe{

  \item{\code{eval}}{logical defaulting to \code{TRUE}.  If
    \code{FALSE}, the code chunk is not evaluated, and hence no text nor
    graphical output is produced. Code chunks that are not evaluated are
    still parsed so the chunk needs to contain valid \R code. To have a
    code code chunk ignored entirely by \code{\link{Sweave}} set the
    \code{engine} option (described below) to something other than \code{"R"} or
    \code{"S"}. For example, \code{<<engine = foo>>=} will cause
    \code{\link{Sweave}} to skip the whole chunk altogether.}

  \item{\code{echo}}{logical defaulting to \code{TRUE} which means \R
    code in a code chunk is \dQuote{echoed} to the output.}

  \item{\code{results}}{character string defaulting to \code{"verbatim"}
    which means show the results of the \R commands as \R code.  This
    means output appears in a fixed-width font with whitespace and line
    breaks preserved.  Using \code{"rtf"} means the output is taken to
    be proper RTF markup and included as is. (This is useful for having
    \R output RTF-formatted tables which have complicated mark-up that
  is hard to write by hand.)
    Using \code{"hide"} means all output from the code chunk is
    completely suppressed (but the code is still executed by
    \code{\link{Sweave}}).  Values can be abbreviated.}

  \item{\code{fig}}{logical defaulting to \code{FALSE}. Indicates
    whether any plots crated in the code chunk produce graphical
    output. Note that only one figure per code chunk can be processed
    this way. The labels for figure chunks are used as part of the file
    names, so should preferably be alphanumeric.}

  \item{\code{png}}{logical defaulting to \code{TRUE} which means a PNG
    figure should be generated when \code{fig = TRUE}. RTF allows
    embedding PNG, JPEG, and Windows enhanced metafiles (WMF).}

  \item{\code{jpeg}}{logical defaulting to \code{FALSE}. Set to
    \code{TRUE} to generate JPEGs when \code{fig = TRUE}. However, to
    use JPEGs rather than PNGs, one needs to also have \code{png =
    FALSE} because PNGs have priority.}

  \item{\code{tiff}}{logical defaulting to \code{FALSE}. The RTF format
    does not support TIFF figures but this option allows
    \code{link{Sweave}} to write out a TIFF version of a figure. A
    possible use case is when using \pkg{rtfSweave} to output RTF files
    with embedded PNGs to form the basis of a manuscript. After the
    manuscript is revised and accepted by a journal, the journal often
    requires high-resolution TIFFs and these can be \dQuote{turned on}
    with something like \code{tiff = TRUE, resolution = 1200}.}

  \item{\code{wmf}}{logical defaulting to \code{FALSE}. Set to
    \code{TRUE} for enhanced Windows metafiles generated by
    \code{\link{win.metafile}} (which is only available on Windows).  To
    get WMF files embedded in the RTF file one needs to have \code{png =
    FALSE} and \code{wmf = TRUE} because PNGs have priority.}

  \item{\code{height}}{height of the figure in inches with a default of 6.}

  \item{\code{width}}{width of the figure in inches with a default of 6.}

  \item{\code{resolution}}{resolution of the figure with a default of
    300 dpi. Note the name of the option here is the full-length word
    \code{resolution} and does not match the argument in
    \code{\link{png}}, \code{\link{jpeg}}, etc. which use the shorter
    \code{res}.}

  \item{\code{pointsize}}{pointsize for figures defaulting to 12.}

  \item{\code{compression}}{type of comperssion for TIFF figures
    defaulting to \code{"lzw"}. See \code{\link{tiff}} for the types of
    compression available. If one is using TIFFs for uploading to a
    journal website and the TIFF is not accepted by the uploader, it may
    be worth trying \code{compression = "zip"} which may be more
    \dQuote{Adobe like}. (And if this is specified as an option in a
    code chunk header, omit the quotes.)}

  \item{\code{hex}}{logical defaulting to \code{TRUE}. This controls
    whether the PNG, JPEG, or WMF figure is embedded into the RTF
    document as hexadecimal characters. The default of \code{TRUE} means
    the RTF file is \dQuote{self-contained.} If \code{FALSE} then the
    output file \dQuote{includes} the file with an
    \dQuote{INCLUDEPICTURE} RTF field instruction. This produces small
    RTF files since the figures are not embedded but it makes the
    document harder to convert correctly to Word's \file{*.docx} format using
    \dQuote{Save As}.}

  \item{\code{prefix}}{logical defaulting to \code{TRUE}.  If
    \code{TRUE} then generated filenames of figures and output all have
    the common prefix given by the \code{prefix.string} option;
    otherwise only chunks without a label use the prefix.}

  \item{\code{prefix.string}}{a character string defaulting to the name
    of the source file (without extension).  Note that
    \code{prefix.string} is used as part of filenames, so needs to be
    \dQuote{portable} which generally means avoiding spaces and punctuation
    other than \code{-} and \code{_}.}

}% End describe
}% End subsection

\subsection{Options controlling formatting for code chunk input and output}{
\describe{
  
  \item{\code{rtf.Schunk}}{RTF \emph{paragraph} formatting commands for
    the \R input and output. The default is Sweave like. Since the \R
    input and output are part of the same RTF paragraph (with lines broken
    by \verb{\line}, i.e., hard linebreaks), this options controls
    formatting for both input and output. A good resources for RTF mark-up
    is the RTF 1.5 specification document posted at 
    \url{http://www.biblioscape.com/rtf15_spec.htm}. In particular,
    the section
    \href{http://www.biblioscape.com/rtf15_spec.htm#Heading31}{Paragraph
      Formatting Properties} is useful to change the look of the output.}
    
  %% Does this even work?
  \item{\code{rtf.Sinput}}{RTF \emph{text} formatting commands for R
    input. A good resources is
  \href{http://www.biblioscape.com/rtf15_spec.htm#Heading41}{Character
    Text}
  from the RTF 1.5 specification.}

  \item{\code{rtf.Soutput}}{RTF \emph{text} formatting commands for R output.}
} % End describe
} % End subsection
  
\subsection{Less often used options}{

\describe{

  \item{\code{print}}{logical defaulting to \code{FALSE}.  If
    \code{TRUE}, this forces auto-printing of all expressions. For
    example, if \code{print = TRUE} then the output for a single line like this
    \code{a <- runif(1); b <- runif(1); a + b} would result in printing
    the value of \code{a}, the value of \code{b} and their sum. The
    default behavior just prints the sum.}


  \item{\code{grdevice}}{character defaulting to \code{NULL}. This
    option allows the user to use custom graphics devices for figures.
    See the section called \sQuote{Custom Graphics Devices} in
    \code{\link{RweaveLatex}}.}

  
  \item{\code{term}}{logical defaulting to \code{TRUE}.  If \code{TRUE},
    visibility of values emulates an interactive \R session: values of
    assignments are not printed, values of single objects are printed.
    If \code{FALSE}, output comes only from explicit \code{\link{print}}
    or similar statements.}


  \item{\code{keep.source}}{logical defaulting to \code{TRUE} which
    means when \code{echo = TRUE} the original source code is copied to
    the file with formatting unchanged.  Otherwise, the code is read in
    by \R and then output using \R's default formatting (i.e., it is
    deparsed).}

  \item{\code{split}}{logical defaulting to false. If \code{TRUE}, text
    output is written to separate files for each code chunk.}

  \item{\code{strip.white}}{character string defaulting to \code{"true"}
    which means blank lines before and after \emph{output} are removed.
    If \code{"all"}, then all blank lines are removed from the output.
    If \code{"false"} then blank lines in the output are retained.}

  \item{\code{include}}{logical defaulting to \code{TRUE} determining
    whether figures are included in the RTF file (in a way that depends
    on the \code{hex} option). Set to \code{FALSE} to have the figure
    file created but not included in the report. (This might be useful
    if you want a figure generated by \code{\link{Sweave}} but not
    included as part of a report.)  The \code{include} option also
    has functionality outside the context of figures. If a code chunk
    has \code{split = TRUE} then the output is written to separate files
    and the option \code{include} determines whether the main RTF
    document has \dQuote{include} statements to bring in the content of
    these files. At this time these \dQuote{include} statements are not
    implemented.}

  \item{\code{concordance}}{logical defaulting to \code{FALSE}. A value
    of \code{TRUE} means a concordance file is created to link the input
    line numbers to the output line numbers. This is an experimental
    feature that is part of \code{\link{Sweave}}; see the source code
    for the output format, which is subject to change in future
    releases.}
      
  \item{\code{figs.only}}{logical defaulting to \code{FALSE}.  By
    default each figure chunk is run once, then re-run for each selected
    type of graphics.  That will open a default graphics device for the
    first figure chunk and use that device for the first evaluation of
    all subsequent chunks.  If this option is \code{TRUE}, the figure
    chunk is run only for each selected type of graphics, for which a
    new graphics device is opened and then closed.}

  \item{\code{engine}}{character string defaulting to \code{"R"}.  Only
    chunks with \code{engine} equal to \code{"R"} or \code{"S"} are
    processed by Sweave. This option can be ignored if only \R code
    chunks are used. (However, as noted above, setting \code{engine} to
    anything but \code{"R"} or \code{"S"} is a way to have
    \code{\link{Sweave}} ignore the code chunk.)}
    
} % end describe
} % end subsection
} % end section

\section{Weaving versus tangling and naming conventions}{ Documents
  written with RTF documentation chunks can be \dQuote{weaved} to form
  an RTF report or \dQuote{tangled} to extract the code chunks into an
  \R file (or into multiple \R files depending on the \code{split}
  option).  A separate driver is needed for these two tasks as
  demonstrated in the examples.

  Since \code{\link{Sweave}} provides \code{\link{RweaveLatex}} and
  \code{\link{RtangleLatex}}, the respective drivers for RTF are named
  \code{RweaveRtf()} and \code{RtangleRtf}.
    
}

\section{Warning}{

  This driver will not work with documents written directly in Microsoft
  Word and saved as RTF prior to processing with \code{\link{Sweave}}.
  The problem is that what might look like a valid code chunk on the
  screen will, once saved as RTF, end up with random linebreaks and a
  good bit of RTF markup in the actual text file.

  The Microsoft WordPad program writes cleaner RTF and it may be
  relatively straightforward to update the \pkg{rtfSweave} drivers to
  remove the hidden RTF commands that get added to a code chunk when the
  file is saved and then send that cleaned-up code chunk to \R. In other
  words, it may not be possible to \dQuote{sanitize} a code chunk saved
  in Word so that \R can understand it but it should be pretty easy to
  sanitize a code chunk saved in WordPad.
}

\author{Stephen Weigand \email{Weigand.Stephen@mayo.edu} adapted the
  code of Friedrich Leisch and others}

\seealso{
  \code{\link{Sweave}}
}

%% ## 'syntax' argument can be unspecified since per its help
%% ## Sweave can find syntax objects matching pattern 'SweaveSyntax.*'

\examples{
testfilepath <- system.file("examples", "rtfSweave-test-1.rtf",
                            package = "rtfSweave")

## 'weave' to create the document
##

Sweave(testfilepath, driver = RweaveRtf, syntax = SweaveSyntaxRtf,
       output = tempfile(fileext = ".rtf"))


## 'tangle' to extract R code chunks.
Sweave(testfilepath, driver = RtangleRtf(), syntax = SweaveSyntaxRtf,
       output = tempfile(fileext = ".R"))

       
}

\keyword{utilities}% at least one, from doc/KEYWORDS
