\name{rtfSweave-package}
\alias{rtfSweave-package}
\docType{package}
\title{
  The rtfSweave Package
}
\description{
  This package provides an add-on driver to enable Sweave to process
  documents with R code chunks and RTF documentation chunks.
}
\details{
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
  a native Microsoft Word format (e.g., \code{*.doc} or \code{*.docx}.

}

\author{Stephen Weigand \email{Weigand.Stephen@mayo.edu}}
\keyword{utilities}% at least one, from doc/KEYWORDS

  