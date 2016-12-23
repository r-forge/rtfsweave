% Adapted version of:
% File src/library/utils/man/SweaveUtils.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Team
% Distributed under GPL 2 or later

\name{rtfSweaveUtils}
%% \alias{RtangleWritedoc}
%% \alias{RweaveChunkPrefix}
%% \alias{RweaveEvalWithOpt}
%% \alias{RweaveLatexFinish}
%% \alias{RweaveLatexOptions}
%% \alias{RweaveLatexWritedoc}
\alias{RweaveRtfFinish}
\alias{RweaveRtfOptions}
\alias{RweaveRtfWritedoc}
\alias{RweaveRtfRuncode} % I added this
\alias{RweaveRtfSetup}   % I added this
%% \alias{RweaveTryStop}
%% \alias{SweaveHooks}
%% \alias{SweaveUtils}
%% \alias{makeRweaveLatexCodeRunner}
\alias{makeRweaveRtfCodeRunner} %% Should really be documented!!!
\title{rtfSweave Driver Utilities}
\description{
  %% These functions are handy for writing Sweave drivers and currently not
  %% documented. Look at the source code of the Sweave Latex driver (in this
  %% package) or the HTML driver (in the R2HTML package from CRAN) to see
  %% how they can be used.
  These functions are components of the rtfSweave driver and currently
  not documented but they are exported. 
}
%% \usage{
%% RtangleWritedoc(object, chunk)
%% RweaveChunkPrefix(options)
%% RweaveEvalWithOpt(expr, options)
%% RweaveLatexFinish(object, error = FALSE)
%% RweaveLatexOptions(options)
%% RweaveTryStop(err, options)
%% SweaveHooks(options, run = FALSE, envir = .GlobalEnv)
%% makeRweaveLatexCodeRunner(evalFunc = RweaveEvalWithOpt)
%% }
\author{Original Sweave versions by Friedrich Leisch, with contributions
  by Seth Falcon. RTF changes by Stephen Weigand \email{Weigand.Stephen@mayo.edu}}
\seealso{The \code{\link{Sweave}}, \code{\link{RweaveLatex}} help pages.
}
\keyword{utilities}

