\documentclass{article}
\usepackage{graphicx}
\usepackage{hyperref}
\usepackage{noweb}
\noweboptions{smallcode}
\pagestyle{noweb}

\addtolength{\topmargin}{-0.5in}
\addtolength{\textheight}{0.75in}
\addtolength{\oddsidemargin}{-0.5in}
\addtolength{\textwidth}{1in}

\renewcommand{\code}[1]{\texttt{#1}}
\newcommand{\pkg}[1]{\texttt{#1}}
\newcommand{\var}[1]{\emph{#1}}

\setlength{\parindent}{0pt}
\setlength{\parskip}{1em plus 0.5em minus 0.2em}

\begin{document}
\title{RtfWeave}
\date{\today}
\author{Stephen Weigand}
\maketitle



\tableofcontents

\section{Overview}

The point of this document is to create a driver for Sweave that
allows one to process RTF files with R code chunks written in the noweb
style. Since the original driver for Sweave was called
\code{RweaveLatex} I have chosen the name.
%\code{RweaveRtf} but will
%probably rename it to \code{RweaveRTF}. I may name the package
%\code{RTFweave} or maybe \code{rtfSweave}.

This driver is simply defined as% checked against SVN on Dec. 5, 2014
<<driver>>=
RweaveRtf <- function(){
  list(setup = RweaveRtfSetup,    
       runcode = RweaveRtfRuncode,  
       writedoc = RweaveRtfWritedoc, 
       finish = RweaveRtfFinish,     
       checkopts = RweaveRtfOptions)
}
@ 
with all the work being done by the component functions.  Once the
source file combining R code and RTF markup are created, one can weave the 
file with
% the syntax arg may have problems
\begin{verbatim}
R> Sweave("myfile.Rrtf", driver = RweaveRtf(), syntax = SweaveSyntaxRtf)
\end{verbatim}

The component functions are described in the current manual that comes
with R version 2.13.0 and higher. The manual can be viewed with

\begin{verbatim}
R> vignette("Sweave", package = "utils"))
\end{verbatim}

For completeness, I reproduce section 5 of the manual here:

\itshape
\small
An Sweave driver is a function of no arguments which should return a
list of five functions:
\begin{description}
\item{\texttt{setup(file, syntax, \dots)}:} Set up the driver, e.g.{}
  open the output file.  Its return value is an object which is passed
  to the next three functions, and can be updated by the next two.
  The value should be a list, and contain a component
  \texttt{options}, a named list of the default settings for the
  options needed by \texttt{runcode}.

\item{\texttt{runcode(object, chunk, options)}:} Process a code chunk.
  Returns a possibly updated \texttt{object}.  Argument \texttt{chunk}
  is a character vector, and \texttt{options} is a options list for
  this chunk.

\item{\texttt{writedoc(object, chunk)}:} Write out a documentation
  chunk.  Returns a possibly updated \texttt{object}.

\item{\texttt{finish(object, error)}:} Finish up, or clean up if
  \texttt{error} is true.

\item{\texttt{checkopts(options)}:} Converts/validates
  \texttt{options} given as a named list of character strings.
\end{description}
\normalsize
\normalfont

My own description of the five functions goes like this.
\begin{description}
\item[\code{RweaveRtfSetup(file, syntax, \dots)}] open the output file
  and pass an object with elements informing the structure of the
  document.

\item[\code{RweaveRtfRuncode(object, chunk, options)}] Process a code
  chunk from the object. Argument \code{chunk} is a character vector
  and \code{options} is the options list for this chunk (which was
  obtained by parsing the code-chunk header via
  \code{SweaveParseOptions}.
  
\item[\code{RweaveRtfWritedoc}] It evaluates
% should this be {\Sexpr }?
  \code{\textbackslash Sexpr()} commands and writes out a documentation
  chunk.
    
\item[\code{RweaveRtfFinish}] Mainly indicate if any errors and closes
  connections (possibly one per chunk)
    
  \item[\code{RweaveRtfOptions}] Check and format options to pass to
    other functions.
\end{description}
  
The hard part is of course creating the component functions. Before we
get into that, here are some preliminaries to make this document
self contained in terms of documentation, functions, and examples.

\section{Preliminaries}

To generate the complete documentation for this file run this function. It
assumes that \code{texi2dvi4a2ps} is in your path.

<<helpers>>=
noweave <- function() {
  system("~/linux-local/noweb/noweave -delay RweaveRtf.Rnw > RweaveRtf.tex")
  tools:::texi2dvi("RweaveRtf.tex", pdf = TRUE, texi2dvi = "texi2dvi4a2ps", clean = TRUE)
}
@ 

Note that in the system command above the \code{-delay} option is what allows
one to include \LaTeX\ commands like \code{\textbackslash usepackage} 
in this source file.

To extract all the driver functions from this noweb file and
place them in \code{rtfSweave.R} and source that,
run this function.

<<helpers>>=
notangle <- function(source = TRUE, echo = TRUE) {
  system(paste("~weigand/linux-local/noweb/notangle",
               "-Rsetup -Rruncode -Rwritedoc -Rfinish -Roptions -Rdriver",
               "/people/biostat2/weigand/RTF/RweaveRtf.Rnw > rtfSweave.R",
               sep = " "))
  if(source){
    source("rtfSweave.R", echo = echo)
  }         
}
#notangle()
@ 

If you want to see the source code for Sweave, see the Subversion 
repository.

See \href{http://svn.r-project.org/R/trunk/src/library/utils/R/Sweave.R}%
{http://svn.r-project.org/R/trunk/src/library/utils/R/Sweave.R}.

And \href{http://svn.r-project.org/R/trunk/src/library/utils/R/SweaveDrivers.R}%
{http://svn.r-project.org/R/trunk/src/library/utils/R/SweaveDrivers.R}.

\section{Example files}

It helps to have example files to play with. Below I create an example
Sweave file and an example rtfWeave file to work with. They can
be tangled with this function:

<<>>=
notangleExamples <- function(){
  system(paste("~weigand/linux-local/noweb/notangle",
               "-R'Example Sweave file with a code chunk'",
               "RweaveRtf.Rnw > example1.Rnw", 
               sep = " "))
  system(paste("~weigand/linux-local/noweb/notangle",
               "-R'Example RTF file with a code chunk'",
               "RweaveRtf.Rnw > example1.Rrtf",
               sep = " "))
}
@ 

<<Example Sweave file with a code chunk>>=
\documentclass{article}
\begin{document}

\title{A Simple Sweave Document}
\maketitle

Below I add two numbers and show the result.

@<<addition>>=
a <- 2
b <- 3
print(a + b)
@@

I can round 3.14 to \Sexpr{round(pi)}.

\end{document}
@ 

This next chunk is a complete RTF file.  Notice that if one were to
create or edit this file in Emacs, then one could customize Emacs to
handle the documentation chunks using \code{rtf-mode}\footnote{A
relevant elisp function may be noweb-set-doc-mode.}. However, some
users may want to write and edit the file in Word in which case the extension
should probably be \code{.rtf}.

However, be aware that the lines in the code chunk will not show up as
separate lines if the file is opened in Microsoft Word because they do
not end in the RTF \code{\textbackslash line} command. Note also that
if one were to type this file in Word, code chunks lines would
typically be preceded by an ``invisible'' RTF control word of
\code{\textbackslah line}.


<<Example RTF file with a code chunk>>=
{\rtf1\ansi\deff0
{\fonttbl
{\f0\froman Times New Roman;}
{\f1\fswiss Arial;}
{\f2\fmodern Courier New;}
}
{\colortbl
;
\red255\green255\blue153;
\red255\green204\blue153;
}
{\info
{\title Example RTF file with a code chunk}
{\author }
{\company }
{\creatim\yr2009\mo07\dy28\hr17\min39}
}
\deflang1033\plain \ftnbj
\paperw12274
\paperh15884
\margl1444
\margr1444
\margt1444
\margb1444

{\footer\pard\qr\chpgn\par}

{\pard \sb120 \sa120 \f1 \fs32 \keepn{\b
A Simple RtfWeave Document
}\par}

{\pard \sb120 \sa120 \f0 \fs24 
Below I add two numbers and show the result.
\par}

@<<echo = TRUE>>=
a <- 2
b <- 3
print(a + b)
@@

{\pard \sb120 \sa120 \f0 \fs24 
I can round 3.14 to {\Sexpr round(pi)}.
\par}

{\pard \sb120 \sa120 \f0 \fs24
Here is normal quantile plot.
\par}


@<<fig = TRUE, echo = FALSE>>=
qqnorm(rnorm(10), pch = 21, bg = "lightblue")
@@
{\pard \sb120 \sa120 \f0 \fs24 \qc
This is not really a legend but it could be. (I have centered the
 paragraph.)
\par}

@<<fig = TRUE, echo = FALSE, width = 4, height = 4>>=
boxplot(list(A = rnorm(10), B = rnorm(10)), pch = 19, col = "lightblue")
@@
{\pard \sb120 \sa120 \f0 \fs24 \qc
{\b Figure 2}. Above are two plots.
\par}

{\pard \sb120 \sa120 \f0 \fs24
Next I show the use of the {\f2 results = rtf} option.
\par}

@<<echo = FALSE, results = rtf>>=
source("simpleRTF.R")
pard("", "A paragraph created using {\\f2 pard()}")
@@
}
@ 

As shown above, the driver consists of these five functions. 
Now I create them.



\section{\code{RweaveRtfOptions}}

This is the last function in the set of driver functions but it is the
simplest so I start with it. The function is designed to enforce
validity of options within a code chunk.  It takes a \code{list} of
options and makes sure they are in the right format, for example, by
converting \code{true} to \code{TRUE}, etc. It also has specific
checks for options that are not numeric and not logical. For example
option \code{results} has to be among \code{c("verbatim", "rtf",
  "hide")}.

The only option I added is \code{res} which is the resolution argument
to \code{png} and \code{jpeg}.\footnote{For R 2.13.0, Sweave has the
  \code{resoltion} option which I need to incorporate.}

The output is a list with any specific options for that code chunk
plus always something for the \code{results} and
\code{strip.white} options.

An important thing to know is that Sweave can identify
code-chunk headers such as \code{@<<mychunk, results = hide>>=}
and have the chunk name and any options parsed by
\code{SweaveParseOptions()}.

<<options>>=
RweaveRtfOptions <- function (options) {
  defaults <- options[[".defaults"]] # new
@ 

This first segment looks to convert \code{c}haracter to
\code{l}ogical so that \code{true} goes to \code{TRUE}. This is
used because in Sweave options following the \code{key=value} format
can have a value of \code{true} or \code{TRUE}.

<<options>>=
  ## convert a character string to logical
  c2l <- function(x) {
        ## if (is.null(x)) 
        ##     return(FALSE)
        ## else return(as.logical(toupper(as.character(x))))
      if (is.null(x)) FALSE else suppressWarnings(as.logical(x))
    }
@ 

These set the numeric options and the non-logical options. The Sweave
version has options \code{pdf.version} and \code{pdf.encoding}
which I don't need. So far I have not added any non-logical options.
Note that I have changed the \code{res} option to \code{resolution}
to match the option in the default driver. (I don't know if I like
this.)
<<options>>=
    
    ## numeric
    NUMOPTS <- c("width", "height", "resolution", "pointsize")

    ## character: largely for safety, but 'label' matters as there
    ## is no default (and someone uses "F")
    CHAROPTS <- c("results", "prefix.string", "engine", "label",
                  "strip.white", "pdf.version", "pdf.encoding", "grdevice")    ##


    #NOLOGOPTS <- c(NUMOPTS, "results", "prefix.string", "engine", 
    #    "label", "strip.white") # "pdf.version", "pdf.encoding")

@ 

This chunk basically cleans up the options so that numerics
are numeric and intended logicals are logical

<<options>>= 
    for (opt in names(options)) {
        if(opt == ".defaults") next
        oldval <- options[[opt]]
        defval <- defaults[[opt]]
        if(opt %in% CHAROPTS || is.character(defval)) {
        } else if(is.logical(defval))
            options[[opt]] <- c2l(oldval)
        else if(opt %in% NUMOPTS || is.numeric(defval))
            options[[opt]] <- as.numeric(oldval)
        else if(!is.na(newval <- c2l(oldval)))
            options[[opt]] <- newval
        else if(!is.na(newval <- suppressWarnings(as.numeric(oldval))))
            options[[opt]] <- newval
        if (is.na(options[[opt]]))
            stop(gettextf("invalid value for %s : %s", sQuote(opt), oldval),
                 domain = NA)
    }
@ 

This chunk handles the non-logical and non-numeric options. There
are only a handful of numerics in the original \code{RweaveLatexOptions} function. One new one here is 
\code{resolution}.

If the \code{results} option is \code{NULL}, then
set it to \code{verbatim}. Otherwise convert it
it to lowercase and check that it is among the three valid choices.

<<options>>= 
if (!is.null(options$results)) {
  res <- as.character(options$results)
  if(tolower(res) != res) # documented as lower-case
    warning("value of 'results' option should be lowercase",
            call. = FALSE)
  options$results <- tolower(res)
}
options$results <- match.arg(options$results, c("verbatim", "rtf", "hide"))


@ 

If the \code{strip.white} option is \code{NULL}, then set it to
\code{"true"}. Otherwise convert it to lowercase and check that
it is among the three valid choices of \code{"true"}, \code{"false"},
or \code{"all"}.

<<options>>=
## if (!is.null(options$strip.white)) 
##   options$strip.white <- 
##     tolower(as.character(options$strip.white))
if (!is.null(options$strip.white)) {
  res <- as.character(options$strip.white)
  if(tolower(res) != res)
    warning("value of 'strip.white' option should be lowercase",
            call. = FALSE)
  options$strip.white <- tolower(res)
}
options$strip.white <- 
    match.arg(options$strip.white, c("true", "false", "all"))
options
}
@

I can test this with the following. I 

<<test>>=
RweaveLatexOptions(list(echo = "true", results = "hide", fig = TRUE, height = 3))
RweaveLatexOptions(list(echo = "true", results = "HIDE", fig = T, height = 3))
RweaveLatexOptions(list(echo = "True", results = "HIDE", fig = T, height = 3))
RweaveLatexOptions(list(fig = "true", jpeg = TRUE, resolution = 125, png = "truE"))
RweaveRtfOptions(list(fig = "true", jpeg = TRUE, res = 125))
RweaveRtfOptions(list(fig = TRUE, jpeg = TRUE, height = "b"))
RweaveRtfOptions(list(fig = TRUE, jpeg = TRUE, height = 3))
@ 


\section{\code{RweaveRtfSetup}}

The function \code{RweaveRtfSetup} relies on a syntax object. I define
and document the syntax object here. Note that to catch a string
of the form \code{\textbackslash blah} I need four backslashes 
since R escapes each backslash so I want to catch a pattern like \code{\textbackslash\textbackslash}
and to do that I have to escape each one. Thus four in total.

Here is the syntax object. The SVN repository says not to escape the right curly bracket
within the ``[]'' so I undid that. I am now also allowing specifying an RTF-like
syntax to take advantage of \code{\textbackslash SweaveInput} which will
insert the contents of the specified file.
%% for ``plain'' RTF that one writes
%% in Emacs or another editor.

<<setup>>=
## NB: } should not be escaped in [] .
## Checked versus https://svn.r-project.org/R/trunk/src/library/utils/R/Sweave.R on Dec. 5, 2014
SweaveSyntaxRtf <- 
  list(doc = "^@",
       code = "^@<<(.*)>>=.*",     # NOTE: extra at sign here to escape dbl angle brackets for noweb processing
       coderef = "^@<<(.*)>>.*",   # NOTE: extra at sign here to escape dbl angle brackets for noweb processing
       docopt = "^[[:space:]]*\\{\\\\SweaveOpts ([^}]*)\\}",       ## twas [^\\}]*
       docexpr = "\\{\\\\Sexpr ([^}]*)\\}", # ... any valid S code, only curly brackets are not allowed ## twas [^\\}]*
       extension = "\\.[rsRS]?rtf$",      # i.e., .rrtf, .srtf, .Rrtf, .Srtf.
       syntaxname = "^[[:space:]]*\\{\\\\SweaveSyntax ([^}]*)\\}", ## twas [^\\}]*
       input = "^[[:space:]]*\\{\\\\SweaveInput ([^}]*)\\}",        ## twas [^\\}]*
       trans = 
       list(doc = "@",                                             ## was "^@"                                       
            code = "^@<<\\1>>=",   # NOTE: extra at sign here to escape dbl angle brackets for noweb processing
            coderef = "^@<<\\1>>", # NOTE: extra at sign here to escape dbl angle brackets for noweb processing
            docopt = "{\\\\SweaveOpts \\1}",                     
            docexpr = "{\\\\Sexpr \\1}", 
            extension = ".Rrtf",
            syntaxname = "{\\\\SweaveSyntax SweaveSyntaxRtf}",   
            input = "{\\\\SweaveInput \\1}"))                    
class(SweaveSyntaxRtf) <- "SweaveSyntax"
@ 

This next version reflects the type of RTF that MS Word 2003 SP 3
writes. The key difference is that one has to deal with
\code{\textbackslash par} at the start of a line.

A \emph{very important} point is that \code{Sweave} has this line

\begin{verbatim}
chunkopts <- sub(syntax$code, "\\1", line) 
\end{verbatim}
which is hard-coding the relevant back reference. So this limits the patterns
that can be used in \code{SweaveSyntaxRtfMS\$code}

<<>>=
SweaveSyntaxRtfMS <- 
  list(doc = "^\\\\par .*?@", #catch '\\par @' | '\\par @                                        
       ## Following line is fine iff I run clean_MSWord_rtf on the file first.
       ## code = "^\\\\par @<<(.*)>>=.*",     # NOTE: extra at sign here to escape dbl angle brackets for noweb processing

       ## This is what I am trying as a way to avoid 'cleaning' the source file first. 
       code = "^\\\\par .*@<<(.*)>>=.*",
       coderef = "^\\\\par @<<(.*)>>.*",   # NOTE: extra at sign here to escape dbl angle brackets for noweb processing
       docopt = "\\{\\\\SweaveOpts ([^\\}]*)\\}",      
       ## "\\{\\\\Sexpr ([^\\}]*)\\}",
       ## Looks like this: \\Sexpr\{coef(fit)[1]\}
       ## Works but not right choice: docexpr = "\\\\\\\\Sexpr\\\\\\{([^\\}]*)\\\\\\}",
       ##
       ## Looks like this: Sexpr\{blah\}
       ## docexpr = "Sexpr\\\\\\{([^(\\\\\\}]*)\\\\\\}", # Works for typing Sexpr{blah} which Word escapes but that approach induces line breaks and smart tags best to avoid curly brackets
       docexpr = "Sexpr\\|([^\\|]*)\\|", # Works when targeting for Sexpr|blah| but line breaks sink me.
       extension = "\\.[rsRS]?rtf$",      # i.e., .rrtf, .srtf, .Rrtf, .Srtf.
       syntaxname = "\\{\\\\SweaveSyntax ([^\\}]*)\\}", 
       input = "^[[:space:]]*\\\\SweaveInput\\{([^\\}]*)\\}", 
       trans = 
       list(doc = "^@",                                       
            code = "^@<<\\2>>=",              # NOTE: extra at sign here to escape dbl angle brackets for noweb processing
            coderef = "^@<<\\2>>",            # NOTE: extra at sign here to escape dbl angle brackets for noweb processing                 

            docopt = "\\\\SweaveOpts{\\1}",                   
            ## docexpr = "Sexpr\\\\\\{\\1}", # Works for Sexpr{blah}
            docexpr = "Sexpr\\|\\1|", # Works for Sexpr|blah| but still line breaks the source file kill me.
            extension = ".Rrtf",
            syntaxname = "{\\\\SweaveSyntax SweaveSyntaxRtf}", 
            input = "\\\\SweaveInput{\\1}"))                 
class(SweaveSyntaxRtfMS) <- "SweaveSyntax"
@

Where \code{doc}, \code{code}, and \code{coderef} are part of the
Noweb syntax. Element \code{docopt} is the regular expression defining
code chunk options for all subsequent chunks; \code{docexpr} is the
regular expression defining when Sweave substitutes R expressions for
results within a documentation chunk; element \code{extension} defines
valid file extensions; element \code{syntaxname} is the regular
expression to change the syntax definition within the document (rather
than that specified in the call to \code{Sweave}); \code{trans} is
the so-called translation table.

I don't know what \code{input} does.

The function \code{RweaveRtfSetup} takes a noweb-style RTF file and a syntax specification
(among other things) and returns a list whose elements are the
following:
\begin{description}
\item[\code{output}] an object of class \code{c("file", "connection")} which is opened
\item[\code{haveconcordance}, \code{debug}, \code{quiet}] logicals
\item[\code{syntax}] an object of class \code{SweaveSyntax}. One
  such object is \code{SweaveSyntaxNoweb}. % Not currently classed as such!!!
\item[\code{options}] A list of options
\item[\code{chunkout}] Seems to be an empty list. Maybe not when using \code{split = TRUE}?
\item[\code{srclines}] Not sure.
\item[\code{srcfile}] An object of class \code{srcfile}.
\end{description}


I have more options specified in the file. The SVN code uses dots for some. For some reason 'hex' is
an option picked up by \code{...}.
<<setup>>=
## SVN difference: eval is now in the options list and not named here
RweaveRtfSetup <-
  function (file, syntax, 
            output = NULL, quiet = FALSE, debug = FALSE, echo = TRUE, 
            eval = TRUE, keep.source = FALSE, split = FALSE, 
            jpeg = FALSE, 
            png = TRUE, 
            wmf = FALSE,
            res = 100, 
            ## Do not want these in the code. They were to try to clean up MS word RTF litter
            ## unrtf.code.header = FALSE,
            ## unrtf.code.chunks = FALSE,
            ...) {

            dots <- list(...)
@ 


This chunk sets the output file name to \code{*.rtf} if it is not
otherwise specified.

<<setup>>= 
    if (is.null(output)) {
        prefix.string <- basename(sub(syntax$extension, "", file))
        output <- paste(prefix.string, "rtf", sep = ".")
    } else prefix.string <- basename(sub("\\.rtf$", "", output))
@ 

The \code{quiet} argument determines whether progress messages are
shown. No alterations from \code{RweaveLatexSetup}.

<<setup>>= 
    if (!quiet) 
        cat("Writing to file ", output, "\n", "Processing code chunks with options ...\n", 
            sep = "")
    encoding <- attr(file, "encoding")
    if (encoding %in% c("ASCII", "bytes")) encoding <- ""
    output <- file(output, open = "w", encoding = encoding)
    ## OLD: output <- file(output, open = "w+")
@ 

This section is commented out because it is \LaTeX\ specific.

<<setup>>= 
##     if (missing(stylepath)) {
##         p <- as.vector(Sys.getenv("SWEAVE_STYLEPATH_DEFAULT"))
##         stylepath <- if (length(p) >= 1 && nzchar(p[1])) 
##             identical(p, "TRUE")
##         else TRUE
##     }
##     if (stylepath) {
##         styfile <- file.path(R.home("share"), "texmf", "Sweave")
##         if (.Platform$OS.type == "windows") 
##             styfile <- gsub("\\\\", "/", styfile)
##         if (length(grep(" ", styfile))) 
##             warning(gettextf("path to '%s' contains spaces,\n", 
##                 styfile), gettext("this may cause problems when running LaTeX"), 
##                 domain = NA)
##     }
##     else styfile <- "Sweave"
@ 

Some of these arguments are documented in \code{help(RweaveLatex)}. I have
commented out the ones that are not used and added my new ones. A new one
is \code{figs.only} which is undocumented.

<<setup>>= 

    options <- list(prefix = TRUE, prefix.string = prefix.string, 
                    engine = "R", print = FALSE, eval = eval, fig = FALSE, 
                    ## pdf = pdf, eps = eps,
                    jpeg = FALSE,       # New
                    png = TRUE,         # New
                    wmf = FALSE,        # New
                    grdevice = "",      ## Allows for a custom device.
                    resolution = 300,          # New default, now called 'resolution' not res
                    pointsize = 12,     # New using the default for grDevices::png
                    hex = TRUE,         # New
                    width = 6, height = 6, term = TRUE, 
                    echo = echo, keep.source = keep.source, results = "verbatim", 
                    split = split, strip.white = "true", include = TRUE, 
                    ##pdf.version = "1.1", pdf.encoding = "default",
                    ## unrtf.code.chunks = FALSE, #unrtf.code.chunks,
                    expand = TRUE,
                    concordance = FALSE, figs.only = TRUE) #New 
    options[names(dots)] <- dots

@ 

The function \code{RweaveRtfOptions} is documented above. This first line
here just checks the options and sets the \code{results} and \code{strip.white}
options if they are not set. After this I return the list.

<<setup>>= 
    ## to be on the safe side: see if defaults pass the check
    options <- RweaveRtfOptions(options)

    ## I may need to take RTF control words out of the code chunk header.
    ## Will they be on separate lines? They can't be. If they were, the
    ## pattern "@<< stuff >>=" would never be found.
    ## if(unrtf.code.header){
    ##   orig.lines <- readLines(file)
    ##   code.start.lines <- grep(syntax$code, orig.lines) # where are the code chunks starting
    ## 
    ##   message("Performing UnRTF on ", file)
    ##   clean.lines <- system(paste("/projects/bsi/neuro/s101846.adir/local/bin/unrtf",
    ##                               "--text", shQuote(file)),
    ##                          intern = TRUE)
    ##   print(clean.lines)
    ##   message("these are the cleaned code header lines")
    ##   print(grep(syntax$code, clean.lines))
    ##   orig.lines[code.start.lines] <- grep("^@<<(.*)>>=.*", clean.lines, value = TRUE) # at sign for noweb
    ##   temp.rtf <- tempfile(fileext = ".rtf")
    ##   message("UnRTF'd file is ", temp.rtf)
    ##   writeLines(orig.lines, temp.rtf)
    ##   file <- temp.rtf
    ## }

    list(output = output, #styfile = styfile, havesty = FALSE, 
        haveconcordance = FALSE, debug = debug, quiet = quiet, 
        syntax = syntax, options = options, chunkout = list(), 
        ## In looking at SVN on Dec. 5, 2014 I see this changed.
        ## srclines = integer(0L), srcfile = srcfile(file))
        srclines = integer()) 
}

@ 

\subsection{Testing \code{RweaveRtfSetup}}

Here I show what the function does with a known example. Recall that
the syntax object \code{SweaveSyntaxNoweb} is defining code chunks
as beginning with the double left angle brackets.

<<>>=
tmp1 <- RweaveLatexSetup("example1.Rnw", syntax = SweaveSyntaxNoweb)
names(tmp1)
str(tmp1)
readLines("example1.tex")

tmp2 <- RweaveRtfSetup("example1.Rrtf", syntax = SweaveSyntaxRtf)
str(tmp2)
readLines("example1.rtf")
@ 


\section{\code{RweaveRtfRuncode}}

This is the \code{runcode} function and is the longest. I 
currently don't know what it does exactly beyond processing
chunks.

<<runcode>>=
RweaveRtfRuncode <- function (object, chunk, options) {

if (!(options$engine %in% c("R", "S"))) {
  return(object)
}
@ 

This chunk ouputs messages along the lines of 

\begin{verbatim}
Processing code chunks ...
 1 : echo term verbatim (label=addition)
\end{verbatim}

<<runcode>>= 
if (!object$quiet) {
  cat(formatC(options$chunknr, width = 2), ":")
  if (options$echo) 
    cat(" echo")
  if (options$keep.source) 
    cat(" keep.source")
  if (options$eval) {
    if (options$print) 
      cat(" print")
    if (options$term) 
      cat(" term")
    cat("", options$results)
    if (options$fig) {
      if (options$png) 
        cat(" png")
      if (options$jpeg) 
        cat(" jpeg") 
      if (options$wmf)
        cat(" wmf")

    }
  }
  if (!is.null(options$label)) 
    cat(" (label=", options$label, ")", sep = "")
  cat("\n")
}
@ 

The following will create a string identifying the chunk as, e.g., 
\code{"example-001"}, \code{"example-002"}, etc.

<<runcode>>=
    chunkprefix <- utils::RweaveChunkPrefix(options)
@ 

If splitting the code chunks so that they are distributed over
different files, then do this.

<<runcode>>=
if (options$split) {
  chunkout <- object$chunkout[chunkprefix][[1]]
  if (is.null(chunkout)) {
    chunkout <- file(paste(chunkprefix, "rtf", sep = "."), "w")
    if (!is.null(options$label)) 
      object$chunkout[[chunkprefix]] <- chunkout
  }
}
else chunkout <- object$output
@ 


Because Word's RTF uses things like smart quotes I want to replace
them in a code chunk.
<<runcode>>=
saveopts <- options(keep.source = options$keep.source)
on.exit(options(saveopts))
SweaveHooks(options, run = TRUE)
## message("Here is the chunk before gsub()")
unrtf.code.chunks <- FALSE
if(unrtf.code.chunks){
message("Here is the chunk before unrtf")
print(chunk)
##print(sub("^\\\\par ", "", chunk, perl = TRUE)) # Problem is the source lines may have '\\par' at the start
## chunkexps <- try(parse(text = gsub("^\\\\par ", "", chunk)), silent = TRUE)

## I do this to make it clear to the reader what the pattern and replacements are.

## WHat is wrong here? First the Word-generated RTF will muck up the lines that *should* 
## start double angle bracks and thus code chunks aren't recognized. 
## I could try to fix this with the syntax object. 

## control.words <- 
##   c("\\}\\{\\\\rtlch\\\\fcs1 \\\\af[02] \\\\ltrch\\\\fcs0  ", "", # evil markup! (leaving off  (\\\\f\\d)? for now
##     ## "^\\\\par ", "",
##     "\\\\'93", "\"", # left smart quote -> plain quote
##     "\\\\'94", "\"", # right smart quote -> plain quote
##     "\\\\endash ", "-",
##     "\\\\emdash ", "-",
##     "\\\\insrsid\\d{6,8} ?", "", # don't take out spaces
##     "\\\\delrsid\\d{7,8} ?", "",
##     "\\\\charrsid\\d{7,8} ?", "",
##     "\\\\sectrsid\\d{7,8} ?", "",
##     "\\\\pararsid\\d{7,8} ?", "",
##     "\\\\tblrrsid\\d{7,8} ?", "",
##     "\\\\[a-z]+[0-9]*", "", # any other bothersome RTF control word that got put in there
##     "\\}  *\\{", "  ")
## if(length(control.words) %% 2)
##    stop("Problem with ", sQuote("control.words"))

## pats <- rep(c(TRUE, FALSE), length(control.words)/2)
## mypattern <- control.words[pats]
## myreplace <- control.words[!pats]
##                
## for(i in 1:length(chunk)) {
##   for(m in 1:length(mypattern)){
##     chunk[i] <- gsub(mypattern[m], myreplace[m], chunk[i], perl = TRUE)
##   }
## }
temp.rtf <- tempfile(fileext = ".rtf")

writeLines(chunk, temp.rtf)

## UnRTF version 21.2 is harder to customize. For example,
## I don't know where to tell it that \\'93 and \\'94 s/b converted to \". 
## This is easy in version 20.4 which has the file './src/text.c' with all
## the conversions.
## myargs <- "-P /people/biostat2/weigand/RTF/ -t text"
## chunk <- system2("/people/biostat2/weigand/linux-local/bin/unrtf",
##                  stdout = TRUE,
##                  args = paste("--text", temp.rtf))

chunk <- system2("/projects/bsi/neuro/s101846.adir/local/bin/unrtf",
                 args = paste("--text", temp.rtf),
                 stdout = TRUE)

## message("Here is the chunk after gsub()")
message("Here is the chunk after unrtf")
print(chunk)
}
chunkexps <- try(parse(text = chunk), silent = TRUE)
RweaveTryStop(chunkexps, options)
openSinput <- FALSE
openSchunk <- FALSE
if (length(chunkexps) == 0) 
  return(object)
srclines <- attr(chunk, "srclines")
linesout <- integer(0)
srcline <- srclines[1]
srcrefs <- attr(chunkexps, "srcref")
if (options$expand) 
  lastshown <- 0
else lastshown <- srcline - 1
thisline <- 0
@ 

<<runcode>>=
for (nce in 1:length(chunkexps)) {
  ce <- chunkexps[[nce]]
  if (nce <= length(srcrefs) && !is.null(srcref <- srcrefs[[nce]])) {
    if (options$expand) {
      srcfile <- attr(srcref, "srcfile")
      showfrom <- srcref[1]
      showto <- srcref[3]
    }
    else {
      srcfile <+- object$srcfile
      showfrom <- srclines[srcref[1]]
      showto <- srclines[srcref[3]]
    }
    dce <- getSrcLines(srcfile, lastshown + 1, showto)
    leading <- showfrom - lastshown
    lastshown <- showto
    srcline <- srclines[srcref[3]]
    while (length(dce) && length(grep("^[[:blank:]]*$", dce[1]))) {
      dce <- dce[-1]
      leading <- leading - 1
    }
  }
  else {
    dce <- deparse(ce, width.cutoff = 0.75 * getOption("width"))
    leading <- 1
  }
  if (object$debug) 
    cat("\nRnw> ", paste(dce, collapse = "\n+  "), "\n")
  if (options$echo && length(dce)) {
    if (!openSinput) {
      if (!openSchunk) {
        ##cat("\\begin{Schunk}\n", file = chunkout, append = TRUE)
        ## This spits out code chunks with font number 2, but Word has 
        cat("{\\pard \\b0 \\ql \\sb120 \\sa120 \\f2 \\fs20\n", file = chunkout, append = TRUE)
        linesout[thisline + 1] <- srcline
        thisline <- thisline + 1
        openSchunk <- TRUE
      }
      ## cat("\\begin{Sinput}", file = chunkout, append = TRUE)
      cat("{\\i", file = chunkout, append = TRUE)
      openSinput <- TRUE
    }
    ## Now emit the R code; rtf lines break with "\\line"
    cat("\n", paste(getOption("prompt"), dce[1:leading], "\\line",
                    sep = "", collapse = "\n"), file = chunkout, 
        append = TRUE, sep = "")
    if (length(dce) > leading) 
      cat("\n", paste(getOption("continue"), dce[-(1:leading)], "\\line",
                      sep = "", collapse = "\n"), file = chunkout, 
          append = TRUE, sep = "") 
    linesout[thisline + 1:length(dce)] <- srcline
    thisline <- thisline + length(dce)
  }
  tmpcon <- file() # so-called "anonymous" connection?
  sink(file = tmpcon) # divert the output to a connection
  err <- NULL
  if (options$eval) 
    err <- RweaveEvalWithOpt(ce, options) #was: err <- evalFunc(ce, options)
  cat("\n") # blank line after code chunk?
  sink() # ends the last diversion
  output <- readLines(tmpcon) # read in the output that was sink'ed
  close(tmpcon) # already got output so we close connection
  if (length(output) == 1 & output[1] == "") 
    output <- NULL
  RweaveTryStop(err, options)
  if (object$debug) 
    cat(paste(output, collapse = "\n"))
  ## Typically this is the case
  if (length(output) > 0 & (options$results != "hide")) {
    if (openSinput) {
      cat("\n}\n", file = chunkout, append = TRUE) # end italic
      linesout[thisline + 1:2] <- srcline
      thisline <- thisline + 2
      openSinput <- FALSE
    }
@

<<runcode>>=
if (options$results == "verbatim") {
  if (!openSchunk) {
    ## cat("\\begin{Schunk}\n", file = chunkout, append = TRUE)
    cat("{\\pard \\b0 \\ql \\sb120 \\sa120 \\f2 \\fs20\n", file = chunkout, append = TRUE)
    linesout[thisline + 1] <- srcline
    thisline <- thisline + 1
    openSchunk <- TRUE
  }
  ##cat("\\begin{Soutput}\n", file = chunkout, append = TRUE)
  cat("\\line\n", file = chunkout, append = TRUE) # nothing special
  linesout[thisline + 1] <- srcline
  thisline <- thisline + 1
}
## If the option is 'rtf' then the output is assumed proper markup and we don't need
## the special line breaks.
output <- paste(output, collapse = ifelse(options$results == "rtf", "\n", "\\line\n")) #these are the results
@ 

<<runcode>>=
if (options$strip.white %in% c("all", "true")) {
  output <- sub("^[[:space:]]*\n", "", output)
  output <- sub("\n[[:space:]]*$", "", output)
  if (options$strip.white == "all") 
    output <- sub("\n[[:space:]]*\n", "\n", output)
}
cat(output, file = chunkout, append = TRUE)
count <- sum(strsplit(output, NULL)[[1]] == "\n")
            if (count > 0) {
              linesout[thisline + 1:count] <- srcline
              thisline <- thisline + count
            }
            remove(output)
            if (options$results == "verbatim") {
                # cat("\n\\end{Soutput}\n", file = chunkout, append = TRUE)
                cat("\n", file = chunkout, append = TRUE)
                linesout[thisline + 1:2] <- srcline
                thisline <- thisline + 2
            }
        }
    }
    if (openSinput) {
        cat("\n}\n", file = chunkout, append = TRUE) # end italic
        linesout[thisline + 1:2] <- srcline
        thisline <- thisline + 2
    }
    if (openSchunk) {
        # cat("\\end{Schunk}\n", file = chunkout, append = TRUE)
        cat("\\par}\n", file = chunkout, append = TRUE) #end the chunk
        linesout[thisline + 1] <- srcline
        thisline <- thisline + 1
    }
    if (is.null(options$label) & options$split) 
        close(chunkout)
    if (options$split & options$include) {
        cat("\\input{", chunkprefix, "}\n", sep = "", file = object$output, 
            append = TRUE)
        linesout[thisline + 1] <- srcline
        thisline <- thisline + 1
    }
@  

This creates the PNG or JPEGs for chunks that have \code{fig=TRUE}.

<<runcode>>=
if (options$fig && options$eval) {
  if (options$png) {
    grDevices::png(file = paste(chunkprefix, "png", sep = "."), 
                   width = options$width, height = options$height, 
                   pointsize = options$pointsize,
                   units = "in", res = options$res)
    err <- try({
      SweaveHooks(options, run = TRUE)
      eval(chunkexps, envir = .GlobalEnv)
    })
    grDevices::dev.off()
    if (inherits(err, "try-error")) 
      stop(err)
  }
  if (options$jpeg) {
    grDevices::jpeg(file = paste(chunkprefix, "jpeg", sep = "."), 
                    width = options$width, height = options$height, 
                    pointsize = options$pointsize,
                    units = "in", res = options$res)
            err <- try({
              SweaveHooks(options, run = TRUE)
              eval(chunkexps, envir = .GlobalEnv)
            })
    grDevices::dev.off()
    if (inherits(err, "try-error")) 
      stop(err)
  }
  if (options$wmf) {
    grDevices::win.metafile(file = paste(chunkprefix, "wmf", sep = "."), 
                    width = options$width, height = options$height, 
                    pointsize = options$pointsize)
                    ## units = "in", res = options$res)
            err <- try({
              SweaveHooks(options, run = TRUE)
              eval(chunkexps, envir = .GlobalEnv)
            })
    grDevices::dev.off()
    if (inherits(err, "try-error")) 
      stop(err)
  }
@ 

It's here that I include the graphics. The canonical way to
include graphics in an RTF file is to use the the \code{\textbackslash pict} tag
with \code{\textbackslash pngblib},
\code{\textbackslash jpegblip}, or \code{\textbackslash emfblip} followed
by a hex representation of the image binary. This would look like this.

\begin{verbatim}
{\pict\pngblip
89504e470d0a1a0a0000000d49484452000001e0000001e008030000004a
0a4ea700000006504c5445000000ffffffa5d99fdd00000bc94944415478
9ced9d099683201005f1fe979e977143a30694a5fb5bf566cba82d526111
...
102c0e82c541b0380816e70fd8c7757b0a9896b60000000049454e44ae42
6082}
\end{verbatim}

This could be placed between \code{\textbackslash pard} and
\code{\textbackslash par} to make the picture its own
paragraph.

A second method is with what are essentially file references
as described in the RTF book. It looks like this

\begin{verbatim}
{\field\fldedit{\*\fldinst { INCLUDEPICTURE \\d
myfile.png
\\* MERGEFORMATINET }}{\fldrslt { }}}
\end{verbatim}

The problem with the first method is that on my Windows machine
it takes a huge amount of time to \emph{append} a character representation
of a bitmap file to the RTF output file. (It's quick to just
write it to a text file but slow to append it.)

The way around this is to use \code{sink} rather than \code{cat} with
\code{append = TRUE}. 

The problem with the second method is that while it's very quick,
the resulting RTF file doesn't have the images embedded in them.
This is fine except that when saving the RTF file to a Word format,
an extra step has to be performed. And then the images end up unexpectedly large anyway.

<<runcode>>=
if (options$include) {
  if (options$png) {
    imagefilename <- paste(chunkprefix, "png", sep = ".")
    if(options$hex) {
      size <- file.info(paste(chunkprefix, "png", sep = "."))$size
      hex <- readBin(paste(chunkprefix, "png", sep = "."), what = "raw", size)
      cat("\n{\\pict\\pngblip\n", file = object$output, append = TRUE)
      ## New approach to avoid cat which is slow on my Windows machine
      sink(object$output, append = TRUE)
      writeLines(as.character(hex))
      sink()
      cat("}\n", file = object$output, append = TRUE)
    }
    else {
      old.op <- options(useFancyQuotes = FALSE)
      cat("{\\field\\fldedit{\\*\\fldinst { INCLUDEPICTURE \\\\d",
          dQuote(imagefilename),
          "\\\\* MERGEFORMATINET }}{\\fldrslt { }}}",
          file = object$output,
          sep = "\n",
          append = TRUE)
      options(old.op)
    }
  }
  if (options$jpeg) { 
    imagefilename <- paste(chunkprefix, "jpeg", sep = ".")
    if(options$hex){
      size <- file.info(imagefilename)$size
      hex <- readBin(imagefilename, what = "raw", size)
      cat("\n{\\pict\\jpegblip\n", file = object$output, append = TRUE)
      ## New approach to avoid cat which is slow on my Windows machine
      sink(object$output, append = TRUE)
      writeLines(as.character(hex))
      sink()
      cat("}\n", file = object$output, append = TRUE)
    }
    else {
      old.op <- options(useFancyQuotes = FALSE)
      cat("{\\field\\fldedit{\\*\\fldinst { INCLUDEPICTURE \\\\d",
          dQuote(imagefilename),
          "\\\\* MERGEFORMATINET }}{\\fldrslt { }}}",
          file = object$output,
          sep = "\n",
          append = TRUE)
      options(old.op)
    }
  }  
  if (options$wmf) { 
    imagefilename <- paste(chunkprefix, "wmf", sep = ".")
    if(options$hex) {
      size <- file.info(imagefilename)$size
      hex <- readBin(imagefilename, what = "raw", size)
      cat("\n{\\pict\\emfblip\n", file = object$output, append = TRUE)
      ## New approach to avoid cat which is slow on my Windows machine
      sink(object$output, append = TRUE)
      writeLines(as.character(hex))
      sink()
      cat("}\n", file = object$output, append = TRUE)
    }
    else {
      old.op <- options(useFancyQuotes = FALSE)
      cat("{\\field\\fldedit{\\*\\fldinst { INCLUDEPICTURE \\\\d",
          dQuote(imagefilename),
          "\\\\* MERGEFORMATINET }}{\\fldrslt { }}}",
          file = object$output,
          sep = "\n",
          append = TRUE)
      options(old.op)
    }
  }
  linesout[thisline + 1] <- srcline
  thisline <- thisline + 1
}
}
object$linesout <- c(object$linesout, linesout)
return(object)
}
@ 

\section{\code{RweaveRtfWritedoc} function}

This is the \code{writedoc} function. It's main job is
to evaluate \code{\textbackslash Sexpr} commands.

<<writedoc>>=
RweaveRtfWritedoc <- function(object, chunk) { #what's object, what's chunk??
    linesout <- attr(chunk, "srclines")
@ 

I comment this segment out because it doesn't apply.
This segment checks to see if
the source code has the \code{\textbackslash usepackage\{Sweave\}}
command
If not, it inserts it in the document chunk where it belongs. But
RTF doesn't have a \code{\textbackslash usepackage} equivalent.
(In the RweaveHTML driver an analogous segment searches for
the string \code{text/css} and if it's not there
puts a pointer to the CSS stylesheet.

At a later stage, I can use this segment to insert the
document formatting boilerplate for an RTF document.

<<writedoc>>=
## if (length(grep("\\usepackage[^\\}]*Sweave.*\\}", chunk))) 
##   object$havesty <- TRUE
## if (!object$havesty) {
##   begindoc <- "^[[:space:]]*\\\\begin\\{document\\}" # see comment above
##   which <- grep(begindoc, chunk)
##   if (length(which)) {
##     chunk[which] <- sub(begindoc, paste("\\\\usepackage{", 
##                 object$styfile, "}\n\\\\begin{document}", sep = ""), 
##                 chunk[which])
##             linesout <- linesout[c(1:which, which, seq(from = which + 
##                 1, length.out = length(linesout) - which))]
##             object$havesty <- TRUE
##         }
##     }
@ 

Look for \code{\{\textbackslash Sexpr\}} and evaluate
those parts. If the chunk is not being evaluated
then just format it with \code{\textbackslash f2} which is usually
the fixed font.

<<writedoc>>=
while (length(pos <- grep(object$syntax$docexpr, chunk))) {
  cmdloc <- regexpr(object$syntax$docexpr, chunk[pos[1L]])
  cmd <- substr(chunk[pos[1L]], cmdloc, 
                cmdloc + attr(cmdloc, "match.length") - 1L)
  cmd <- sub(object$syntax$docexpr, "\\1", cmd)
  if (object$options$eval) {
    ## Review of SVN on Dec. 5 shows this change  
    ## val <- as.character(eval(parse(text = cmd), envir = .GlobalEnv))
    ## if (length(val) == 0) 
    ##   val <- ""
    val <- tryCatch(as.character(eval(parse(text = cmd), envir = .GlobalEnv)),
		    error = function(e) {
	               filenum <- attr(chunk, "srcFilenum")[pos[1L]]
                       filename <- attr(chunk, "srcFilenames")[filenum]
                       location <- paste0(basename(filename), ":", attr(chunk, "srclines")[pos[1L]])
		       stop("at ",location, ", ", conditionMessage(e), call. = FALSE)
		   })
            ## protect against character(), because sub() will fail
            if (length(val) == 0L) val <- ""
        }
  }
  else val <- paste("{\\f2 <<", # keep line break!! Otherwise noweb requires
                    cmd,        # '@' prior to paired angle brackets here
                    ">>}", sep = "")
  chunk[pos[1]] <- sub(object$syntax$docexpr, val, chunk[pos[1]])
}
@ 

To be compatible with noweb, 
Sweave allows options to be set using the \LaTeX\ command
\code{\textbackslash SweaveOpts} as an alternative to 
putting the options inside the angle brackets at the start
of the chunk. This segment processes those if they show up in
a chunk.

% I don't currently have need for that so comment them out.
% Now I actually see how I need this for example to set 
% options for figures "globally"
I used to have this commented out. I keep my commented out part
but insert from the SVN version of SweaveDrivers.R.

<<writedoc>>=
## while (length(pos <- grep(object$syntax$docopt, chunk))) {
##   opts <- sub(paste(".*", object$syntax$docopt, ".*", sep = ""), 
##               "\\1", chunk[pos[1]])
##   object$options <- SweaveParseOptions(opts, object$options, 
##                                        RweaveLatexOptions)
##   if (isTRUE(object$options$concordance) && !object$haveconcordance) {
##     savelabel <- object$options$label
##     object$options$label <- "concordance"
##     prefix <- RweaveChunkPrefix(object$options)
##     object$options$label <- savelabel
##     object$concordfile <- paste(prefix, "tex", sep = ".")
##     chunk[pos[1]] <- sub(object$syntax$docopt, paste("\\\\input{", 
##                                                      prefix, "}", sep = ""), chunk[pos[1]])
##     object$haveconcordance <- TRUE
##   }
##   else chunk[pos[1]] <- sub(object$syntax$docopt, "", chunk[pos[1]])
## }
## Process \SweaveOpts{} or similar
    ## Since they are only supposed to affect code chunks, it is OK
    ## to process all such in a doc chunk at once.
while(length(pos <- grep(object$syntax$docopt, chunk)))
    {
        opts <- sub(paste0(".*", object$syntax$docopt, ".*"),
                    "\\1", chunk[pos[1L]])
        object$options <- SweaveParseOptions(opts, object$options,
                                             RweaveLatexOptions)
        
        if (isTRUE(object$options$concordance)
            && !object$haveconcordance) {
            savelabel <- object$options$label
            object$options$label <- "concordance"
            prefix <- RweaveChunkPrefix(object$options)
            object$options$label <- savelabel
            object$concordfile <- paste(prefix, "tex", sep = ".")
            chunk[pos[1L]] <- sub(object$syntax$docopt,
                                  paste0("\\\\input{", prefix, "}"),
                                  chunk[pos[1L]])
            object$haveconcordance <- TRUE
        } else
            chunk[pos[1L]] <- sub(object$syntax$docopt, "", chunk[pos[1L]])
    }

@ 

The line below was changed to match the SVN.
<<writedoc>>=
cat(chunk, sep = "\n", file = object$output)#, append = TRUE)
object$linesout <- c(object$linesout, linesout)
object$filenumout <- c(object$filenumout, filenumout)
return(object)
}
@ 

\section{\code{RweaveRtfFinish}}

This is the second to the last function in the driver set. 

\begin{verbatim}
shell> ~/linux-local/noweb/notangle -Rfinish RweaveRtf.Rnw > RweaveRtfFinish.R
\end{verbatim}

Here, \code{object} is 

<<finish>>= 
RweaveRtfFinish <- function (object, error = FALSE) {
@ 

Recall from above that \code{object\$output} is a \code{c("file", "connection")}
object so this gives the description of the output file (e.g., \code{report.rtf}).
<<finish>>=
  outputname <- summary(object$output)$description
@ 

This gives the input filename. Remember that \code{object\$srcfile} is 
an object of class \code{srcfile}.

<<finish>>=
    inputname <- object$srcfile$filename
@ 

Indicate success and what to do with the RTF file.

<<finish>>=
    if (!object$quiet && !error) {
      ## cat("\n", gettextf("You can now run LaTeX on '%s'", outputname), 
      ##    "\n", sep = "")
      cat("\n", gettextf("You can now open '%s' with your RTF-aware word processor", outputname), 
          "\n", sep = "")      
    }
@ 

Now close the output file (e.g., \code{report.rtf}).

<<finish>>=
    close(object$output)
@

<<finish>>=
    if (length(object$chunkout) > 0) 
        for (con in object$chunkout) close(con)
@ 

This is the \code{concordance} stuff which will according to \code{help(RweaveLatex)}:

\begin{quote}
      Write a concordance file to link the input line numbers to the
      output line numbers.  This is an experimental feature; see the
      source code for the output format, which is subject to change in
      future releases.
\end{quote}

For now it is commented out. (Note that \code{odfWeave} does not implement it.)     
      
<<finish>>= 
    ##if (object$haveconcordance) {
    ##    linesout <- object$linesout
    ##    vals <- rle(diff(linesout))
    ##    vals <- c(linesout[1], as.numeric(rbind(vals$lengths, 
    ##        vals$values)))
    ##    concordance <- paste(strwrap(paste(vals, collapse = " ")), 
    ##        collapse = " %\n")
    ##    special <- paste("\\special{concordance:", outputname, 
    ##        ":", inputname, ":%\n", concordance, "}\n", sep = "")
    ##    cat(special, file = object$concordfile)
    ##}
@

Now return the name of the output file (e.g.,  \code{report.rtf}) invisibly.
<<finish>>=
    invisible(outputname)
}
@

\section{Testing}
I can test the results. Note I have to make the syntax object accept the RTF. Unfortunately,
Word espaces curly brackets so I need to work on \code{docexpr}. 

<<>>=
notangle()
notangleExamples()
Sweave("example1.Rrtf", driver = RweaveRtf, syntax = SweaveSyntaxRtf)
@ 


\appendix
\section{What is \code{Sweave} doing?}
It turns out that it's important to know what \code{Sweave} does. Here
I outline the process as I now it.


\section{Reminders}
\begin{itemize}
  \item Sweave options are code chunk options
    \item Hooks are functions that correspond to logical Sweave
      options. For example \code{options(SweaveHooks = list(fig =
        foo))} means \code{foo()} is executed before the chunk. For
      example, we might have \code{foo <- function() par(font.main =
        1)}.
\end{itemize}

The two key elements that Sweave works with are \code{object} and
\code{chunk}. I know what a \code{chunk} is (a collection of
documentation or code lines lines) but I don't know what an
\code{object} is.

The function \code{Sweave()} does a lot but at its heart appears to be
this simplified sequence.
\begin{itemize}
  \item Chunk zero is the start of the document and is a \code{"doc"} chunk.
  \item 
\end{itemize}


\section{Issues}
\begin{itemize}
  \item If the document is written in Word, the file would have a code chunk defined by this
    \begin{verbatim}
      \par @<<>>=
    \end{verbatim}
    This can be handled by modifying the syntax object. Note that we need those paragraph breaks
    since the RTF that Word emits will have the code on separate lines. Using linebreaks will just
    add the command \textbackslash{line} in front of each code line but string them altogether like
    this
    \begin{verbatim}
      \par @<<>>=\line ab <- a + b\line print(7)\line median(y)\line @
    \end{verbatim}
  \item Do I want to use \code{Sweave(..., driver = RweaveRtf)} or
    \code{Rtfweave()}? For example, \code{odfWeave} is really a
    wrapper for \code{Sweave}. I'll wrap later.
  \item I may want to add code chunk options to set figure resolution and pointsize.
\end{itemize}

\section{Cleaning RTF}
After saving and with additional editing MS Word will put revision
marks and other hidden things that give Rtfweave problems.  These can
be taken out if they are regular enough regular expressions.  I don't
know the patterns exactly. The current problem is something like
this. (The underscore is really a space.)

\begin{verbatim}
\insrsid10901146\charrsid10901146__each period  
\insrsid10901146\charrsid6638647_approve
\end{verbatim}


<<>>=
clean_MSWord_rtf <- function(con){
  document <- readLines(con)
  ## patterns <- 
  ##   c("\\\\insrsid\\d{7,8}  ", 
  ##     "\\\\delrsid\\d{7,8}  ", 
  ##     "\\\\charrsid\\d{7,8}  ", 
  ##     "\\\\sectrsid\\d{7,8}  ", 
  ##     "\\\\pararsid\\d{7,8}  ", 
  ##     "\\\\tblrrsid\\d{7,8}  ") 
  ## 
  ## ## The above regular expressions are replaced with one space
  ## for(i in 1:length(document)){
  ##   for(p in patterns){
  ##     document[i] <- gsub(p, " ", document[i], perl = TRUE)
  ##   }
  ## }
  
  patterns <- 
    c("\\}\\{\\\\rtlch\\\\fcs1 \\\\af[02] \\\\ltrch\\\\fcs0 (\\\\f\\d{1,2})?",
      "\\\\insrsid\\d{6,8} ?", 
      "\\\\delrsid\\d{7,8} ?", 
      "\\\\charrsid\\d{7,8} ?", 
      "\\\\sectrsid\\d{7,8} ?", 
      "\\\\pararsid\\d{7,8} ?", 
      "\\\\tblrrsid\\d{7,8} ?")
      
    
  ## The above regular expressions are replaced with '""'
  for(i in 1:length(document)){
    for(p in patterns){
      document[i] <- gsub(p, "", document[i], perl = TRUE)
    }
  }
  writeLines(document, con = "WEAVEME.Rrtf")
}

clean_MSWord_rtf("MSWord native RTF ovarian.rtf")
W <- readLines("WEAVEME.Rrtf")
(lines <- grep("Sexpr", W, value = TRUE))

tail(W)

Sweave("WEAVEME.Rrtf", 
       driver = RweaveRtf(), 
       syntax = SweaveSyntaxRtfMS, 
       output = "output.rtf")

Sweave("MSWord native RTF.rtf",
       driver = RweaveRtf(), 
       syntax = SweaveSyntaxRtfMS, 
       output = "output.rtf")



grep(SweaveSyntaxRtfMS$docexpr, line, v = T)

grep("Sexpr\\|([^\\|]*)\\|", "\\par How about Sexpr|3+2*pi / 5|?", value = TRUE)

## Cannot use \Sexpr{} because eventually XML tags or stuff gets iserted
## Currently trying Sexpr{} but not having luck yet.


@ 

  
\end{document}

%% Current brick wall: SOLVED!!!
%% In MSword native RTF.rtf -> WEAVEME.rtf -> output.rtf, the following problems:
%% 1) chunk options are not honored. Not sure why
%% 2) chunk label is "\par" and I'm not sure why.
%%    - I don't even now where the code chunk label gets parsed. Is it driven by the
%%      syntax object?
%%

%% I may want to allow the output chunks to be in a user-specified font; currently hard-coded f2
%% but that seems to correspond to Courrier New which is fine. 

%% Need to change \Sexpr syntax to something that works in RTF that Word saves. Word saves
%% it as "\\Sexpr\{pi + 2 + 3\}" which isn't so bad.
%% Challenge is currently that Word may insrt line breaks in the RTF file.



%% Other problems? Cleaning Word seems to be working but need to make that automatic.

%% Needed:

%% Function to get the font table of a document
%% function to get the color table of a document

Heather's problems:

Line wrapping in Word creates problems. Myabe I want to do this:

I think so long as each line in the source document displays as a single line that is valid R code (i.e. without line wrapping) I should be able to get rid of the control characters and have everything on one line as before. 

Algorithm for within a code chunk:
1 Fix anything like smart quotes, \\lquote, etc.

2 Get rid of "} *{" first

3then get rid of control words with a space
    
    
% cat junk.rtf | ~/linux-local/bin/unrtf --text /dev/stdin 

How do I do this in R?

<<>>=

system2("/people/biostat2/weigand/linux-local/bin/unrtf",
        args = "-P /people/biostat2/weigand/RTF/ -t text ~/ibm/junk.rtf",
        stdout = TRUE)



        
@ 

unrtf -P /people/biostat2/weigand/linux-local/lib/unrtf/ -t text junk.rtf


I may have to add unrtf = TRUE to flag whether the chunk is passed to unrtf.
