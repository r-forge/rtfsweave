## NB: } should not be escaped in [] .
## Checked versus https://svn.r-project.org/R/trunk/src/library/utils/R/Sweave.R on Dec. 5, 2014
SweaveSyntaxRtf <- 
  list(doc = "^@",
       code = "^<<(.*)>>=.*",     # NOTE: extra at sign here to escape dbl angle brackets for noweb processing
       coderef = "^<<(.*)>>.*",   # NOTE: extra at sign here to escape dbl angle brackets for noweb processing
       docopt = "^[[:space:]]*\\{\\\\SweaveOpts ([^}]*)\\}",       ## twas [^\\}]*
       docexpr = "\\{\\\\Sexpr ([^}]*)\\}", # ... any valid S code, only curly brackets are not allowed ## twas [^\\}]*
       extension = "\\.[rsRS]?rtf$",      # i.e., .rrtf, .srtf, .Rrtf, .Srtf.
       syntaxname = "^[[:space:]]*\\{\\\\SweaveSyntax ([^}]*)\\}", ## twas [^\\}]*
       input = "^[[:space:]]*\\{\\\\SweaveInput ([^}]*)\\}",        ## twas [^\\}]*
       trans = 
       list(doc = "@",                                             ## was "^@"                                       
            code = "^<<\\1>>=",   # NOTE: extra at sign here to escape dbl angle brackets for noweb processing
            coderef = "^<<\\1>>", # NOTE: extra at sign here to escape dbl angle brackets for noweb processing
            docopt = "{\\\\SweaveOpts \\1}",                     
            docexpr = "{\\\\Sexpr \\1}", 
            extension = ".Rrtf",
            syntaxname = "{\\\\SweaveSyntax SweaveSyntaxRtf}",   
            input = "{\\\\SweaveInput \\1}"))                    
class(SweaveSyntaxRtf) <- "SweaveSyntax"
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
    if (is.null(output)) {
        prefix.string <- basename(sub(syntax$extension, "", file))
        output <- paste(prefix.string, "rtf", sep = ".")
    } else prefix.string <- basename(sub("\\.rtf$", "", output))
    if (!quiet) 
        cat("Writing to file ", output, "\n", "Processing code chunks with options ...\n", 
            sep = "")
    encoding <- attr(file, "encoding")
    if (encoding %in% c("ASCII", "bytes")) encoding <- ""
    output <- file(output, open = "w", encoding = encoding)
    ## OLD: output <- file(output, open = "w+")
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

    ## to be on the safe side: see if defaults pass the check
    options <- RweaveRtfOptions(options)

    ## I may need to take RTF control words out of the code chunk header.
    ## Will they be on separate lines? They can't be. If they were, the
    ## pattern "<< stuff >>=" would never be found.
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
    ##   orig.lines[code.start.lines] <- grep("^<<(.*)>>=.*", clean.lines, value = TRUE) # at sign for noweb
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

RweaveRtfRuncode <- function (object, chunk, options) {

if (!(options$engine %in% c("R", "S"))) {
  return(object)
}
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
    chunkprefix <- utils::RweaveChunkPrefix(options)
if (options$split) {
  chunkout <- object$chunkout[chunkprefix][[1]]
  if (is.null(chunkout)) {
    chunkout <- file(paste(chunkprefix, "rtf", sep = "."), "w")
    if (!is.null(options$label)) 
      object$chunkout[[chunkprefix]] <- chunkout
  }
}
else chunkout <- object$output
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
RweaveRtfWritedoc <- function(object, chunk) { #what's object, what's chunk??
    linesout <- attr(chunk, "srclines")
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

cat(chunk, sep = "\n", file = object$output)#, append = TRUE)
object$linesout <- c(object$linesout, linesout)
object$filenumout <- c(object$filenumout, filenumout)
return(object)
}
RweaveRtfFinish <- function (object, error = FALSE) {
  outputname <- summary(object$output)$description
    inputname <- object$srcfile$filename
    if (!object$quiet && !error) {
      ## cat("\n", gettextf("You can now run LaTeX on '%s'", outputname), 
      ##    "\n", sep = "")
      cat("\n", gettextf("You can now open '%s' with your RTF-aware word processor", outputname), 
          "\n", sep = "")      
    }
    close(object$output)
    if (length(object$chunkout) > 0) 
        for (con in object$chunkout) close(con)
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
    invisible(outputname)
}
RweaveRtfOptions <- function (options) {
  defaults <- options[[".defaults"]] # new
  ## convert a character string to logical
  c2l <- function(x) {
        ## if (is.null(x)) 
        ##     return(FALSE)
        ## else return(as.logical(toupper(as.character(x))))
      if (is.null(x)) FALSE else suppressWarnings(as.logical(x))
    }
    
    ## numeric
    NUMOPTS <- c("width", "height", "resolution", "pointsize")

    ## character: largely for safety, but 'label' matters as there
    ## is no default (and someone uses "F")
    CHAROPTS <- c("results", "prefix.string", "engine", "label",
                  "strip.white", "pdf.version", "pdf.encoding", "grdevice")    ##


    #NOLOGOPTS <- c(NUMOPTS, "results", "prefix.string", "engine", 
    #    "label", "strip.white") # "pdf.version", "pdf.encoding")

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
if (!is.null(options$results)) {
  res <- as.character(options$results)
  if(tolower(res) != res) # documented as lower-case
    warning("value of 'results' option should be lowercase",
            call. = FALSE)
  options$results <- tolower(res)
}
options$results <- match.arg(options$results, c("verbatim", "rtf", "hide"))


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
RweaveRtf <- function(){
  list(setup = RweaveRtfSetup,    
       runcode = RweaveRtfRuncode,  
       writedoc = RweaveRtfWritedoc, 
       finish = RweaveRtfFinish,     
       checkopts = RweaveRtfOptions)
}
