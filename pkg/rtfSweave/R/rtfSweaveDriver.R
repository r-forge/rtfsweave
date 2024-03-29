###############################################################
##  RTF "port" of src/library/utils/R/SweaveDrivers.R        ##
##  which is part of the R package, http://www.R-project.org ##
##  Written by Stephen Weigand <Weigand.Stephen@mayo.edu>    ##
###############################################################
##  
##  Copyright (C) 1995-2021 The R Core Team
##
##  This program is free software; you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation; either version 2 of the License, or
##  (at your option) any later version.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details. 
##
##  A copy of the GNU General Public License is available at
##  http://www.r-project.org/Licenses/
##
###############################################################

RweaveRtf <- function()
{
    list(setup = RweaveRtfSetup,
         runcode = RweaveRtfRuncode,
         writedoc = RweaveRtfWritedoc,
         finish = RweaveRtfFinish,
         checkopts = RweaveRtfOptions)
}

## We definitely do not want '.' in here, to avoid misidentification
## of file extensions.  Note that - is used literally here.
.SweaveValidFilenameRegexp <- "^[[:alnum:]/#+_-]+$"

RweaveRtfSetup <-
    function(file, syntax, output = NULL, quiet = FALSE, debug = FALSE,
             ...)
{
    
    ## RTF spec says encoding must be 7 bit ASCII. This will print
    ## out any non-ASCII characters in the file and stop
    if (length(tools::showNonASCIIfile(file))) {
        stop("File ", sQuote(file), "\n",
             "       has non-ASCII characters on the lines shown above\n",
             "       which need to be removed to prevent corrupting the RTF\n",
             "       output file. You can use ", sQuote("tools::showNonASCIIfile"),
             ".")
    }

    dots <- list(...)
    if (is.null(output)) {
        prefix.string <- basename(sub(syntax$extension, "", file))
        output <- paste(prefix.string, "rtf", sep = ".")
    } else prefix.string <- basename(sub("\\.rtf$", "", output))

    if (!quiet) cat("Writing to file ", output, "\n",
                   "Processing code chunks with options ...\n", sep = "")
    encoding <- attr(file, "encoding")
    if (encoding %in% c("ASCII", "bytes")) encoding <- ""
    output <- file(output, open = "w", encoding = encoding)

    options <- list(prefix = TRUE,
                    prefix.string = prefix.string,
                    engine = "R",
                    print = FALSE,
                    eval = TRUE,
                    fig = FALSE,
                    png = TRUE,
                    jpeg = FALSE,
                    tiff = FALSE,
                    wmf = FALSE,
                    width = 6,
                    height = 6,
                    pointsize = 12,
                    resolution = 300,
                    tiff.resolution = 300,                    
                    tiff.compression = "lzw", 
                    hex = TRUE,
                    grdevice = "",
                    rtf.Schunk  = "\\ql \\sb120 \\sa120",
                    rtf.Sinput  = "\\b0 \\f2 \\fs20 \\i",
                    rtf.Soutput = "\\b0 \\f2 \\fs20",
                    term = TRUE,
                    echo = TRUE,
                    keep.source = TRUE,
                    results = "verbatim",
                    split = FALSE,
                    strip.white = "true",
                    include = TRUE,
                    expand = TRUE, # unused by us, for 'highlight'
                    concordance = FALSE,
                    figs.only = TRUE)
    options$.defaults <- options
    options[names(dots)] <- dots

    ## to be on the safe side: see if defaults pass the check
    options <- RweaveRtfOptions(options)

    list(output = output,
         haveconcordance = FALSE, debug = debug, quiet = quiet,
         syntax = syntax, options = options,
         chunkout = list(), # a list of open connections
         srclines = integer())
}

makeRweaveRtfCodeRunner <- function(evalFunc = RweaveEvalWithOpt)
{
    ## Return a function suitable as the 'runcode' element
    ## of an Sweave driver.  evalFunc will be used for the
    ## actual evaluation of chunk code.
    ## FIXME: well, actually not for the figures.
    ## If there were just one figure option set, we could eval the chunk
    ## only once.
    function(object, chunk, options) {
        png.Swd <- function(name, width, height, options, ...)
            grDevices::png(filename = paste(chunkprefix, "png", sep = "."),
                           width = width, height = height,
                           pointsize = options$pointsize,
                           res = options$resolution, units = "in")
        jpeg.Swd <- function(name, width, height, options, ...)
            grDevices::jpeg(filename = paste(chunkprefix, "jpeg", sep = "."),
                            width = width, height = height,
                            pointsize = options$pointsize,
                            res = options$resolution, units = "in")
        tiff.Swd <- function(name, width, height, options, ...)
            grDevices::tiff(filename = paste(chunkprefix, "tiff", sep = "."),
                            width = width, height = height,
                            pointsize = options$pointsize,
                            res = options$tiff.resolution, units = "in",
                            compression = options$tiff.compression)
        wmf.Swd <- function(name, width, height, options, ...)
            grDevices::win.metafile(filename = paste(chunkprefix, "wmf", sep = "."),
                                    width = width, height = height,
                                    pointsize = options$pointsize)

        if (!(options$engine %in% c("R", "S"))) return(object)

        devs <- devoffs <- list()
        if (options$fig && options$eval) {
            if (options$png) {
                devs <- c(devs, list(png.Swd))
                devoffs <- c(devoffs, list(grDevices::dev.off))
            }
            if (options$jpeg) {
                devs <- c(devs, list(jpeg.Swd))
                devoffs <- c(devoffs, list(grDevices::dev.off))
            }
            if (options$tiff) {
                devs <- c(devs, list(tiff.Swd))
                devoffs <- c(devoffs, list(grDevices::dev.off))
            }
            if (options$wmf) {
                devs <- c(devs, list(wmf.Swd))
                devoffs <- c(devoffs, list(grDevices::dev.off))
            }

            if (nzchar(grd <- options$grdevice)) {
                devs <- c(devs, list(get(grd, envir = .GlobalEnv)))
                grdo <- paste(grd, "off", sep = ".")
                devoffs <- c(devoffs,
                             if (exists(grdo, envir = .GlobalEnv))
                                 list(get(grdo, envir = .GlobalEnv))
                             else list(grDevices::dev.off))
            }
        }
        if (!object$quiet) {
            cat(formatC(options$chunknr, width = 2), ":")
            if (options$echo) cat(" echo")
            if (options$keep.source) cat(" keep.source")
            if (options$eval) {
                if (options$print) cat(" print")
                if (options$term) cat(" term")
                cat("", options$results)
                if (options$fig) {
                    if (options$png) cat(" png")
                    if (options$jpeg) cat(" jpeg")
                    if (options$tiff) cat(" tiff")                    
                    if (options$wmf) cat(" wmf")
                    if (!is.null(options$grdevice)) cat("", options$grdevice)
                }
            }
            cat(" (")
            if (!is.null(options$label))
                cat("label = ", options$label, ", ", sep = "")
            filenum <- attr(chunk, "srcFilenum")[1]
            filename <- attr(chunk, "srcFilenames")[filenum]
            cat(basename(filename), ":", attr(chunk, "srclines")[1], ")", sep = "")
            cat("\n")
        }

        chunkprefix <- RweaveChunkPrefix(options)

        if (options$split) {
            ## [x][[1L]] avoids partial matching of x
            chunkout <- object$chunkout[chunkprefix][[1L]]
            if (is.null(chunkout)) {
                chunkout <- file(paste(chunkprefix, "rtf", sep = "."), "w")
                if (!is.null(options$label))
                    object$chunkout[[chunkprefix]] <- chunkout
                if(!grepl(.SweaveValidFilenameRegexp, chunkout))
                    warning("file stem ", sQuote(chunkout), " is not portable",
                            call. = FALSE, domain = NA)
            }
        } else chunkout <- object$output

        srcfile <- srcfilecopy(object$filename, chunk, isFile = TRUE)

        ## Note that we edit the error message below, so change both
        ## if you change this line:
        chunkexps <- try(parse(text = chunk, srcfile = srcfile), silent = TRUE)
        if (inherits(chunkexps, "try-error"))
            chunkexps[1L] <- sub(" parse(text = chunk, srcfile = srcfile) : \n ",
                                 "", chunkexps[1L], fixed = TRUE)

        RweaveTryStop(chunkexps, options)

        ## Some worker functions used below...
        putSinput <- function(dce, leading) {
            if (!openSinput) {
                if (!openSchunk) {
                    ## cat("\\begin{Schunk}\n", file = chunkout)
                    cat("{\\pard", options$rtf.Schunk, "\n", file = chunkout)
                    linesout[thisline + 1L] <<- srcline
                    filenumout[thisline + 1L] <<- srcfilenum
                    thisline <<- thisline + 1L
                    openSchunk <<- TRUE
                }
                ## cat("\\begin{Sinput}", file = chunkout)
                cat("{", options$rtf.Sinput, "\n", sep = "", file = chunkout)
                openSinput <<- TRUE
            }
            leading <- max(leading, 1L) # safety check
            ## RTF line break without a new paragraph is '\line'
            cat("\n", paste(getOption("prompt"),
                            ## Need to escape curly braces
                            gsub("(\\{|\\})", "\\\\\\1", dce[seq_len(leading)]),
                            "\\line",
                            sep = "", collapse = "\n"),
                file = chunkout, sep = "")
            if (length(dce) > leading)
                cat("\n", paste(getOption("continue"),
                                gsub("(\\{|\\})", "\\\\\\1", dce[-seq_len(leading)]),
                                "\\line",
                                sep = "", collapse = "\n"),
                    file = chunkout, sep = "")
            linesout[thisline + seq_along(dce)] <<- srcline
            filenumout[thisline + seq_along(dce)] <<- srcfilenum
            thisline <<- thisline + length(dce)
        }

        trySrcLines <- function(srcfile, showfrom, showto, ce) {
	    lines <- tryCatch(suppressWarnings(getSrcLines(srcfile, showfrom, showto)),
			      error = function(e)e)
	    if (inherits(lines, "error")) {
		lines <- if (is.null(ce)) character()
		else deparse(ce, width.cutoff = 0.75*getOption("width"))
            }
            lines
        }

        echoComments <- function(showto) {
            if (options$echo && !is.na(lastshown) && lastshown < showto) {
                dce <- trySrcLines(srcfile, lastshown + 1L, showto, NULL)
                linedirs <- grepl("^#line ", dce)
		dce <- dce[!linedirs]
		if (length(dce))
                    putSinput(dce, length(dce)) # These are all trailing comments
                lastshown <<- showto
            }
        }

        openSinput <- FALSE
        openSchunk <- FALSE

        srclines <- attr(chunk, "srclines")
        srcfilenums <- attr(chunk, "srcFilenum")
        linesout <- integer()      # maintains concordance
        filenumout <- integer()	   # ditto
        srcline <- srclines[1L]    # current input line
        srcfilenum <- srcfilenums[1L] # from this file
        thisline <- 0L             # current output line
        lastshown <- 0L            # last line already displayed;

##        refline <- NA    # line containing the current named chunk ref
        leading <- 1L    # How many lines get the user prompt

        srcrefs <- attr(chunkexps, "srcref")

        if (length(devs)) {
            if(!grepl(.SweaveValidFilenameRegexp, chunkprefix))
                warning("file stem ", sQuote(chunkprefix), " is not portable",
                        call. = FALSE, domain = NA)
            if (options$figs.only)
                devs[[1L]](name = chunkprefix,
                           width = options$width, height = options$height,
                           options)
        }
        SweaveHooks(options, run = TRUE)

        for (nce in seq_along(chunkexps)) {
            ce <- chunkexps[[nce]]
            if (options$keep.source && nce <= length(srcrefs) &&
                !is.null(srcref <- srcrefs[[nce]])) {
                showfrom <- srcref[7L]
                showto <- srcref[8L]

                dce <- trySrcLines(srcfile, lastshown+1L, showto, ce)
                leading <- showfrom - lastshown

                lastshown <- showto
                srcline <- srcref[3L]

                linedirs <- grepl("^#line ", dce)
                dce <- dce[!linedirs]
                # Need to reduce leading lines if some were just removed
                leading <- leading - sum(linedirs[seq_len(leading)])

                while (length(dce) && length(grep("^[[:blank:]]*$", dce[1L]))) {
                    dce <- dce[-1L]
                    leading <- leading - 1L
                }
            } else {
                dce <- deparse(ce, width.cutoff = 0.75*getOption("width"))
                leading <- 1L
            }
            if (object$debug)
                cat("\nRnw> ", paste(dce, collapse = "\n+  "),"\n")

            if (options$echo && length(dce)) putSinput(dce, leading)

            ## avoid the limitations (and overhead) of output text connections
            if (options$eval) {
                tmpcon <- file()
                sink(file = tmpcon)
                err <- tryCatch(evalFunc(ce, options), finally = {
                     cat("\n")           # make sure final line is complete
                     sink()
                })
                output <- readLines(tmpcon)
                close(tmpcon)
                ## delete empty output
                if (length(output) == 1L && !nzchar(output[1L])) output <- NULL
                RweaveTryStop(err, options)
            } else output <- NULL

            ## or writeLines(output)
            if (length(output) && object$debug)
                cat(paste(output, collapse = "\n"))

            if (length(output) && (options$results != "hide")) {
                if (openSinput) {
                    ## cat("\n\\end{Sinput}\n", file = chunkout)
                    cat("\n}\n", file = chunkout) # end input paragraph
                    linesout[thisline + 1L:2L] <- srcline
                    filenumout[thisline + 1L:2L] <- srcfilenum
                    thisline <- thisline + 2L
                    openSinput <- FALSE
                }
                if (options$results == "verbatim") {
                    if (!openSchunk) {
                        ## cat("\\begin{Schunk}\n", file = chunkout)
                        cat("{\\pard", options$rtf.Schunk, "\n", file = chunkout)
                        linesout[thisline + 1L] <- srcline
                        filenumout[thisline + 1L] <- srcfilenum
                        thisline <- thisline + 1L
                        openSchunk <- TRUE
                    }
                    ## No '\\line' here.
                    ## cat("\\begin{Soutput}\n", file = chunkout)
                    cat("{", options$rtf.Soutput, "\n", sep = "", file = chunkout)
                    linesout[thisline + 1L] <- srcline
                    filenumout[thisline + 1L] <- srcfilenum
                    thisline <- thisline + 1L
                }
                ## I should check this.
                ## output <- paste(output, collapse = "\n")
                output <- paste(output, collapse = ifelse(options$results == "rtf", "\n", "\\line\n")) #these are the results
                if (options$strip.white %in% c("all", "true")) {
                    output <- sub("^[[:space:]]*\n", "", output)
                    output <- sub("\n[[:space:]]*$", "", output)
                    if (options$strip.white == "all")
                        output <- sub("\n[[:space:]]*\n", "\n", output)
                }
                cat(output, file = chunkout)
                count <- sum(strsplit(output, NULL)[[1L]] == "\n")
                if (count > 0L) {
                    linesout[thisline + 1L:count] <- srcline
                    filenumout[thisline + 1L:count] <- srcfilenum
                    thisline <- thisline + count
                }

                remove(output)

                if (options$results == "verbatim") {
                    ## cat("\n\\end{Soutput}\n", file = chunkout)
                    cat("\n}\n", file = chunkout)
                    linesout[thisline + 1L:2L] <- srcline
                    filenumout[thisline + 1L:2L] <- srcfilenum
                    thisline <- thisline + 2L
                }
            }
        } # end of loop over chunkexps.

        ## Echo remaining comments if necessary
        if (options$keep.source) echoComments(length(srcfile$lines))

        if (openSinput) {
            cat("\n}\n", file = chunkout) # end italic
            linesout[thisline + 1L:2L] <- srcline
            filenumout[thisline + 1L:2L] <- srcfilenum
            thisline <- thisline + 2L
        }

        if (openSchunk) {
            cat("\\par}\n", file = chunkout) # end the chunk
            linesout[thisline + 1L] <- srcline
            filenumout[thisline + 1L] <- srcfilenum
            thisline <- thisline + 1L
        }

        if (is.null(options$label) && options$split) close(chunkout)

        ## NOTE: This has no analog in RTF
        if (options$split && options$include) {
            cat("\\input{", chunkprefix, "}\n", sep = "", file = object$output)
            linesout[thisline + 1L] <- srcline
            filenumout[thisline + 1L] <- srcfilenum
            thisline <- thisline + 1L
        }

        if (length(devs)) {
            if (options$figs.only) devoffs[[1L]]()
            for (i in seq_along(devs)) {
                if (options$figs.only && i == 1) next
                devs[[i]](name = chunkprefix, width = options$width,
                          height = options$height, options)
                err <- tryCatch({
                    SweaveHooks(options, run = TRUE)
                    eval(chunkexps, envir = .GlobalEnv)
                }, error = function(e) {
                    devoffs[[i]]()
                    stop(conditionMessage(e), call. = FALSE, domain = NA)
                })
                devoffs[[i]]()
            }

            if (options$include) {
                ## TIFFs can't be embedded in an RTF the way PNGs or JPEGs
                ## can so if we have <<... tiff = true ...>>= it means the
                ## user wants the TIFF written to disk. So we keep this
                ## tiff conditional out of the if ... else blocks
                if (options$tiff) {
                    imagefilename <- paste(chunkprefix, "tiff", sep = ".")
                }
                if (options$png) {
                    imagefilename <- paste(chunkprefix, "png", sep = ".")
                    if(options$hex) {
                        size <- file.info(imagefilename)$size
                        hex <- readBin(imagefilename, what = "raw", size)
                        cat("\n{\\pict\\pngblip\n", file = object$output)
                        cat(as.character(hex), file = object$output, fill = TRUE, sep = "")
                        cat("}\n", file = object$output)
                    }
                    else {
                        cat("{\\field{\\*\\fldinst { INCLUDEPICTURE \\\\d",
                            shQuote(imagefilename, "cmd"),
                            "\\\\* MERGEFORMAT}}}",
                            file = object$output,
                            sep = "\n")
                        }
                } else if (options$jpeg) {
                    imagefilename <- paste(chunkprefix, "jpeg", sep = ".")
                    if(options$hex) {
                        size <- file.info(imagefilename)$size
                        hex <- readBin(imagefilename, what = "raw", size)
                        cat("\n{\\pict\\jpegblip\n", file = object$output)
                        cat(as.character(hex), file = object$output, fill = TRUE, sep = "")
                        cat("}\n", file = object$output)
                    }
                    else {
                        cat("{\\field\\fldedit{\\*\\fldinst { INCLUDEPICTURE \\\\d",
                            shQuote(imagefilename, "cmd"),
                            "\\\\* MERGEFORMATINET }}{\\fldrslt { }}}",
                            file = object$output,
                            sep = "\n")
                    }
                } else if (options$wmf) {
                    imagefilename <- paste(chunkprefix, "wmf", sep = ".")
                    if(options$hex) {
                        size <- file.info(imagefilename)$size
                        hex <- readBin(imagefilename, what = "raw", size)
                        cat("\n{\\pict\\emfblip\n", file = object$output)
                        cat(as.character(hex), file = object$output, fill = TRUE, sep = "")
                        cat("}\n", file = object$output)
                    }
                    else {
                        cat("{\\field\\fldedit{\\*\\fldinst { INCLUDEPICTURE \\\\d",
                            shQuote(imagefilename, "cmd"),
                            "\\\\* MERGEFORMATINET }}{\\fldrslt { }}}",
                            file = object$output,
                            sep = "\n")
                    }
                }
                linesout[thisline + 1L] <- srcline
                filenumout[thisline + 1L] <- srcfilenum
                thisline <- thisline + 1L
            }
        }
        object$linesout <- c(object$linesout, linesout)
        object$filenumout <- c(object$filenumout, filenumout)
        object
    }
}

RweaveRtfRuncode <- makeRweaveRtfCodeRunner()

RweaveRtfWritedoc <- function(object, chunk)
{
    linesout <- attr(chunk, "srclines")
    filenumout <- attr(chunk, "srcFilenum")

    while(length(pos <- grep(object$syntax$docexpr, chunk)))
    {
        cmdloc <- regexpr(object$syntax$docexpr, chunk[pos[1L]])
        cmd <- substr(chunk[pos[1L]], cmdloc,
                      cmdloc + attr(cmdloc, "match.length") - 1L)
        cmd <- sub(object$syntax$docexpr, "\\1", cmd)
        if (object$options$eval) {
            val <- tryCatch(as.character(eval(parse(text = cmd), envir = .GlobalEnv)),
		    error = function(e) {
	               filenum <- attr(chunk, "srcFilenum")[pos[1L]]
                       filename <- attr(chunk, "srcFilenames")[filenum]
                       location <- paste0(basename(filename), ":", attr(chunk, "srclines")[pos[1L]])
		       stop("at ",location, ", ", conditionMessage(e), domain = NA, call. = FALSE)
		   })
            ## protect against character(), because sub() will fail
            if (length(val) == 0L) val <- ""
        }
        else val <- paste0("{\\\\f2 <<", cmd, ">>}")

        chunk[pos[1L]] <- sub(object$syntax$docexpr, val, chunk[pos[1L]])
    }

    ## Process \SweaveOpts{} or similar
    ## Since they are only supposed to affect code chunks, it is OK
    ## to process all such in a doc chunk at once.
    while(length(pos <- grep(object$syntax$docopt, chunk)))
    {
        opts <- sub(paste0(".*", object$syntax$docopt, ".*"),
                    "\\1", chunk[pos[1L]])
        ## SweaveParseOptions is not exported. I do what the 'ascii' package
        ## does. ('R2HTML' copies in the function from r45768 and calls it
        ## InternalSweaveParseOptions        
        object$options <- utils:::SweaveParseOptions(opts, object$options,
                                                     RweaveRtfOptions)

        if (isTRUE(object$options$concordance)
            && !object$haveconcordance) {
            savelabel <- object$options$label
            object$options$label <- "concordance"
            prefix <- RweaveChunkPrefix(object$options)
            object$options$label <- savelabel
            object$concordfile <- paste(prefix, "tex", sep = ".")
            chunk[pos[1L]] <- sub(object$syntax$docopt,
                                  paste0("\\\\input{", prefix, "}"), ## No analog in RTF but leave it for now
                                  chunk[pos[1L]])
            object$haveconcordance <- TRUE
        } else
            chunk[pos[1L]] <- sub(object$syntax$docopt, "", chunk[pos[1L]])
    }

    cat(chunk, sep = "\n", file = object$output)
    object$linesout <- c(object$linesout, linesout)
    object$filenumout <- c(object$filenumout, filenumout)

    object
}

RweaveRtfFinish <- function(object, error = FALSE)
{
    outputname <- summary(object$output)$description
    if (!object$quiet && !error) {
	if(!file.exists(outputname))
	    stop(gettextf("the output file '%s' has disappeared", outputname))
	cat("\n",
	    sprintf("You can now open %s", sQuote(outputname)),
	    "\n", sep = "")
    }
    close(object$output)
    if (length(object$chunkout))
        for (con in object$chunkout) close(con)
    if (object$haveconcordance) {
    	## This output format is subject to change.  Currently it contains
    	## three or four parts, separated by colons:
    	## 1.  The output .rtf filename
    	## 2.  The input .Rnw filename
    	## 3.  Optionally, the starting line number of the output coded as "ofs nn",
    	##     where nn is the offset to the first output line.  This is omitted if nn is 0.
    	## 4.  The input line numbers corresponding to each output line.
    	##     This are compressed using the following simple scheme:
    	##     The first line number, followed by
    	##     a run-length encoded diff of the rest of the line numbers.
        linesout <- object$linesout
        filenumout <- object$filenumout
        filenames <- object$srcFilenames[filenumout]
	if (!is.null(filenames)) {  # Might be NULL if an error occurred
	    filegps <- rle(filenames)
	    offset <- 0L
	    for (i in seq_along(filegps$lengths)) {
		len <- filegps$lengths[i]
		inputname <- filegps$values[i]
		vals <- rle(diff(linesout[offset + seq_len(len)]))
		vals <- c(linesout[offset + 1L], as.numeric(rbind(vals$lengths, vals$values)))
		concordance <- paste(strwrap(paste(vals, collapse = " ")), collapse = " %\n")
		special <- paste0("\\Sconcordance{concordance:", outputname, ":",
			     inputname, ":",
			     if (offset) paste0("ofs ", offset, ":") else "",
			     "%\n",
			     concordance,"}\n")
		cat(special, file = object$concordfile, append=offset > 0L)
		offset <- offset + len
	    }
	}
    }
    invisible(outputname)
}

## This is the check function similar to that used in RweaveLatex and Rtangle drivers
RweaveRtfOptions <- function(options)
{
    defaults <- options[[".defaults"]]

    ## convert a character string to logical
    c2l <- function(x)
        if (is.null(x)) FALSE else suppressWarnings(as.logical(x))

    ## numeric
    NUMOPTS <- c("width", "height", "resolution", "pointsize",
                 "tiff.resolution")

    ## character: largely for safety, but 'label' matters as there
    ## is no default (and someone uses "F")
    CHAROPTS <- c("results", "prefix.string", "engine", "label",
                  "strip.white", "grdevice")


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


###------------------------------------------------------------------------
SweaveSyntaxRtf <- 
  list(doc = "^@",
       code = "^<<(.*)>>=.*",     
       coderef = "^<<(.*)>>.*",   
       docopt = "\\{\\\\SweaveOpts ([^\\}]*)\\}",      
       docexpr = "\\{\\\\Sexpr ([^\\}]*)\\}", 
       extension = "\\.[rsRS]?rtf$",
       syntaxname = "\\{\\\\SweaveSyntax ([^\\}]*)\\}", 
       input = "^[[:space:]]*\\{\\\\SweaveInput ([^\\}]*)\\}",
       trans = 
       list(doc = "^@",                                         
            code = "^<<\\1>>=",
            coderef = "^<<\\1>>",
            docopt = "{\\\\SweaveOpts \\1}",
            docexpr = "{\\\\Sexpr \\1}",
            extension = ".Rrtf",
            syntaxname = "{\\\\SweaveSyntax SweaveSyntaxRtf}",
            input = "{\\\\SweaveInput \\1}"))

class(SweaveSyntaxRtf) <- "SweaveSyntax"

## Jan. 19, 2016. Didn't know I needed a separate tangle driver
## A few things I need from utils are not exported.
RtangleRtf <- function(){
    list(setup = RtangleSetup,
         runcode = utils::RtangleRuncode,
         writedoc = RtangleWritedoc,
         finish = utils::RtangleFinish,
         checkopts = RweaveRtfOptions)    
}
