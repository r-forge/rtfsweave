## ===================================
## The "built-in" hooks for RTF output
## ===================================
##
## Questions
## =========
##
## How do we tell `knit()` how to automatically handle a file name
## `file.Rrtf`? Is this in the patterns? No. `knitr:::auto_out_name`
## is hard-coded to recognize some names but not others. It looks like
## I have to specify `output = "file.rtf"`. Too bad.
##
## How do we set the hooks?
## ------------------------
## I think an early chunk has to have `render_rtf()` and `knit` does
## the right thing. Since `knit` expects global options to be set in a
## code chunk, I think the document would use
##
## <<echo=FALSE>>=
## render_rtf()
## opts_chunk$set(comment = NA, ..., prompt = FALSE)
## @
##
## Should I set the patterns in `render_rtf()`?
## --------------------------------------------
## I don't know
##
## How does `knit` figure out the patterns?
## ----------------------------------------
## - It checks current patterns with `knit_patterns$get()`
## - It uses `detect_pattern(text, ext)` where `text` comes
##   from `readLines`. This function checks for known extensions
##   then if extension is not recognized, then it iterates
##   through `names(all_patterns)` and if it finds the document
##   is using a matching `chunk.begin` pattern or a matching
##   `inline.code` pattern, uses that pattern.
##
##   This means I have to put my patterns first so that
##   `names(all_patterns)[1]` is `"rtf"`?

## Maybe I just do 


## Seems like the protocol would be
## 1. Set the patterns
##    - We recommend `rnw`-like patterns

## Needed for a `render_rtf` function
## ==================================
## 1. `chunk` options
##
## 2. `knit` options
##
## 3. Hooks (use `str(knit_hooks$get())`)
##    - source ::
##    - output ::
##    - warning ::
##    - message ::
##    - error ::
##    - plot ::
##    - inline ::
##    - chunk ::
##    - text ::
##    - document ::

## We may want to keep the paragraph together
## or with the next (a chunk options?)?

## Patterns
## ========

## knit_patterns$restore() ## ?knit_patterns
## 
## rtf_patterns <-
##   list(chunk.begin = "^\\s*<<(.*)>>=.*$",
##        chunk.end = "^\\s*@\\s*(%+.*|)$",
##        inline.code = "\\{\\\\Sexpr([^}]+)\\}", # "\\\\Sexpr\\{([^}]+)\\}",
##        inline.comment = "^\\s*%.*", 
##        ref.chunk = "^\\s*<<(.+)>>\\s*$")
##        ## Not ever pattern has these two so I omit them:
##        ## header.begin = "(^|\n)[^%]*\\s*\\\\documentclass[^}]+\\}"
##        ## document.begin = \\s*\\\\begin\\{document\\}"
## 
## 
## pat_rnw()
## knit_patterns$set(rtf_patterns)

## Hooks (n=10)
## ============

rtf.hook.source <- function(x, options) {
  commands <- rtf.getOption("par.source")
  
  if (is.null(commands))
    commands <- "\\sb240 \\sa240 \\f2 \\fs20 \\shading100 \\cfpat4 \\cbpat4"
  ## In a test, x was c("x <- 10", "rnorm(20)") so need to append \\line
  paste(c(paste("{\\pard", commands),
          gsub("\\n", "\\\\line\n", sub("$", "\\\\line", x)),
          "\\par}\n\n"),
        collapse = "\n")
}

rtf.hook.output <- function(x, options) {
  commands <- rtf.getOption("par.output")

  if (is.null(commands))
    commands <- "\\sb240 \\sa240 \\f2 \\fs20 \\shading100 \\cfpat4 \\cbpat4"

  paste(c(paste("{\\pard", commands),
          gsub("\\n", "\\\\line\n", x),
          "\\par}\n\n"),
          collapse = "\n")  
}

rtf.hook.warning <- rtf.hook.message <- rtf.hook.error <- rtf.hook.output

rtf.hook.plot <- function(x, options) {
  paste(c("{\\field{\\*\\fldinst { INCLUDEPICTURE \\\\d",
          shQuote(x, "cmd"),
          "\\\\* MERGEFORMAT}}}"),
        collapse = "\n")
}

rtf.hook.inline <- function(x) {
  x
}

is_blank <- function(x) {
    if (length(x)) 
        all(grepl("^\\s*$", x))
    else TRUE  
  }
output_asis <- function (x, options) {
  is_blank(x) || options$results == "asis"
}


rtf.hook.chunk <- function (x, options) {
  if (output_asis(x, options))
    return(x)
  paste0("\n", x, "\n")
}

rtf.hook.text <- function(x) {
  x
}

rtf.hook.document <- rtf.hook.text




## `render_rtf` puts it all together
## =================================
##
## Note: We use `::` since knitr is only a 'Suggests' package

render_rtf <- function () {
  ## Most formats don't set in thei `render_x` function
  ## knit_patterns$set(rtf_patterns)

  knitr::opts_chunk$set(highlight = FALSE, comment = "##", prompt = FALSE)
  knitr::opts_knit$set(out.format = "sweave")
  
  ## This is text to add to document. The argment name doesn't
  ## mean anything.
  ## set_header(framed = "", highlight = "\\usepackage{Sweave}")
  knitr::set_header(framed = "",
             highlight =
             ## could grab font list from rtf.options
             paste("{\\fonttbl",
                   "{\\f0 Times New Roman;}",
                   "{\\f1 Arial;}",
                   "{\\f2 Courier New;}",
                   "}",
                   collapse = "\n"))
                   
  knitr::knit_hooks$set(source = rtf.hook.source,
                        output = rtf.hook.output,
                        warning = rtf.hook.warning,
                        message = rtf.hook.message,
                        error = rtf.hook.error,
                        plot = rtf.hook.plot,
                        inline = rtf.hook.inline,
                        chunk = rtf.hook.chunk,
                        document = rtf.hook.document)
}
