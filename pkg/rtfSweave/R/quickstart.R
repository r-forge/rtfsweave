## Prompt user for information in order to generate an Rrtf file
## skeleton to help a user get started using rtfSweave

quickstart <- function() {
    if (!interactive()) {
        message(sQuote("rtfSweave::quickstart"),
                " only works in an interactive session")
        return(NULL)
    }

    message("Welcome to the rtfSweave quickstart utility\n")
    
    message("Please enter values for the following (just press Enter to\n",
            "accept a default value, if one is given in brackets).")
    
    file.Rrtf <- readline(prompt = "> Name of the rtfSweave file: ")
    file.header <- paste0(basename(file_path_sans_ext(file.Rrtf)), "-header.rtf")

    orientation <- readline(prompt = "> Document orientation (portrait/landscape) [portrait]: ")
    if (identical(orientation, "")) orientation <- "portrait"

    margins <- readline(prompt = paste0("> Page margins in inches specified as ",
                                        sQuote("c(bottom, left, top, right)"),
                                        " [ c(1, 1, 1, 1) ]: "))                      
    margins <-
        if (identical(margins, "")) {
            rep(1L, 4)
        } else {
            eval(str2lang(margins))
        }

    if (!is.numeric(margins) & length(margins) == 4L) {
        message("Not a valid margin specification. Using defaults of 1 inch on all sides.")
        margins <- rep(1L, 4)
    }
    
    pnumbers <- readline("> Do you want page numbering? (y/n) [y]: ")
    if (identical(pnumbers, "")) pnumbers <- "y"
    if (!(identical(pnumbers, "y") | identical(pnumbers, "n"))) {
        message("Not a valid ", sQuote("y"), " or ", sQuote("n"), " answer; assuming yes.")
        pnumbers <- "y"
    }

    serif.font <- readline("> Serif font [Times New Roman]: ")
    if (identical(serif.font, "")) serif.font <- "Times New Roman"

    sans.font <- readline("> Sans serif font [Arial]: ")
    if (identical(sans.font, "")) sans.font <- "Arial"

    fixedwidth.font <- readline("> Fixed width font [Courier New]: ")
    if (identical(fixedwidth.font, "")) fixedwidth.font <- "Courier New"


    prefix.dir <- readline("> Subdirectory to save image files [figures]: ")
    if (identical(prefix.dir, "")) prefix.dir <- "figures"

    if (!dir.exists(prefix.dir)) {
        dir.create(prefix.dir)
        message("Creating ", sQuote(prefix.dir), " directory for figures")
    }

    prefix.string <- file.path(prefix.dir, "fig")


    ## build <- function(infile, outfile) {
    ##     require(rtfSweave)
    ##     Sweave(
    ## }
    
    
    message("Creating ", sQuote(file.Rrtf))
    out <- c("{\\rtf1",
             sprintf("{\\SweaveInput %s}", basename(file.header)),
             "",
             "<<information, engine=none>>=",
             "",
             paste0("This code chunk will be ignored since ",
                    sQuote("engine"),
                    " is neither ",
                    sQuote("R"),
                    " nor ",
                    sQuote("S"),
                    "."),
             "Which means you can use it for notes, as a scratchpad, etc.",
             "",
             "@",
             "",
             "<<build-function, eval=false, echo=false>>=",
             "## You can use this function to \"build\" the RTF file",
             "build <- function() {",
             sprintf("    Sweave(\"%s\",", file.Rrtf),
             "           RweaveRtf(),",
             "           SweaveSyntaxRtf,",
             sprintf("           prefix.string = \"%s\")", prefix.string),
             "}",
             "@",
             "",
             "}")

    writeLines(out, file.Rrtf)

    out <- c("\\ansi\\deff0",
             sprintf("{\\fonttbl{\\f0\\froman %s;}{\\f1\\fswiss %s;}{\\f2\\fmodern %s;}}",
                     serif.font, sans.font, fixedwidth.font),
             sprintf("\\paperw%d \\paperh%d \\margb%d \\margl%d \\margt%d \\margr%d",
                     1440 * 8.5,
                     1440 * 11,
                     as.integer(1440 * margins[1]),
                     as.integer(1440 * margins[2]),
                     as.integer(1440 * margins[3]),
                     as.integer(1440 * margins[4])),
             if (pnumbers == "y") {
                 c("{\\footerf\\pard{\\fs20 \\f1\\qc\\chpgn\\par}}",
                   "{\\footer\\pard{\\fs20 \\f1\\qc\\chpgn\\par}}")
             } else {
                 ""
             }
             )
    writeLines(out, file.header)
             
}
