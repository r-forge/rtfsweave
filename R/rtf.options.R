## I have set up a option system like ps.options()
## For simplicity, it's a "flat" system that doesn't use an options heirarchy
## To set an option, you might do this
## R> rtf.options(table.cell.pointsize = 11)


## This is an environment to hold the options objects
.RTFenv <- new.env()

## It's empty
## ls(envir = .RTFenv, all.names = TRUE)

## I am choosing to have a list that is flat so that I can use the ps.options approach
## and grDvices:::check.options rather than a hierarchical list like lattice uses. 
assign(".RTF.Options.default", 
       list(table.row.align = "c",
            table.cell.font = 1L,
            table.cell.pointsize = 11L,
            table.cell.color.foreground = 0L,
            table.cell.space.before = 3L,
            table.cell.space.after = 3L,
            table.cell.space.between = 4L,
            table.cell.shading = "transparent"),
       envir = .RTFenv)
assign(".RTF.Options", get(".RTF.Options.default", envir = .RTFenv), envir = .RTFenv)

## Show the list of options
## get(".RTF.Options", envir = .RTFenv)
## get(".RTF.Options.default", envir = .RTFenv)

## This is from 'lattice.getOption'
## This allows me to get at two levels (maximum) for options. Good enough. [Huh??]
rtf.getOption <- function(name){
    get(".RTF.Options", envir = .RTFenv)[[name]]
}

## rtf.getOption("table.cell.font")
## rtf.getOption("table.cell.pointsize")

## This needs to be able to accommodate new options unlike 'ps.options'
## but like 'lattice.options'. But I can't read 'lattice.options'.
## 
## Not sure how to do this with environments and such

## If called without arguments return the current state
## if called without arguments except 'reset = FALSE' also return the current state
rtf.options <- function(..., reset = FALSE) { 

    old <- get(".RTF.Options", envir = .RTFenv)
    if (reset) {
      ## assign(".RTF.Options",
      ##        get(".RTF.Options.default", envir = .RTFenv),
      ##        envir = .RTFenv)
      .RTFenv$.RTF.Options <- .RTFenv$.RTF.Options.default
    }
    new <- list(...)
    l... <- length(new)
    if (l... == 0) {
      invisible(old)
    }
    ## Commented out since check.options won't allow new options
    ## ---------------------------------------------------------
    ## check.options(new, name.opt = ".RTF.Options", envir = .RTFenv,
    ##               assign.opt = l... > 0, override.check = TRUE)

    ## Note: 'modifyList' is in utils which the rtfSweave package imports
    ## so can assume it's available
    both <- modifyList(old, new)
    assign(".RTF.Options", both, envir = .RTFenv)
    if (reset || l... > 0) 
        invisible(both)
    else
      both
}

assign(".RTF.Options", get(".RTF.Options.default", envir = .RTFenv), envir = .RTFenv)
str(rtf.options())
rtf.options(pumpkin = "blah")
rtf.getOption("pumpkin")
rtf.options(reset = TRUE)  ## This does not work!!!
str(rtf.options())
rtf.getOption("pumpkin")

str(.RTFenv$.RTF.Options)
assign(".RTF.Options", get(".RTF.Options.default", envir = .RTFenv), envir = .RTFenv)

rtf.options(pumpkin = 3, reset = TRUE)

rtf.options(yello = TRUE, blue = FALSE)
str(rtf.options())
