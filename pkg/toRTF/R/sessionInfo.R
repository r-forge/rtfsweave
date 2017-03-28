## A `toRTF` method for a `sessionInfo` object
## ===========================================
##
## Started March 27, 2016
##
## * TODO: Enhancements
## - [ ] Pull from rtf.options
## - [ ] See what pander.sessionInfo does

toRTF.sessionInfo <- function (object, locale = TRUE, ...) {
    opkgver <- sapply(object$otherPkgs, function(x) x$Version)
    nspkgver <- sapply(object$loadedOnly, function(x) x$Version)
    z <- c("{\\pard \\f0",
           paste0("{\\b  ", 
                  object$R.version$version.string,
                  "}, ",
                  "{\f2 ",
                  object$R.version$platform,
                  "}"),
           "\\par}")
    if (locale) {
        z <- c(z,
               "{\\pard \\f0 ",
               paste0("{\\f2 ",
                      gsub(";", "},\n{\\\\f2 ", object$locale),
                      "}"),
               "\\par}")
    }
    z <- c(z,
           "{\\pard \\f0 {\\b Base packages:} {\\i ",
           paste0(sort(object$basePkgs), collapse = ", "),
           "}\\par}")

    if (length(opkgver)) {
        opkgver <- opkgver[sort(names(opkgver))]
        z <- c(z,
               "{\\pard \\f0 {\\b Other packages:} {\\i ",
               paste(names(opkgver), opkgver, sep = "\\~", collapse = ", "),
               "}\\par}")
                       
    }
    if (length(nspkgver)) {
        nspkgver <- nspkgver[sort(names(nspkgver))]
        z <- c(z,
               "{\\pard \\f0 {\\b Loaded via a namespace (and not attached):} {\\i ",
               paste(names(nspkgver), nspkgver, sep = "~", collapse = ", "),
               "}\\par}")
    }
    z <- c(z, "\\end{itemize}")
    class(z) <- "RTF"
    z
}
