## A `toRTF` method for a `sessionInfo` object
## ===========================================
##
## Started March 27, 2016
##
## * TODO: Enhancements
## - [ ] Eventually pull from rtf.options
## - [ ] See what pander.sessionInfo does

toRTF.sessionInfo <- function (object, locale = TRUE, ...) {
    decorate <- function(x, commands = "\\i"){
        paste0("{", commands, " ", x, "}")
    }
    opkgver <- sapply(object$otherPkgs, function(x) x$Version)
    nspkgver <- sapply(object$loadedOnly, function(x) x$Version)
    z <- c("{\\pard \\f0",
           paste0("{\\b ", 
                  object$R.version$version.string,
                  "} on ",
                  "{\\f2 ",
                  object$R.version$platform,
                  "}; running under ",
                  object$running),
           "\\par}")
    if (locale) {
        z <- c(z,
               "{\\pard \\f0 ",
               paste0("{\\f2 ",
                      gsub(";", "},\n {\\\\f2 ", object$locale),
                      "}"),
               "\\par}")
    }
    z <- c(z,
           "{\\pard \\f0 {\\b Base packages:}",
           paste0(decorate(sort(object$basePkgs), "\\i"),
                  collapse = ", "),
           "\\par}")

    if (length(opkgver)) {
        opkgver <- opkgver[sort(names(opkgver))]
        z <- c(z,
               "{\\pard \\f0 {\\b Other packages:}",
               paste(decorate(names(opkgver), "\\i"), opkgver, sep = "\\~v",
                     collapse = ", "),
               "\\par}")
                       
    }
    if (length(nspkgver)) {
        nspkgver <- nspkgver[sort(names(nspkgver))]
        z <- c(z,
               "{\\pard \\f0 {\\b Loaded via a namespace (and not attached):}",
               paste(decorate(names(nspkgver), "\\i"), nspkgver, sep = "\\~v",
                     collapse = ", "),
               "\\par}")
    }
    class(z) <- "RTF"
    z
}

