check_brackets <- function(filename) {
    if (length(filename) > 1L) {
        message("Only checking first filename given")
    }
    out <- .C(check_rtf_brackets, as.character(filename))
    invisible(NULL)
}    
