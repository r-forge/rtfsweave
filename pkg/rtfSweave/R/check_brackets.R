check_brackets <- function(filename) {
    if (!(length(filename) == 1L & is.character(filename))) {
        stop(sQuote("filename"), " must be a length-one character vector")
    }
    out <- .C(check_rtf_brackets, as.character(filename), 99L)
    out$status_code
}    
