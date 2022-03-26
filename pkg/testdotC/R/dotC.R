dotC <- function(filename) {
    .C("print123", PACKAGE = "testdotC")
}
                       
