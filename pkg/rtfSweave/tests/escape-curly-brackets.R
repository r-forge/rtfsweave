## Test whether curly brackets are escaped in RTF output when code
## chunks have `echo = true`

library(rtfSweave)

temp.rtf <- tempfile(fileext = ".rtf")

## The manual write, "The contents of the inst subdirectory will be copied recursively to
## the installation directory." So when using `system.file` don't specify `inst`

Sweave(file = system.file("tests", "escape-curly-brackets.Rrtf", package = "rtfSweave"),
       driver = RweaveRtf(),
       syntax = SweaveSyntaxRtf,
       output = temp.rtf)


lines.observed <- readLines(temp.rtf)
lines.expected <- readLines(system.file("tests", "escape-curly-brackets.rtf", package = "rtfSweave"))

stopifnot(all.equal(lines.observed, lines.expected))

file.remove(temp.rtf)
