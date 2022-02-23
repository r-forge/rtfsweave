## Test whether curly brackets are escaped when code chunks have `echo = true`

require(rtfSweave)
require(utils)
require(tools)

temp.rtf <- tempfile(fileext = ".rtf")

## The manual write, "The contents of the inst subdirectory will be copied recursively to
## the installation directory." So when using `system.file` don't specify `inst`

Sweave(file = system.file("tests", "escape-curly-brackets.Rrtf", package = "rtfSweave"),
       driver = RweaveRtf(),
       syntax = SweaveSyntaxRtf,
       output = temp.rtf)


hash.observed <- md5sum(temp.rtf)
hash.expected <- md5sum(system.file("tests", "escape-curly-brackets.rtf", package = "rtfSweave"))

stopifnot(hash.observed == hash.expected)
