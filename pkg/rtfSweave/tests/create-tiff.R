## Test whether I create a compressed TIFF file when `tiff = true`

library(rtfSweave)
library(tools) # for md5sum

setwd(tempdir())

temp.rtf <- tempfile(fileext = ".rtf")
expected.tiff <- sub("\\.rtf$", "-001.tiff", temp.rtf)

Sweave(file = system.file("tests", "create-tiff.Rrtf", package = "rtfSweave"),
       driver = RweaveRtf(),
       syntax = SweaveSyntaxRtf,
       output = temp.rtf)

stopifnot(file.exists(temp.rtf))
stopifnot(file.exists(expected.tiff))

observed.tiff <- tempfile(fileext = ".tiff")

## Make the tiff I *should* get using the code in create-tiff.Rrtf
tiff(observed.tiff,
     height = 4,
     width = 5,
     units = "in",
     res = 100,
     compression = "lzw")

plot(1,1)
dev.off()

stopifnot(file.exists(observed.tiff))

stopifnot(md5sum(expected.tiff) == md5sum(observed.tiff))


file.remove(c(temp.rtf, expected.tiff, observed.tiff))

