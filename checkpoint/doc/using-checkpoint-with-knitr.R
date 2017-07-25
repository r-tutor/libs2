## ---- echo=FALSE, comment=""---------------------------------------------
cat("```{r, include=FALSE}", '
# write a manifest to local folder
cat("
library(darts)
",
file = "manifest.R")
', "```", sep = "")

## ---- echo=FALSE, comment=""---------------------------------------------
oldLibPaths <- .libPaths()

cat("```{r, include=FALSE}", '
# Create .checkpoint folder (in tempdir for this example)
td <- tempdir()
dir.create(file.path(td, ".checkpoint"), recursive = TRUE, showWarnings = FALSE)

# Create the checkpoint
library(checkpoint)
checkpoint("2017-03-28", checkpointLocation = td)
', "```", sep = "")

## ----checkpoint, warning=FALSE-------------------------------------------
# write a manifest to local folder
cat('
library(darts)
',
file = "manifest.R")

# Create .checkpoint folder (in tempdir for this example)
dir.create(file.path(tempdir(), ".checkpoint"), recursive = TRUE, showWarnings = FALSE)
options(install.packages.compile.from.source = "no")

# Create the checkpoint
library(checkpoint)
checkpoint("2017-03-28", checkpointLocation = tempdir())


## ---- eval=FALSE---------------------------------------------------------
#  .libPaths()
#  ## [1] ".../Temp/RtmpIVB6bI/.checkpoint/2017-03-28/lib/x86_64-w64-mingw32/3.3.2"
#  ## [2] ".../Temp/RtmpIVB6bI/.checkpoint/R-3.3.2"

## ------------------------------------------------------------------------
installed.packages()[, "Package"]

## ---- warning=FALSE, fig.asp=4/3-----------------------------------------
# Example from ?darts
library(darts)
x = c(12,16,19,3,17,1,25,19,17,50,18,1,3,17,2,2,13,18,16,2,25,5,5,
      1,5,4,17,25,25,50,3,7,17,17,3,3,3,7,11,10,25,1,19,15,4,1,5,12,17,16,
      50,20,20,20,25,50,2,17,3,20,20,20,5,1,18,15,2,3,25,12,9,3,3,19,16,20,
      5,5,1,4,15,16,5,20,16,2,25,6,12,25,11,25,7,2,5,19,17,17,2,12)
mod = simpleEM(x, niter=100)
e = simpleExpScores(mod$s.final)
oldpar <- par(mfrow=c(1, 2))
drawHeatmap(e)
drawBoard(new=TRUE)
drawAimSpot(e, cex = 5)
par(oldpar)

## ---- include=TRUE-------------------------------------------------------
# clean up

detach("package:darts", unload = TRUE)
unlink("manifest.R")
unlink(file.path(tempdir(), ".checkpoint"), recursive = TRUE)
unCheckpoint(oldLibPaths)
.libPaths()

