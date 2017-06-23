\name{riskCdfRawFn}
\alias{riskCdfRawFn}

\title{
Plot a *raw* riskCdf.
}
\description{
This function draws a *raw* riskCdf.  It is a "raw" function
because it requires \code{setUpTrellisFn} to be called first
to set up the plotting region(s).

For a more user-friendly riskCdf see \code{riskCdf}, which sets
up the plotting region, then draws riskCdf(s).  This function is
designed to allow the user to create sophisticated trellis-style
graphics.
}
\usage{
riskCdfRawFn(pos, x1, a1, col = "#B23AEE", pch = 20, cex = 1)
}

\arguments{
  \item{pos}{
An integer vector with two elements.  Before this function is 
called, \code{setUpTrellisFn} must be called, which sets up a 
"grid" of graphing regions.  This \code{pos} argument specifies
the row and column in which the individualized attribute diagram 
should be drawn.
}
  \item{x1}{
An object of class crp is a list containing two (named) vectors.  
The first vector, named CRP, is a numeric vector containing the CRP
of each affected subject.  x1 is the first vector of an object of 
class crp.
}
  \item{a1}{
The second vector of an object of class crp, named a, is a numeric
vector containing the two-stage sampling weights corresponding to the
elements in the first vector CRP.
}
  \item{col}{
The color on the plot that represents this crp.
}
  \item{pch}{
The plotting character on the plot that represents this crp.
}
  \item{cex}{
The number that determines the size of the plotting character.
}
}

\value{
A named list containing the elements x1 and a1.
}
\references{
For more details and examples, see the package help page at:
http://www.stanford.edu/~ggong/rmap/index.html
}
\author{
David Johnston and Gail Gong
}

\examples{
  set.seed(1)
  sampleData = df_randomSample_r1_r2(NTotal = 500)
  riskGroup = list(
    ungrouped = list(epsilon = 0.15, tStar = 10))
  
  rvu1b = riskValidateUngrouped(
    e = sampleData$e, t = sampleData$t, r = sampleData$r1,
    design = "randomSample", riskGroup = riskGroup, bootstrap = 20,
    rvpar = FALSE, multicore = FALSE, verbose = TRUE)
  
  rvu2b = riskValidateUngrouped(
    e = sampleData$e, t = sampleData$t, r = sampleData$r2,
    design = "randomSample", riskGroup = riskGroup, bootstrap = 20,
    rvpar = FALSE, multicore = FALSE, verbose = TRUE)
  
  crps = list(model1 = rvu1b$AUC$CRP, model2 = rvu2b $AUC$CRP)
  rvpar = rvparFn(xlab = "CRP (\%)", ylab = "F(CRP) (\%)", col = c("red", "blue"))
  setUpTrellisFn(1, 1, main = "Comparing CRPs of two models using cdfs",
    xlab = rvpar$xlab, ylab = rvpar$ylab, inflate = 1.02)
  nModels = length(crps)
  pch = rvpar$pch + rep(0,length(crps))
  cex = rvpar$cex + rep(0,length(crps))
  sapply(seq_along(crps), function(i) {
    riskCdfRawFn(
      pos = c(1, 1), 
      x1 = crps[[i]]$CRP, a1 = crps[[i]]$a,
      col = rvpar$col[i], 
      pch = pch[i], 
      cex = cex[i])
  })
  
  downViewport(paste(1, 1))
  
  legendvp = viewport(
    x = unit(1, "lines"),
    y = unit(1, "npc") - unit(1, "lines"),
    just = c("left", "top"),
    height = unit(nModels + 1, "lines"),
    width = max(stringWidth(names(crps))) + unit(3, "lines"),
    name = "legendvp")
  pushViewport(legendvp)
  grid.rect()
  
  grid.text(
    label = names(crps),
    x = unit(2, "lines"),
    y = unit(nModels:1, "lines"),
    just = "left")
  
  grid.points(
     x = unit(rep(1, nModels), "lines"),
     y = unit(nModels:1, "lines"),
     size = unit(cex, "char"), pch = pch, 
     gp = gpar(col = rvpar$col))
      
}
