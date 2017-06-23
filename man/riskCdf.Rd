\name{riskCdf}
\alias{riskCdf}
\title{
Cumulative distribution function of the CRPs
}
\description{
Draws a plot to allow comparison of the cumulative distribution
function of the CRPs of two or more models.  A model with high CRPs has high discrimination.
}
\usage{
riskCdf(crps, 
  rvpar = rvparFn(xlab = "CRP (\%)", ylab = "F(CRP) (\%)"), 
  ocall = NULL)
}

\arguments{
  \item{crps}{
A list containing elements of class crp.  If rvu is of class rvu
then rvu$AUC$CRP is of class crp.
}
  \item{rvpar}{
A named list of class "rvpar" that contains fifteen elements: 
col, lightCol, xmax, ymax, inflate, comment, annotate, pch, cex, 
axesInPercents, atX, atY, xlab, ylab, main. Such an object is the
the output of the function \code{rvparFn}.  
See \code{?rvparFn} for more information. 
}
  \item{ocall}{
A function argument that allows the user to see the original 
function call in the box under the attribute diagram.  This 
argument should be left as the default value. 
}
}

\value{
The list crps.
}
\references{
For more details and examples, see the package help page at:
http://www.stanford.edu/~ggong/rmap/index.html
}
\author{
David Johnston and Gail Gong
}

\examples{
library(rmap)
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

riskCdf(
  crps = list(model1 = rvu1b$AUC$CRP, model2 = rvu2b $AUC$CRP), 
  rvpar = rvparFn(col = c("red", "blue"), cex = .5,
    main = "Figure 6: Empirical cdf comparing the two models",
    xlab = "CRP (\%)", ylab = "F(CRP) (\%)"))
}


