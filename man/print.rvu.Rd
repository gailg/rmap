\name{print.rvu}
\alias{print.rvu}
\title{
Display rvu objects in a pretty way.
}
\description{
riskValidateUngrouped produces an object of class rvu.  This class
is a list with two elements: AUC and PNN. AUC is a list with three 
elements: the AUC estimate, a 95 percent confidence interval, and 
the CRP vector.  PNN is a list of matrices: the est matrix has two 
named columns rho and piHatNN; the confBand matrix has two named 
columns "2.5\%" and "97.5\%".  This function displays all this 
information in a readable way.
}
\usage{
print.rvu(rvu, n = 6, ...)
}

\arguments{
  \item{rvu}{
An object of class rvu.  Such an object is produced by 
riskValidateUngrouped.
}
  \item{n}{
A positve integer describing how many items are returned by head.
}
  \item{\dots}{
further arguments passed to or from other methods.
}
}
\details{

}

\references{
For more details and examples, see the package help page at:
http://www.stanford.edu/~ggong/rmap/index.html

}
\author{
David Johnston and Gail Gong

}
\note{

}



\seealso{

}
\examples{
  set.seed(1)
  sampleData = df_randomSample_r1_r2(NTotal = 200)
  rvu1 = riskValidateUngrouped(
    e = sampleData$e, 
    t = sampleData$t, 
    r = sampleData$r1,
    design = "randomSample", 
    riskGroup = list(ungrouped = list(epsilon = 0.15,  tStar = 10)),
    rvpar = FALSE)
  rvu1
  
  rvu1b = riskValidateUngrouped(
    e = sampleData$e, 
    t = sampleData$t, 
    r = sampleData$r1,
    design = "randomSample", 
    riskGroup = list(ungrouped = list(epsilon = 0.15,  tStar = 10)),
    bootstrap = 20,
    rvpar = FALSE, 
    multicore = FALSE, 
    verbose = TRUE)
  rvu1b
}
