\name{AUCInternalFn}
\alias{AUCInternalFn}
\title{
Calculate the AUC for ungrouped data, a 95 percent confidence interval and the CRPs.
}
\description{
  Coordinates the calculation of the Area Under the
  Receiver Operating Characteristic curve for ungrouped data. This
  function provides the AUC, an 95 percent confidence interval for the
  AUC, and the case risk percentiles.
}
\usage{
AUCInternalFn(baseArgs = FALSE, extraArgs = FALSE)
}

\arguments{
  \item{baseArgs}{
A list that has class attribute 'baseArgs'.  This list hase named elements:
e, t, r, c, k, K, epsilon, tStar, ungrouped, N, n, rSummary, nBootstraps,
multicore, verbose.  This list is the output of the function \code{baseArgsFn}.
The elements of this list are the individual components that the low-level
functions in 'rmap' require.  A description of each of these 15 elements can
be found in \code{?baseArgsFn}.

}
  \item{extraArgs}{
A list of named elements, which can contain: gammaHat, lambdaHat, piHat,
Sigma, crp, and auc. This argument allows this function to take a "shortcut",
using intermediate values that have previously been calculated.

}
}
\details{

}
\value{
  A list containing 
\itemize{
  \item AUC: a real number between 0 and 1; the Area Under the
    Receiver Operating Characteristic curve.
  \item AUC_CI: a named vector of two real numbers representing the lower
    and upper limits of the 95 percent confidence interval for the AUC.
  \item  CRP:  A list that contains two lists: (1) CRP$CRP are the Case Risk
    Percentiles; for each case, its CRP is the percentile of the
    case's assigned risk among assigned risks of the controls. (2)
    CRP$a are the two-stage sampling weights of the cases, in the same
    order as that of CRP$CRP.
}
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
  
  tStar = 10
  epsilon = nrow(sampleData)^(-1/3)
  epsilon
  riskGroup = list(ungrouped = list(tStar = tStar, epsilon = epsilon))
  
  baseArgs = baseArgsFn(e = sampleData$e, t = sampleData$t, r = sampleData$r1,
    design = "randomSample", riskGroup = riskGroup, rSummary = "mean",
    bootstrap = FALSE, multicore = FALSE, verbose = TRUE)
  auc = AUCInternalFn(baseArgs = baseArgs)
  names(auc)
  auc$AUC
  auc$AUC_CI
  lapply(auc$CRP, head)
}
