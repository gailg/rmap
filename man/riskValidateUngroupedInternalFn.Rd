\name{riskValidateUngroupedInternalFn}
\alias{riskValidateUngroupedInternalFn}
\title{
Validate a risk model for ungrouped cohort name (internal).
}
\description{
  This function is very similar to the function
  'riskValidateUngroupedExternalFn'. Unlike
  'riskValidateUngroupedExternalFn', this function expects its inputs to
  be "massaged" by baseArgsFn. The function 'riskValidateUngrouped' is
  the top level function and is designed to be called by package users.
  If you are a user of this package, chances are you want to use the
  function 'riskValidateUngrouped' and not this function.
}
\usage{
riskValidateUngroupedInternalFn(baseArgs = FALSE, extraArgs = FALSE)
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
  An object of type rvu, which is a list that contains the
  elements AUC (an object from AUCInternalFn) and PNN (an object from
  piHatNNInternalFn). 
  + AUC is a list that contains three elements: (1) AUC is a numerical
    value equal to the Area Under the Receiver Operating
    Characteristic curve. (2) AUC_CI is a numerical vector of length 2
    containing the lower and upper limits of the 95 percent confidence
    interval for the AUC. (3) CRP is a list containing two lists, each
    of length equal to the number of cases and each element in the two
    lists corresponding to a case; the first sublist of CRP
    is also called CRP and contains the Case Risk Percentiles; the
    second sublist of CRP is called a and contains the two-stage
    sampling weights of the cases.

  + PNN is a list of matrices. The first matrix is named est and has
    two columns each of length equal to the number of distinct values
    of assigned risk.  The first column is rho and equal to the sorted
    distinct values of assigned risk.  The second column is piHatNN
    contains the nearest neighbor estimate of outcome probability at
    each rho.
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
AUCInternalFn and piHatNNInternalFn
}
\examples{
  set.seed(1)
  sampleData = df_randomSample_r1_r2(NTotal = 200)
  
  tStar = 10
  epsilon = nrow(sampleData)^(-1/3)
  epsilon
  riskGroup = list(ungrouped = list(epsilon = epsilon, tStar = tStar))
  
  baseArgs = baseArgsFn(e = sampleData$e, t = sampleData$t, r = sampleData$r1,
    design = "randomSample", riskGroup = riskGroup, rSummary = "mean",
    bootstrap = FALSE, multicore = FALSE, verbose = TRUE)
  
  rvu = riskValidateUngroupedInternalFn(baseArgs = baseArgs)
  rvu
  
  baseArgsB = baseArgsFn(e = sampleData$e, t = sampleData$t, r = sampleData$r1,
    design = "randomSample", riskGroup = riskGroup, rSummary = "mean",
    bootstrap = 20, multicore = FALSE, verbose = TRUE)
  
  rvub = riskValidateUngroupedInternalFn(baseArgs = baseArgsB)
  rvub
}
