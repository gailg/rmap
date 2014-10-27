\name{CRP_Fn}
\alias{CRP_Fn}
\title{
Calculate the Casse Risk Percentiles (CRP)
}
\description{
Calculates the Case Risk Percentiles.  For each
  case, calculate its risk percentile among the risks of the
  controls.  A case is defined to be a subject (e = 1) observed to become
  affected with disease during the duration fo the study.  A control
  is defined to be a subject who is known to die from other causes (e
  = 2) or known to survive throughout the duration of the study
  without disease or death from other causes (e = 2 or (e = 0 and t >=
  tStar)). The calculations here take into account two-stage sampling
  if appropriate.
}
\usage{
CRP_Fn(baseArgs = FALSE, extraArgs = FALSE)
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
\item CRP: A vector of numbers between 0 and 1. The Case Risk
    Percentiles;  for each case, its CRP is the percentile of the
    cases' assigned risk among assigned risks of the controls. 
\item a: A vector of numbers between 0 and 1. The two-stage sampling
    weights of the cases, in the same order as that of CRP.
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

  crp = CRP_Fn(baseArgs)
  lapply(crp, head)
}
