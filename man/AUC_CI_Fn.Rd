\name{AUC_CI_Fn}
\alias{AUC_CI_Fn}
\title{
AUC 95\% confidence interval.
}
\description{
Computes a 95 percent confidence interval for the
  Area Under the Receiver Operating Characteristic Curve. If the model
  is grouped, use the delta method as described on pp 14-17 in (108)
  in rmap-formulas-v02.pdf, available from 
  http://www.stanford.edu/~ggong/rmap/index.html.
  If the mdoel is ungrouped, use the
  bootstrap.
}
\usage{
AUC_CI_Fn(baseArgs = FALSE, extraArgs = FALSE)
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
A numeric vector with two elements.  The first element is the lower bound of 
a 95\% 
confidence interval for AUC.  The second element is a the upper bound.

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
  
  riskGroup = list(K = 3)

  baseArgs = baseArgsFn(e = sampleData$e, t = sampleData$t, r = sampleData$r1,
    design = "randomSample", riskGroup = riskGroup, rSummary = "mean",
    bootstrap = FALSE)
  AUC_CI_Fn(baseArgs)

  tStar = 10
  epsilon = nrow(sampleData)^(-1/3)
  epsilon
  riskGroup = list(ungrouped = list(tStar = tStar, epsilon = epsilon))
  
  baseArgsUngrouped = baseArgsFn(e = sampleData$e, t = sampleData$t, 
    r = sampleData$r1,
    design = "randomSample", riskGroup = riskGroup, rSummary = "mean",
    bootstrap = FALSE, multicore = FALSE, verbose = TRUE)

  AUC_CI_Fn(baseArgs = baseArgsUngrouped)
}
