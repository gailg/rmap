\name{SigmaFn}
\alias{SigmaFn}

\title{
Calculate Sigma, the covariance matrix of (gammaHat[1], ..., gammaHat[K-1], piHat).
}
\description{
Sigma is one of the important products of the rmap package.  
It is gotten by calculating (1) the asymptotic covariance matrix of the 
c(gammaHat[1], ..., gammaHat[K-1], lambda) under random sampling, 
(2) adjusting for two-stage 
sampling if appropriate, and (3) transforming the covariance matrix of 
(gammaHat[1], ..., gammaHat[K-1], piHat) using the delta method.
From Sigma, we can obtain
confidence intervals for piHat.  Sigma is also an intermediate step for obtaining
confidence intervals for the AUC and the Hosmer-Lemeshow statistic.
}
\usage{
SigmaFn(baseArgs = FALSE, extraArgs = FALSE)
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
A symmetric square matrix with numbers of rows and columns equal to 
2 * K - 1. See Equation (94) in "rmap-formulas-v01.pdf" from the website.
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
d = df_twoStage()
ba = baseArgsFn(e = d$d$e, t = d$d$t, r = d$d$r, design = list(c = d$d$c, N = d$N), riskGroup = list(K = 2),
  rSummary = "median", bootstrap = FALSE)
ph1 = piHatFn(ba)
ph1
Sigma1 = SigmaFn(ba)
Sigma1
set.seed(1)
d = df_randomSample()
ba = baseArgsFn(e = d$e, t = d$t, r = d$r, design = "randomSample", riskGroup = list(K = 3),
  rSummary = "median", bootstrap = FALSE)
ph2 = piHatFn(ba)
ph2
Sigma2 = SigmaFn(ba)
Sigma2
}
