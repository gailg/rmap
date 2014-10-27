\name{piHatFn}
\alias{piHatFn}

\title{
Computes piHat, the estimated probability of getting disease in the duration of 
the study for each risk group.
}
\description{
This function computes piHat, which is the estimated probability of getting 
disease in (0, t*], the duration
of the study.  This function is usually called internally by 
\code{riskValidate} or \code{performanceDifference}.  
}
\usage{
piHatFn(baseArgs = FALSE, extraArgs = FALSE)
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
See equation (90) in "rmap-formulas-v01.pdf" from the website.
}
\value{
A vector, with one value of piHat for each risk group.  This vector has names c("k1", "k2", ...)
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

ba = baseArgsFn(
  e = d$d$e, t = d$d$t, r = d$d$r, design = list(c = d$d$c, N = d$N), 
  riskGroup = list(K = 2), rSummary = "median", bootstrap = FALSE)
ph1 = piHatFn(ba)
ph1

set.seed(1)
d = df_randomSample()
ba = baseArgsFn(
  e = d$e, t = d$t, r = d$r, design = "randomSample", 
  riskGroup = list(K = 3), rSummary = "median", bootstrap = FALSE)
ph2 = piHatFn(ba)
ph2
}
