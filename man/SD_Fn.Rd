\name{SD_Fn}
\alias{SD_Fn}

\title{
Compute SD, the estimated standard deviation of the model
}
\description{
Computes SD, the estimated standard deviation of the model.  
See equation (136) in "rmap-formulas-v01.pdf" from the website.  This statistic measures the 
variability in piHat.  

}
\usage{
SD_Fn(baseArgs = FALSE, extraArgs = FALSE)
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
A single numeric value representing the estimated standard deviation of the 
model.  
}
\references{
For more details and examples, see the package help page at:
http://www.stanford.edu/~ggong/rmap/index.html

}
\author{
David Johnston and Gail Gong

}
\note{
This function does not apply if riskGroup has only one risk group.  
}



\seealso{
\code{SD_CI_Fn}
}
\examples{
# random sampled data:
set.seed(1)
d_rs = df_randomSample()
ba = baseArgsFn(e = d_rs$e, t = d_rs$t, r = d_rs$r, design = "randomSample",
  riskGroup = list(K = 3), rSummary = "median", bootstrap = 300)

SD1 = SD_Fn(ba)
SD1

# two-stage sampled data:
set.seed(1)
d_ts = df_twoStage()
ba = baseArgsFn(e = d_ts$d$e, t = d_ts$d$t, r = d_ts$d$r,
  design = list(c = d_ts$d$c, N = d_ts$N), riskGroup = list(k = d_ts$d$k),
  rSummary = "median", bootstrap = 300)

SD2 = SD_Fn(ba)
SD2


}
