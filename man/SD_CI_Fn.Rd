\name{SD_CI_Fn}
\alias{SD_CI_Fn}

\title{
Compute a 95\% confidence interval for SD
}
\description{
This function computes a 95\% confidence interval for SD, the standard deviation of the model.  
The variance used to compute the confidence interval for SD is obtained from 
equation (151) in "rmap-formulas-v01.pdf" from the website.
}
\usage{
SD_CI_Fn(baseArgs = FALSE, extraArgs = FALSE)
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
A numeric vector with two elements: the lower and upper bound of the confindence
interval. 
}
\references{
For more details and examples, see the package help page at:
http://www.stanford.edu/~ggong/rmap/index.html

}
\author{
David Johnston and Gail Gong

}
\note{
SD_CI_Fn does not apply when there is only one risk group.  
}



\seealso{
\code{SD_Fn}
}
\examples{
# random sampled data:
set.seed(1)
d_rs = df_randomSample()
ba = baseArgsFn(e = d_rs$e, t = d_rs$t, r = d_rs$r, design = "randomSample",
  riskGroup = list(K = 3), rSummary = "median", bootstrap = 300)

SD_CI_1 = SD_CI_Fn(ba)
SD_CI_1

# two-stage sampled data:
set.seed(1)
d_ts = df_twoStage()
ba = baseArgsFn(e = d_ts$d$e, t = d_ts$d$t, r = d_ts$d$r,
  design = list(c = d_ts$d$c, N = d_ts$N), riskGroup = list(k = d_ts$d$k),
  rSummary = "median", bootstrap = 300)

SD_CI_2 = SD_CI_Fn(ba)
SD_CI_2


}
