\name{ChiSqFn}
\alias{ChiSqFn}

\title{
Compute the Hosmer Lemeshow Chi-squared goodness-of-fit statistic, and 
corresponding p-value.  
}
\description{
Estimates the Hosmer Lemeshow Chi-squared goodness-of-fit statistic and corresponding 
p-value.  
}
\usage{
ChiSqFn(baseArgs = FALSE, extraArgs = FALSE)
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
A named vector holding the Chi-squared goodness-of-fit statistic and corresponding p-value.
}
\value{
A named vector with two elements: HosmerLemeshow and HL_pval, which hold the 
value of the Hosmer Lemeshow Chi Squared goodness-of-fit statistic and the corresponding 
p-value. 
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
d = df_randomSample()
ba = baseArgsFn(
  e = d$e, t = d$t, r = d$r, design = "randomSample", 
  riskGroup = list(K = 3), rSummary = "median", 
  bootstrap = FALSE)
ChiSqFn(ba)


}
