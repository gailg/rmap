\name{V2StageFn}
\alias{V2StageFn}

\title{
Calculate V2Stage.
}
\description{
V2Stage is the covariance matrix for c(gammaHat[1], ..., gamma[K-1], lambdaHat) under
two stage sampling.  
It is an intermediate step in the calculation of Sigma which is used to obtain
confidence intervals for pi and AUC and to obtain the Hosmer-Lemeshow
statistic. This function is usually called internally by 
\code{riskValidate} or \code{performanceDifference}.  
}
\usage{
V2StageFn(baseArgs = FALSE, extraArgs = FALSE)
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
See Equations (80-84) in "rmap-formulas-v01.pdf" from the website.
}
\value{
A symmetric square matrix with numbers of rows and columns equal to
(K-1) + 2 * (M_1 + ... + M_K).  This matrix is the covariance of the following 
random variables where the subscripts for lambdaHat are ordered so that 
k grows most slowly, e grows next most slowly, and m grows fastest:
{gammaHat_k : k = 1, ..., K-1}, 
{lambdaHat_{k,e,m} : k = 1, ... K; e = 1,2; m = 1, ..., M_k}
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
  options(digits = 3)
  options(scipen = 10)
  set.seed(5)
  ddd = df_twoStage(NTotal = 10, KKK = 2)
  d = ddd$d
  d[9,] = d[1,]
  d[9,]$e = 0
  d[9,]$k = 1
  d[1,]$e = 0
  d[7,]$e = 0
  d[3,]$e = 2
  d[8,]$e = 0
  d[8,]$c = "B"
  rownames(d) = NULL
  d  #A very doctored data set that will give a tiny example with quite general features.
  N = ddd$N
  N["B"] = 6
  N  #First stage numbers
  n = ddd$n
  n["B"] = 3
  n  #Second stage numbers
  d_k_equals_1 = cbind(d[d$k == 1,c("e", "t", "c")], aaa = N[d[d$k == 1,]$c]/n[d[d$k == 1,]$c], k = 1)
  d_k_equals_1 #The relevant columns of d for k = 1.
  d_k_equals_2 = cbind(d[d$k == 2,c("e", "t", "c")], aaa = N[d[d$k == 2,]$c]/n[d[d$k == 2,]$c], k = 2)
  d_k_equals_2 #Ditto for k = 2.
  baseArgs = baseArgsFn(e = d$e, t = round(d$t,2), r = round(d$r,2), design = list(N = N, c = d$c),
    riskGroup = list(k = d$k), rSummary = "median", bootstrap = FALSE)
  V2StageFn(baseArgs)
}
