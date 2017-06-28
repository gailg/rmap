\name{DerFn}
\alias{DerFn}

\title{
Compute Der.  
}
\description{
 This function is usually called internally by 
\code{riskValidate} or \code{performanceDifference}.  
This function computes Der,  
the derivative of c(gammaHat[1], ..., gamma[K-1], piHat) with respect to 
c(gammaHat[1], ..., gamma[K-1], lambdaHat). Der is a
matrix used in the delta method to
compute Sigma, the covariance matrix for 
parameters c(gammaHat[1], ..., gamma[K-1], piHat).  
}
\usage{
DerFn(baseArgs = FALSE, extraArgs = FALSE)
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
A matrix.
Define K to be the number of risk groups.  There is a column for each parameter 
(gammaHat[1], ..., gamma[K-1], piHat) for a total of 2 * K - 1 columns.  Define M_k to be the 
length of tau (see lambdaHatFn) for the kth risk group.  The number of rows in this
matrix will be K - 1 + 2 * (M_1 + ... + M_K) .  
See equation (92) in "rmap-formulas-v01.pdf" from the website.
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
  Der = DerFn(baseArgs)
  Der
}
