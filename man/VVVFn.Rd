\name{VVVFn}
\alias{VVVFn}

\title{
Compute VVV. 
}
\description{
VVV is the covariance matrix for c(gammaHat[1], ..., gamma[K-1], lambdaHat) under random sampling.
It is an intermediate step in the calculation of Sigma which is used to obtain
confidence intervals for pi and AUC and to obtain the Hosmer-Lemeshow
statistic. This function is usually called internally by 
\code{riskValidate} or \code{performanceDifference}.  
}
\usage{
VVVFn(baseArgs = FALSE, extraArgs = FALSE)
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
See equations (51), (75), and (79) in "rmap-formulas-v01.pdf" from the website.
}
\value{
A block-diagonal matrix.  Define K to be the number of risk groups.  Define
M_k to be the length of tau for risk group k (see lambdaHatFn).
Down the diagonal of VVV are K+1 submatrixes or boxes.
The first box has dimension
(K-1) x (K-1) and is given by equation (51) in "rmap-formulas-v01.pdf" from the website.
The second box is for k = 1, and so on until the (K+1)th box for k = K
The kth box has row headings
{(e1,m)| e1 = 1, 2 and m = 1, ... M_k} where e1 grows more slowly than m, and
column headings {(e2,m)| e2 = 1, 2 and m = 1, ... M_k}. The entry in each 
cell in the box is V_{k, e1, e2, m} given in Equation (75) in "rmap-formulas-v01.pdf" from the website.
  
The number of rows and columns of VVV is (K-1) + 2 * (M_1 + ... + M_K).
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
  lambdaHat = lambdaHatFn(baseArgs)
  lambdaHat[["k1"]]$tau
  lambdaHat[["k2"]]$tau
  VVV = VVVFn(baseArgs)
  VVV #The column and row labels are c(gamma1, c(kem = 111,112,113,121,122,123,211,212,221,222))
}
