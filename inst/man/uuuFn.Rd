\name{uuuFn}
\alias{uuuFn}

\title{
Compute uuu.
}
\description{
u_n is the vector of partial derivatives of the loglike of the nth observation
with respect to the parameters c(gamma[1], ..., gamma[K-1], lambda).
}
\usage{
uuuFn(baseArgs = FALSE, extraArgs = FALSE)
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
A matrix with one row for each observation and one column for each parameter requiring
estimation. The observations are ordered by their risk group membership beginning with
k = 1 up to k = K.  The parameters are ordered gamma_{1}, ..., gamma_{K-1}, followed
by lambda_{kem}, with k = 1, ..., K moving most slowly, e = 1, 2, moving next most
slowly, and then m = 1, ..., M_k moving the fastest.  The matrix has 
number of rows equalling the number of observations and the number of columns equalling
K-1 + 2(M_1 + ... + M_K).  See Equation (34) and (52) in "rmap-formulas-v01.pdf" from the website.
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
  uuu = uuuFn(baseArgs)
  uuu
}
