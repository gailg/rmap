\name{B2Fn}
\alias{B2Fn}

\title{
Compute B2.
}
\description{
B2 is an intermediate component used to compute Sigma
 (see \code{?Sigma}).  This function is necessary only under two-stage 
sampling, and is one of the more computationally-intensive parts of the rmap package.  
}
\usage{
B2Fn(baseArgs = FALSE, extraArgs = FALSE)
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
K - 1 + 2*(M_1 + ... M_K). See Equation (83) and (84) in "rmap-formulas-v01.pdf" from the website.
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
  d  # A very doctored data set that will give 
     # a tiny example with quite general features.
  N = ddd$N
  N["B"] = 6
  N  #First stage numbers
  n = ddd$n
  n["B"] = 3
  n  #Second stage numbers
  d_k_equals_1 = cbind(
    d[d$k == 1,c("e", "t", "c")], 
    aaa = N[d[d$k == 1,]$c]/n[d[d$k == 1,]$c], 
    k = 1)
  d_k_equals_1 #The relevant columns of d for k = 1.
  d_k_equals_2 = cbind(
    d[d$k == 2,c("e", "t", "c")], 
    aaa = N[d[d$k == 2,]$c]/n[d[d$k == 2,]$c], 
    k = 2)
  d_k_equals_2 #Ditto for k = 2.
  baseArgs = baseArgsFn(
    e = d$e, t = round(d$t,2), r = round(d$r,2), 
    design = list(N = N, c = d$c),
    riskGroup = list(k = d$k), 
    rSummary = "median", bootstrap = FALSE)
  B2Fn(baseArgs)
}
