\name{vvTsumFnC}
\alias{vvTsumFnC}

\title{
Computes the sum of v %*% t(v) %*% a using C-code
}
\description{
This function computes v %*% t(v) %*% a, which is an internal 
calculation needed to eventually compute the value B2, which
is used in the two-stage sampling estimate of the covariance matrix, Sigma. 

It calls a C routine, which speeds up this computationally expensive
calculations.
}
\usage{
vvTsumFnC(uuu, mult = rep(1, nrow(uuu)))
}

\arguments{
  \item{uuu}{
A matrix, where each row represents a subject and each column 
represents a partial derivative. See equation (52) in "rmap-formulas-v01.pdf" from the website.
}
  \item{mult}{
A multiplier. Currently not used.
}
}
\details{
This function computes the summation in equation (82) in "rmap-formulas-v01.pdf" from the website.
}
\value{
A square matrix, with dimensions: (K - 1 + 2 sum_k (M_k) ), where K is 
the total number of risk groups, k is indexes the risk groups, and M_k is the 
number of unique times to death or disease for the k-th risk group.
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
\code{vvTsumFnC}

}


\examples{
set.seed(1)
# A smaller-than-normal dataset:
d = df_twoStage(NTotal = 20)
baseArgs = baseArgsFn(
  e = d$d$e, t = d$d$t, r = d$d$r, design = list(c = d$d$c, N = d$N), 
  riskGroup = list(K = 2),
  rSummary = "median", bootstrap = FALSE)

# The innards of B2Fn:
uuu = uuuFn(baseArgs)
cUni = names(baseArgs$N)
ccc = cUni[1]
uuu_c = uuu[baseArgs$c[order(baseArgs$k)] == ccc, ]
uuT_C = vvTsumFnC(uuu_c)
uuT_C
}