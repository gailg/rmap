\name{gammaHatFn}
\alias{gammaHatFn}

\title{
Computes gammaHat, the proportion of subjects in each risk group.
}
\description{
This function computes gammaHat, the proportion of subjects in each
risk group.  This function is usually called internally by 
\code{riskValidate} or \code{performanceDifference}.  
It takes two-stage sampling weights into account if a two-stage 
sampling design was used.  
}
\usage{
gammaHatFn(baseArgs)
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

}
\details{

}
\value{
Returns a numeric vector with one element for each risk group.  The k-th element of this
vector is the proportion of subjects in risk group k.  
See equation (45) in "rmap-formulas-v01.pdf" from the website.
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
ba = baseArgsFn(e = d$d$e, t = d$d$t, r = d$d$r, design = list(c = d$d$c, N = d$N), riskGroup = list(K = 2),
  rSummary = "median", bootstrap = FALSE)
gh1 = gammaHatFn(ba)
gh1

set.seed(1)
d = df_randomSample()
ba = baseArgsFn(e = d$e, t = d$t, r = d$r, design = "randomSample", riskGroup = list(K = 3),
  rSummary = "median", bootstrap = FALSE)
gh2 = gammaHatFn(ba)
gh2


}
