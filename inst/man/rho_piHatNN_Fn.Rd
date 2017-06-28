\name{rho_piHatNN_Fn}
\alias{rho_piHatNN_Fn}
\title{
Distinct values of assigned risks and the epsilon kernel nearest neighbor estimate of outcome probabilities.
}
\description{
Calculate for each distinct value rho of assigned risks,
the epsilon kernel nearest neighbor estimate piHatNN of outcome
probabilities. 

The document rmap-formulas-v02.pdf, available from 
http://www.stanford.edu/~ggong/rmap/index.html,
defines an epsilon kernel nearest neigbhorhood of rho.
Considering all observations in this neighborhood to be in one risk
group, piHatNN is the "grouped piHat estimate" of this group.
}
\usage{
rho_piHatNN_Fn(baseArgs)
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
    A matrix with the columns rho and piHatNN; rho contains
    the distinct values of assigned risks, and piHatNN contains 
    nearest neighbor estimate of outcome probability at each rho.
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
  sampleData = df_randomSample_r1_r2(NTotal = 200)
  
  tStar = 10
  epsilon = nrow(sampleData)^(-1/3)
  epsilon
  riskGroup = list(ungrouped = list(tStar = tStar, epsilon = epsilon))
    
  baseArgs = baseArgsFn(e = sampleData$e, t = sampleData$t, r = sampleData$r1,
    design = "randomSample", riskGroup = riskGroup, rSummary = "mean",
    bootstrap = 20, multicore = FALSE, verbose = TRUE)

  est = rho_piHatNN_Fn(baseArgs)
  head(est)
  str(est)
}
