\name{piHatNNInternalFn}
\alias{piHatNNInternalFn}
\title{
The epsilon kernel nearest neighbor estimates of outcome probabilities.
}
\description{
Calculates the values required for 
an individualized attribute diagram and 95 percent (nonsimultaneous)
confidence bands. 

The individualized attribute diagram is defined to
be a scatterplot of points x = rho and y = piHatNN  with line segments
connecting addjacent pints.  rho are the distinct values of assigned
risks and piHatNN are the epsilon kernel nearest neighbor estimates of
outcome probabilities. 

Confidence bands are obtained by bootstrapping.  
For each bootstrap replication, obtain a bootstrap
sample of the data and calculate an individualized attribute diagram
based on this bootstrap sample.  Imagine drawing all the bootstrap 
diagrams on one plot and at each rho, calculating the 2.5 and 97.5
percentiles of where the bootstrap diagrams intersect rho.  Connecting
the 2.5 percentiles with line segments forms the lower bound of the
confidence bands while connecting the 97.5 percentiles forms the
upper bound.
}
\usage{
piHatNNInternalFn(baseArgs = FALSE, extraArgs = FALSE)
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
gatherFn organizes the bootstrap replications into a (giant) matrix
with the number of rows equal to the number of bootstraps and the
number of columns equal to the number of distinct assigned risks rho.  
For each rho_k and each bootstrap, if the bootstrap replication
contains an observation with assigned risk rho_k, the corresponding
entry in the matrix is the piHatNN for that rho_k and for that bootstrap
replication. 

The giant matrix will contain many missing values because not all rho
show up in all bootstraps.  The missing values are interpolated using
the function interpolateOneBsFn.
}
\value{
A list containing
\itemize{
\item est: a matrix with the columns rho and piHatNN; rho contains
    the sorted and distinct values of assigned risks, and piHatNN contains 
    nearest neighbor estimate of outcome probability at each rho.
\item confBand: a matrix with two columns containing the lower and
    upper bounds of the confidence interval at each distinct value of
    assigned risk in est$rho.
}
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
    bootstrap = FALSE, multicore = FALSE, verbose = TRUE)
  pnn = piHatNNInternalFn(baseArgs = baseArgs)
  
  head(pnn$est)
  head(pnn$confBand)
  
  baseArgsB = baseArgsFn(e = sampleData$e, t = sampleData$t, r = sampleData$r1,
    design = "randomSample", riskGroup = riskGroup, rSummary = "mean",
    bootstrap = 20, multicore = FALSE, verbose = TRUE)
  pnnB = piHatNNInternalFn(baseArgs = baseArgsB)
  
  head(pnnB$est)
  str(pnnB$est)
  head(pnnB$confBand)
  str(pnnB$confBand)
}
