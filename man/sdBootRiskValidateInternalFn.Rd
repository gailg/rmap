\name{sdBootRiskValidateInternalFn}
\alias{sdBootRiskValidateInternalFn}

\title{
Compute estimate of bootstrap standard deviation for piHat, AUC, and model SD
}
\description{
This function computes the bootstrap standard deviation of piHat, AUC, and the 
model's standard deviation.  For each bootstrap replication, this function 
(1) draws a bootstrap sample of the subjects in the original dataset, and 
(2) computes piHat, AUC, and SD for the bootstrap sample.
This function then computes the standard deviation of many bootstrap samples.  
}
\usage{
sdBootRiskValidateInternalFn(baseArgs)
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
A named vector containing the standard deviation for: each piHat (one for each risk group), 
the AUC, and the SD of the model.  
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
# random sampled data:
# (takes some time)
set.seed(1)
d_rs = df_randomSample()
ba = baseArgsFn(
    e = d_rs$e, t = d_rs$t, r = d_rs$r, 
    design = "randomSample", riskGroup = list(K = 3), rSummary = "median", 
    bootstrap = 300)

sdBoot1 = sdBootRiskValidateInternalFn(ba)

# two-stage sampled data:
# (takes more time)
set.seed(1)
d_ts = df_twoStage()
ba = baseArgsFn(
    e = d_ts$d$e, t = d_ts$d$t, r = d_ts$d$r,
    design = list(c = d_ts$d$c, N = d_ts$N), riskGroup = list(k = d_ts$d$k),
    rSummary = "median", 
    bootstrap = 300)

sdBoot2 = sdBootRiskValidateInternalFn(ba)
}
