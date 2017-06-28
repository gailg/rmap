\name{ecdf2Stg}
\alias{ecdf2Stg}
\title{
The empirical cumulative distribution function allowing for two-stage sampling
}
\description{
Compute or plot the empirical cumulative distribution function
  allowing for two-stage sampling.  This function extends ecdf.
}
\usage{
ecdf2Stg(x, aaa)
}

\arguments{
  \item{x}{
A numeric vector of observations.
}
  \item{aaa}{
Weights corresponding to the observations in x
}
}
\details{
This function was tailored after ecdf.  See help(ecdf) to better understand its 
behavior.
}
\value{
A function of class "ecdf".

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
  NTotal = 200
  twoStage = df_twoStage_r1_r2(NTotal = NTotal)
  
  tStar = 10
  epsilon = NTotal^(-1/3)
  epsilon
  riskGroup = list(ungrouped = list(tStar = tStar, epsilon = epsilon))
    
  baseArgs = baseArgsFn(e = twoStage$d$e, t = twoStage$d$t, r = twoStage$d$r1,
    design = list(N = twoStage$N, c = twoStage$d$c), riskGroup = riskGroup, rSummary = "mean",
    bootstrap = 20, multicore = FALSE, verbose = TRUE)
  
  aaa = (baseArgs$N / baseArgs$n)[baseArgs$c]
  GFn = ecdf2Stg(baseArgs$r, aaa)
  plot(GFn)
}
