\name{gatherFn}
\alias{gatherFn}
\title{
Organize the bootstrapped piHatNNs into a matrix.
}
\description{
This function is called by piHatNNInternalFn to
  help organize the bootstrapped piHatNNs into a matrix that will
  faciliate interpolating the values of bootstrapped piHatNNs of the
  rho_k not sampled by a boostrap.
}
\usage{
gatherFn(bootstrapsRaw, all_rho)
}

\arguments{
  \item{bootstrapsRaw}{
    A list with number of elements equal to the number
    of bootstrap replications specified by the bootstrap argument in
    riskValidateUngrouped. Each element in the list corresponds to one
    bootstrap replication and is a result from rho_piHatNN_Fn and
    therefore a data.frame with the columns rho and piHatNN; rho
    contains the distinct values of assigned risks among the subjects
    that show up in this bootstrap replication, and piHatNN contains 
    nearest neighbor estimate of outcome probability at each rho.
}
  \item{all_rho}{
    A numeric vector containing the sorted and distinct
    values of assigned risks.
}
}
\details{

}
\value{
    A matrix with one row for each bootstrap and one column for each 
    value of rho equal to a distinct value of assigned risk.  If the 
    bootstrap that corresponds to a particular row sampled a particular
    rho, then the entry in the matrix is equal to the bootstrap nearest
    neighbor estimate for this rho; otherwise the entry in the matrix is
    NA.
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
      bootstrap = 20, multicore = FALSE, verbose = FALSE)
      
    est = rho_piHatNN_Fn(baseArgs)
      
    bootstrapsRaw = lapply(seq_len(baseArgs$nBootstraps), function(i) {
        while(TRUE) {
          baseArgsBoot = baseArgsBootFn(baseArgs)
          x = tryCatch(rho_piHatNN_Fn(baseArgsBoot), error = function(x) "error!")
          if(! (is.character(x) && x == "error!")) {
            break
          } 
        }
        x      
      })
    
    bootstraps = gatherFn(bootstrapsRaw, all_rho = est[, "rho"])
    dim(bootstraps)
    bootstraps[1:20, 1:6]
    str(bootstraps)
}
