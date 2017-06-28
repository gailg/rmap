\name{interpolateOneBsFn}
\alias{interpolateOneBsFn}
\title{
Interpolate the values of piHatNN at those rhos missing in a bootstrap.
}
\description{
This function is called by piHatNNInternalFn to
  help get bootstrap confidence bands.  This function is the
  numerical analogue of drawing line segments that join the
  scatterplot in the bootstrap indivdualized attribute diagrams.
}
\usage{
interpolateOneBsFn(oneBs, all_rho)
}

\arguments{
  \item{oneBs}{
A numerical vector of with the same length as all_rho. This
    vector corresponds to one bootstrap replication of piHatNN. For
    each rho in all_rho, if rho shows up in this bootstrap
    replication, oneBs equals piHatNN at this rho.  If rho does not
    show up in this bootstrap replication, oneBs equals NA.
}
  \item{all_rho}{
A numerical vector of length equal to the number of
    distinct values of assigned risk. This vector is equal to
    sort(unique(r)) where r is the argument called by
    riskValidateUngrouped.
}
}
\details{

}
\value{
A numerical vector of length equal to the length of
  all_rho. The same vector as oneBs, but with all the NAs replaced
  with interpolated values.
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
  oneBs = bootstraps[1,]
  all_rho = est[, "rho"]
  interpolated = interpolateOneBsFn(oneBs = oneBs, all_rho = all_rho)
  
  head(data.frame(oneBs, interpolated), 20)
}
