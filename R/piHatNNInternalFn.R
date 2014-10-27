piHatNNInternalFn = function(baseArgs = FALSE, extraArgs = FALSE) {
  est = rho_piHatNN_Fn(baseArgs)
  iterFn = if(baseArgs$multicore) {
    require(parallel)
    mclapply
  } else {
    lapply
  }
  if(baseArgs$nBootstraps != 0) {
    randomSeeds = sample(1:1e8, baseArgs$nBootstraps, replace = FALSE)
    bootstrapsRaw = iterFn(seq_len(baseArgs$nBootstraps), function(i) {
      set.seed(randomSeeds[i])
      if(baseArgs$verbose) 
        print(paste("PID: ", Sys.getpid(), " ", date(),
          " piHatNNInternalFn: starting bootstrap ", i, sep = "")) 
      while(TRUE) {
        baseArgsBoot = baseArgsBootFn(baseArgs)
        x = tryCatch(rho_piHatNN_Fn(baseArgsBoot),
          error = function(x) "error!")
        if( !(is.character(x) && x == "error!") ) {
          break
        } else {
          if(baseArgs$verbose)
            print(paste("PID: ", Sys.getpid(), " ", date(),
              " piHatNNInternalFn: BAD SAMPLE in bootstrap ",
              i, ". RESAMPLING.", sep = ""))
        }
      }
      x      
    })
    bootstraps = gatherFn(bootstrapsRaw, all_rho = est[, "rho"])
    interpolatedBs = t(apply(bootstraps, 1, interpolateOneBsFn, est[, "rho"]))
    confBand = t(apply(interpolatedBs, 2, quantile, probs = c(0.025, 0.975)))
    return(list(est = est, confBand = confBand))
  } else {
    return(list(est = est))
  }
}
## Tue Aug 30 15:40:53 PDT 2011
## est has been changed from a data.frame to a matrix
## and so we needed to change est$rho to est[, "rho"].

## Thu Sep 22 20:26:29 PDT 2011
## Gail "cleaned up" the spacing so she could more easily
## follow this as an example for sdBootRiskValidateInternalFn

## Mon Oct 27 16:19:50 PDT 2014
## Gail changed require(multicore) to require(parallel).
