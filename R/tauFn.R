

## tauFn = function(baseArgs = FALSE, extraArgs = FALSE) {
##    if(baseArgs$K == 1) {
##     "tau does not apply for K = 1"
##   } else {
##     piHat = if("piHat" %in% names(extraArgs)) extraArgs$piHat else piHatFn(baseArgs, extraArgs)
##     gammaHat = if("gammaHat" %in% names(extraArgs)) extraArgs$gammaHat else gammaHatFn(baseArgs)
    
##     piHatOverall = sum(piHat * gammaHat)
  
##     tauSquared = sum(gammaHat * (piHat - piHatOverall) ^ 2)
##     sqrt(tauSquared)
##   }
## }

## # Wed Mar 16 11:06:49 PDT 2011
