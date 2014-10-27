

## tau_CI_Fn = function(baseArgs = FALSE, extraArgs = FALSE) { 

##   if(baseArgs$K == 1) {
##     "tau does not apply for K = 1"
##   } else {
##     piHat = if("piHat" %in% names(extraArgs)) extraArgs$piHat else piHatFn(baseArgs, extraArgs)
##     gammaHat = if("gammaHat" %in% names(extraArgs)) extraArgs$gammaHat else gammaHatFn(baseArgs)
##     Sigma = if("Sigma" %in% names(extraArgs)) extraArgs$Sigma else SigmaFn(baseArgs, extraArgs)
    
##     piHatOverall = sum(piHat * gammaHat)
    
##     tauSquared = sum(gammaHat * (piHat - piHatOverall) ^ 2)
##     qqq = qlogis(tauSquared)
##     Dgamma = (piHat[-baseArgs$K] - piHatOverall) ^ 2 - (piHat[baseArgs$K] - piHatOverall) ^ 2
##     Dpi = 2 * gammaHat * (piHat - piHatOverall)
##     DtauSquared = c(Dgamma, Dpi)
##     VtauSquared = as.numeric(t(DtauSquared) %*% Sigma %*% DtauSquared / sum(baseArgs$N))
##     Vtau = VtauSquared / (4 * tauSquared)

##     prob_CI_Fn(sqrt(tauSquared), sqrt(Vtau))[1,] # FLAG: Check this CI
##   }
## }

## # Wed Mar 16 11:07:09 PDT 2011
