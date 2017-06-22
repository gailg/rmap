

SD_Fn = function(baseArgs = FALSE, extraArgs = FALSE) {
   if(baseArgs$K == 1) {
    "SD does not apply for K = 1"
  } else {
    piHat = if("piHat" %in% names(extraArgs)) extraArgs$piHat else piHatFn(baseArgs, extraArgs)
    gammaHat = if("gammaHat" %in% names(extraArgs)) extraArgs$gammaHat else gammaHatFn(baseArgs)
    
    piHatOverall = sum(piHat * gammaHat)
  
    SD_Squared = sum(gammaHat * (piHat - piHatOverall) ^ 2)
    sqrt(SD_Squared)
  }
}

# Wed Mar 16 11:06:49 PDT 2011
# Changed tauFn to SD_Fn Wed Apr 27 09:42:00 PDT 2011
