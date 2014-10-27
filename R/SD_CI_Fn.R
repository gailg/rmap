
SD_CI_Fn = function(baseArgs = FALSE, extraArgs = FALSE) { 

  if(baseArgs$K == 1) {
    "SD does not apply for K = 1"
  } else {
    piHat = if("piHat" %in% names(extraArgs)) extraArgs$piHat else piHatFn(baseArgs, extraArgs)
    gammaHat = if("gammaHat" %in% names(extraArgs)) extraArgs$gammaHat else gammaHatFn(baseArgs)
    Sigma = if("Sigma" %in% names(extraArgs)) extraArgs$Sigma else SigmaFn(baseArgs, extraArgs)
    
    piHatOverall = sum(piHat * gammaHat)
    
    SD_Squared = sum(gammaHat * (piHat - piHatOverall) ^ 2)
    qqq = qlogis(SD_Squared)
    Dgamma = (piHat[-baseArgs$K] - piHatOverall) ^ 2 - (piHat[baseArgs$K] - piHatOverall) ^ 2
    Dpi = 2 * gammaHat * (piHat - piHatOverall)
    D_SD_Squared = c(Dgamma, Dpi)
    V_SD_Squared = as.numeric(t(D_SD_Squared) %*% Sigma %*% D_SD_Squared / sum(baseArgs$N))
    V_SD = V_SD_Squared / (4 * SD_Squared)

    prob_CI_Fn(sqrt(SD_Squared), sqrt(V_SD))[1,] # FLAG: Check this CI
  }
}

# Wed Mar 16 11:07:09 PDT 2011
# Changed tau_CI_Fn to SD_CI_Fn Wed Apr 27 09:43:00 PDT 2011
