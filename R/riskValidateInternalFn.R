

riskValidateInternalFn = function(baseArgs = FALSE, extraArgs = FALSE) {

  gammaHat = gammaHatFn(baseArgs)

  lambdaHat = lambdaHatFn(baseArgs)

  extraArgs = list(gammaHat = gammaHat,
                   lambdaHat = lambdaHat)
  
  piHat = piHatFn(baseArgs, extraArgs)
  extraArgs$piHat = piHat

  Sigma = SigmaFn(baseArgs, extraArgs)
  extraArgs$Sigma = Sigma

  piHatSummary = piHatSummaryFn(baseArgs, extraArgs)

  ChiSq = ChiSqFn(baseArgs, extraArgs)

  AUC = AUC_Fn(baseArgs, extraArgs)

  AUC_CI = AUC_CI_Fn(baseArgs, extraArgs)

  SD = SD_Fn(baseArgs, extraArgs)

  SD_CI = SD_CI_Fn(baseArgs, extraArgs)



  if(baseArgs$nBootstraps == 0){
    rv = list(gammaHat = gammaHat,
      piHat = piHat,
      Sigma = Sigma,
      piHatSummary = piHatSummary,
      ChiSq = ChiSq,
      AUC = AUC,
      AUC_CI = AUC_CI,
      SD = SD,
      SD_CI = SD_CI)
  } else {
    sdBoot = sdBootRiskValidateInternalFn(baseArgs)

    #>>> DJ edit for v0.01-01 Thu May 12 09:58:35 PDT 2011
    if(baseArgs$K == 1) {
      CIBoot = prob_CI_Fn(piHat, sdBoot)
    } else {
      CIBoot = prob_CI_Fn(c(piHat, AUC, SD), sdBoot)
    }
    #>>>
    
    rownames(CIBoot) = names(sdBoot)
    piHatSummary = cbind(piHatSummary, sigmaBoot = sdBoot[1:baseArgs$K],
      lowerBoot = CIBoot[1:baseArgs$K, 1], upperBoot = CIBoot[1:baseArgs$K, 2],
      inBootCI = ifelse(
        CIBoot[1:baseArgs$K, 1] <= baseArgs$rSummary &
        baseArgs$rSummary <= CIBoot[1:baseArgs$K, 2], "yes", "no")
      )
    rv = list(gammaHat = gammaHat,
      piHat = piHat,
      Sigma = Sigma,
      piHatSummary = piHatSummary,
      ChiSq = ChiSq,
      AUC = AUC,
      AUC_CI = AUC_CI,
      
      #>>> DJ edit for v0.01-01 Thu May 12 09:58:35 PDT 2011
      AUC_CI_boot = if(baseArgs$K == 1) {
                      "Bootstrap AUC CI does not apply for K = 1"
                    } else {
                      CIBoot["AUC",]
                    },
      #>>>
      
      SD = SD,
      SD_CI = SD_CI,

      #>>> DJ edit for v0.01-01 Thu May 12 09:58:35 PDT 2011
      SD_CI_boot = if(baseArgs$K == 1) {
                     "Bootstrap SD CI does not apply for K = 1"
                   } else {
                     CIBoot["SD",]
                   }
      #>>>
      
      )
  }
  class(rv) = c("rv", class(rv))
  rv
}

# Wed Mar 23 16:56:53 PDT 2011
