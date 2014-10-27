

performanceDifferenceInternalFn = function(baseArgs1 = FALSE, baseArgs2 = FALSE) {
  
  extraArgs1 = list(gammaHat = gammaHatFn(baseArgs1),
                    piHat = piHatFn(baseArgs1))

  extraArgs2 = list(gammaHat = gammaHatFn(baseArgs2),
                    piHat = piHatFn(baseArgs2))

  AUC_diff = AUC_Fn(baseArgs2, extraArgs2) - AUC_Fn(baseArgs1, extraArgs1)
  SD_diff = SD_Fn(baseArgs2, extraArgs2) - SD_Fn(baseArgs1, extraArgs1)

  if(baseArgs1$K == 2 && baseArgs2$K == 2) {
      PPP_diff = as.numeric(extraArgs2$piHat[2] - extraArgs1$piHat[2])
      PPN_diff = as.numeric((1 - extraArgs2$piHat[1]) - (1 - extraArgs1$piHat[1]))
    }

  bootReps = t(sapply(seq_len(baseArgs1$nBootstraps), function(i) {
    
    baseArgsBoot1 = baseArgsBootFn(baseArgs1)
    baseArgs2$indices = baseArgsBoot1$indices
    baseArgs2$indicesTwoStage = baseArgsBoot1$indicesTwoStage
    baseArgsBoot2 = baseArgsBootFn(baseArgs2)

    extraArgsBoot1 = list(gammaHat = gammaHatFn(baseArgsBoot1),
                          piHat = piHatFn(baseArgsBoot1))

    extraArgsBoot2 = list(gammaHat = gammaHatFn(baseArgsBoot2),
                          piHat = piHatFn(baseArgsBoot2))

    AUC_diff_boot = AUC_Fn(baseArgsBoot2, extraArgsBoot2) - AUC_Fn(baseArgsBoot1, extraArgsBoot1)
    SD_diff_boot = SD_Fn(baseArgsBoot2, extraArgsBoot2) - SD_Fn(baseArgsBoot1, extraArgsBoot1)

    if(baseArgs1$K == 2 && baseArgs2$K == 2) {
      PPP_diff_boot = as.numeric(extraArgsBoot2$piHat[2] - extraArgsBoot1$piHat[2])
      PPN_diff_boot = as.numeric((1 - extraArgsBoot2$piHat[1]) - (1 - extraArgsBoot1$piHat[1]))
      c(AUC = AUC_diff_boot, SD = SD_diff_boot,
        PPP = PPP_diff_boot, PPN = PPN_diff_boot)
    } else {
      c(AUC = AUC_diff_boot, SD = SD_diff_boot)
    }
  }))
  
  sd_AUC_diff_boot = sd(bootReps[ , "AUC"])
  sd_SD_diff_boot = sd(bootReps[ , "SD"])

  ci_AUC_diff = c(lower = AUC_diff - 1.96 * sd_AUC_diff_boot,
                  upper = AUC_diff + 1.96 * sd_AUC_diff_boot)

  ci_SD_diff = c(lower = SD_diff - 1.96 * sd_SD_diff_boot,
                  upper = SD_diff + 1.96 * sd_SD_diff_boot)


  if(baseArgs1$K == 2 && baseArgs2$K == 2) {

    sd_PPP_diff_boot = sd(bootReps[ , "PPP"])
    sd_PPN_diff_boot = sd(bootReps[ , "PPN"])

    ci_PPP_diff = c(lower = PPP_diff - 1.96 * sd_PPP_diff_boot,
                    upper = PPP_diff + 1.96 * sd_PPP_diff_boot)
    
    ci_PPN_diff = c(lower = PPN_diff - 1.96 * sd_PPN_diff_boot,
                    upper = PPN_diff + 1.96 * sd_PPN_diff_boot)

    list(AUC_diff = AUC_diff,
         ci_AUC_diff = ci_AUC_diff,
         SD_diff = SD_diff,
         ci_SD_diff = ci_SD_diff,
         PPP_diff = PPP_diff,
         ci_PPP_diff = ci_PPP_diff,
         PPN_diff = PPN_diff,
         ci_PPN_diff = ci_PPN_diff)
  } else {
    list(AUC_diff = AUC_diff,
         ci_AUC_diff = ci_AUC_diff,
         SD_diff = SD_diff,
         ci_SD_diff = ci_SD_diff)
  }
}

# Wed Mar 23 16:57:26 PDT 2011
