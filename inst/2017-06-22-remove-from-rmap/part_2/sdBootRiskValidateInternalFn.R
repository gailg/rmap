sdBootRiskValidateInternalFn = function(baseArgs){
  
  bootReps = t(sapply(1:baseArgs$nBootstraps, function(booIndex){
    baseArgsBoot = baseArgsBootFn(baseArgs)
    gammaHat = gammaHatFn(baseArgsBoot)
    piHat = piHatFn(baseArgsBoot)
    extraArgs = list(gammaHat = gammaHat, piHat = piHat)
    if(baseArgs$K == 1){
      structure(piHat, .Names = c(paste("piHat", 1:baseArgs$K, sep = "")))
    } else {
      AUC = AUC_Fn(baseArgsBoot, extraArgs)
      SD = SD_Fn(baseArgsBoot, extraArgs)
      structure(c(piHat, AUC, SD),
        .Names = c(paste("piHat", 1:baseArgs$K, sep = ""), "AUC", "SD"))
    }
  }))
  
  if(baseArgs$K == 1)
    bootReps = structure(t(bootReps), dimnames = list(NULL, "piHat1"))
  if(baseArgs$K == 1){
    structure(apply(bootReps, 2, sd), .Names = "piHat1")
  } else {
    structure(apply(bootReps, 2, sd),
      .Names = c(paste("piHat", 1:baseArgs$K, sep = ""), "AUC", "SD"), sep = "")
  }
}

## Wed Mar 16 11:08:27 PDT 2011

## Thu Sep 22 20:31:47 PDT 2011
## Gail "cleaned up" spacing in preparation for adding multicore and verbose.


