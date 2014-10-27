

B2Fn = function(baseArgs = FALSE, extraArgs = FALSE) {
  gammaHat = if("gammaHat" %in% names(extraArgs)) extraArgs$gammaHat else gammaHatFn(baseArgs)
  lambdaHat = if("lambdaHat" %in% names(extraArgs)) extraArgs$lambdaHat else lambdaHatFn(baseArgs)
  uuu = if("uuu" %in% names(extraArgs)) extraArgs$uuu else uuuFn(baseArgs)
  
  cUni = names(baseArgs$N)
  muHat = t(sapply(cUni, function(ccc) colMeans(uuu[baseArgs$c[order(baseArgs$k)] == ccc, ])))   ###DJDJ
                                        # Had to reorder based on the order of $k to get results consistent with rime 0.00.008

  # FLAG!  This is really slow.  We need to look at it later. 17/34
  PhiHat = structure(lapply(cUni, function(ccc) {
    uuu_c = uuu[baseArgs$c[order(baseArgs$k)] == ccc, ]    ###DJDJ
                                        # Had to reorder based on the order of $k to get results consistent with rime 0.00.008
    uuT = vvTsumFnC(uuu_c)
    uuT / nrow(uuu_c)
  }), .Names = cUni) 

  omegaHat = baseArgs$N / sum(baseArgs$N)

  pHat = baseArgs$n / baseArgs$N

  # FLAG! This is also really slow.  10/34 total seconds
  PhiHatPart = apply(array(unlist(mapply(PhiHatFn, omegaHat, pHat, baseArgs$n, PhiHat, SIMPLIFY = FALSE)),
      c(ncol(uuu), ncol(uuu), length(cUni))), c(1,2), sum)

  multiplier = omegaHat * ((1 - pHat)/pHat) * baseArgs$n / (baseArgs$n - 1)
  muHatPart = vvTsumFn(muHat, multiplier)
  
  PhiHatPart - muHatPart
}

# Wed Mar 16 11:02:54 PDT 2011
# Tue May  3 14:50:33 PDT 2011 -- now calls vvTsumFnC instead of vvTsumFn.
