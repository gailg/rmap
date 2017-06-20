

baseArgsBootFn = function(baseArgs) {

  if(all(c("indices", "indicesTwoStage") %in% names(baseArgs))) {
    indices = baseArgs$indices
    indicesTwoStage = baseArgs$indicesTwoStage  
  } else {
    indices = suppressWarnings({
      sample(1:sum(baseArgs$n), sum(baseArgs$N), replace = TRUE,
             prob = (baseArgs$N / baseArgs$n )[baseArgs$c])
    })

                                        #oneStg = (df[indices, ])
    keep = rbinom(sum(baseArgs$N), 1, (baseArgs$n / baseArgs$N )[baseArgs$c[indices]])
    indicesTwoStage = indices[as.logical(keep)]
  }

     
  tempDf = data.frame(
    eBoo = baseArgs$e[indicesTwoStage],
    tBoo = baseArgs$t[indicesTwoStage],
    rBoo = baseArgs$r[indicesTwoStage],
    cBoo = baseArgs$c[indicesTwoStage],
    kBoo = baseArgs$k[indicesTwoStage], stringsAsFactors = FALSE ) #DJ -> added stringsAsFactors=FALSE
  tempDf = tempDf[order(tempDf$kBoo),]                             #      based on Gail's version in 
                                                                   #      00-existing-functions-gail
  cUni = names(baseArgs$N)
  nBoo = structure(sapply(cUni, function(c1){
    sum(tempDf$cBoo == c1)
  }), .Names = cUni)
  NBoo = structure(sapply(cUni, function(c1){
    sum(baseArgs$c[indices] == c1)
  }), .Names = cUni)
  aaa = NBoo / nBoo
  weight = unname(aaa[tempDf$cBoo])

  baseArgsBoot = list(e = tempDf$eBoo,
       t = tempDf$tBoo,
       r = tempDf$rBoo,
       c = tempDf$cBoo,
       k = tempDf$kBoo,
       K = baseArgs$K,
       epsilon = baseArgs$epsilon,
       weight = weight,
       tStar = baseArgs$tStar,
       ungrouped = baseArgs$ungrouped,
       N = NBoo,
       n = nBoo,
       multicore = baseArgs$multicore,
       verbose = baseArgs$verbose,
       indices = indices,
       indicesTwoStage = indicesTwoStage)

  
  # Checks that there is at least one significant event (death from disease) in every k group:
  # New Tue Apr 26 15:15:14 PDT 2011 >>>
  if (!all(tapply(baseArgsBoot$e, baseArgsBoot$k, function(e_k) any(e_k == 1)  ) ) ) {
    
    stop(paste("In at least one bootstrap sample, one of the risk groups had no",
               "disease events (e = 1). This probably happened because one of the",
               "risk groups in the original data had very small numbers of ",
               "disease events.  You may want to redefine your risk groups so that",
               "the numbers of disease events is moderate for each group. You",
               "may also decide not to use the bootstrap to get confidence intervals.",
               sep = "\n"))
  }
  # <<<

  class(baseArgsBoot) = c("baseArgsBoot", class(baseArgsBoot))
  baseArgsBoot
}          

# Wed Mar 16 11:07:42 PDT 2011

# Tue May  3 15:09:35 PDT 2011 -- replaced the error message if a bootstrap sample
#                                 lacks a disease event in a risk group.



# In the future:
#It might be nice to to allow throwing away a bootstrap or two.

