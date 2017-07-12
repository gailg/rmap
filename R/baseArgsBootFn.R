#' Like \code{baseArgsFn} but do it to a bootstrapped sample
#' 
#' @param baseArgs A list provided by \code{baseArgsFn}.
#' This function calls either calls
#' \code{gammaHatFn} and \code{lambdaHatFn} or uses values
#' calculated by them
#' so all objects requried by either of these functions are
#' also required by \code{baseArgsBootFn}.  
#' Also required here is \code{N_two_stage}
#' \code{n_two_stage},
#' \code{e}, \code{t}, \code{r}, \code{c}, \code{k},
#' \code{K}, \code{epsilon}, \code{tStar},
#' \code{ungrouped}, \code{multicore}, \code{verbose}
#' (in other words, all parameters required in the calculation
#' of the estimates of \code{pi} and \code{calibration}).
#' 
#' @param extraArgs A list of named elements which can contain
#' \code{gammaHat}, \code{lambdaHat}, \code{piHat}, and \code{Sigma}.
#' This argument gives access to intermediate values 
#' previously calculated.
#' 
#' @return A list containing 
#' \itemize{
#' \item{Bootstrapped versions of: }{
#'   \code{e}, \code{t}, \code{r}, \code{c}, and \code{k}.
#'   \code{N_two_stage}, \code{n_two_stage};
#' }
#' \item{Values inherited from \code{baseArgs}: }{
#'   \code{K}, \code{epsilon}, \code{tStar},
#'   \code{ungrouped}, \code{multicore}, \code{verbose}
#' }
#' \item{Objects new to this function: }{
#'   \code{weight} equal to NBoo / nBoo and used instead
#'   of aaa in concordance calcuations;
#'   \code{indices} and \code{indicesTwoStage} which 
#'   are the indices from the bootstrapping and
#'   included here for debugging and not used in other
#'   \code{rmap} functions
#'}
#'}
#'
#' @examples 
#' set.seed(1)
#' sampleData1 = df_randomSample()
#' baseArgs1 = baseArgsFn(
#'   e = sampleData1$e, t = sampleData1$t, r = sampleData1$r, 
#'   tStar = 10, design = "randomSample", riskGroup = list(K = 3),
#'   rSummary = "median", bootstrap = FALSE)
#' str(baseArgs1)
#' str(baseArgsBootFn(baseArgs1))
#' sampleData2 = df_randomSample_r1_r2(NTotal = 200)
#' epsilon = nrow(sampleData2)^(-1/3)
#' baseArgs2 = baseArgsFn(
#'   e = sampleData2$e, t = sampleData2$t, r = sampleData2$r1, 
#'   tStar = 10, design = "randomSample", riskGroup = list(epsilon = epsilon),
#'   rSummary = "mean", bootstrap = 20, multicore = FALSE, verbose = TRUE)
#' str(baseArgs2)
#' str(baseArgsBootFn(baseArgs2))
#'
#' @export


baseArgsBootFn = function(baseArgs) {

  if(all(c("indices", "indicesTwoStage") %in% names(baseArgs))) {
    indices = baseArgs$indices
    indicesTwoStage = baseArgs$indicesTwoStage  
  } else {
    indices = suppressWarnings({
      sample(1:sum(baseArgs$n_two_stage), sum(baseArgs$N_two_stage), replace = TRUE,
             prob = (baseArgs$N_two_stage / baseArgs$n_two_stage )[baseArgs$c])
    })

                                        #oneStg = (df[indices, ])
    keep = rbinom(sum(baseArgs$N_two_stage), 1, (baseArgs$n_two_stage / baseArgs$N_two_stage )[baseArgs$c[indices]])
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
  cUni = names(baseArgs$N_two_stage)
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
       N_two_stage = NBoo,
       n_two_stage = nBoo,
       multicore = baseArgs$multicore,
       verbose = baseArgs$verbose,
       indices = indices,
       indicesTwoStage = indicesTwoStage)

  
  # Checks that there is at least one significant event (death from disease) in every k group:
  # New Tue Apr 26 15:15:14 PDT 2011 >>>
  if(FALSE){  # "2017-06-20 12:40:57 PDT" GG I have other error messages in place in rmap_two_stage_fn
  if (!all(tapply(baseArgsBoot$e, baseArgsBoot$k, function(e_k) any(e_k == 1)  ) ) ) {
    
    stop(paste("In at least one bootstrap sample, one of the risk groups had no",
               "disease events (e = 1). This probably happened because one of the",
               "risk groups in the original data had very small numbers of ",
               "disease events.  You may want to redefine your risk groups so that",
               "the numbers of disease events is moderate for each group. You",
               "may also decide not to use the bootstrap to get confidence intervals.",
               sep = "\n"))
  }
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

