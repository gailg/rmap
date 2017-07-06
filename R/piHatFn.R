#' \code{piHat}, the estimated probability of getting disease 
#' in the duration of the study for each risk group.
#' 
#' This function computes \code{piHat}, 
#' which is the estimated probability of getting disease in 
#' \code{(0, t*]}, the duration of the study.  
#' This function is usually called internally by grouped \code{rmap}.
#' See equation (90) in "rmap-formulas-v02.pdf" from the website.
#' 
#' @param baseArgs A list provided by \code{baseArgsFn}.
#' This function calls either calls
#' \code{gammaHatFn} and \code{lambdaHatFn} or uses values
#' calculated by them
#' so all objects requried by either of these functions are
#' also required by \code{uuuFn}.  Also required here is \code{K}
#' 
#' @param extraArgs A list of named elements which can contain
#' \code{gammaHat}, \code{lambdaHat}, \code{piHat}, and \code{Sigma}.
#' This argument gives access to intermediate values 
#' previously calculated.
#' 
#' @return A vector, with one value of \code{piHat} 
#' for each risk group.  This vector has names 
#' \code{c("k1", "k2", ...)}.
#' 
#' @examples 
#' set.seed(1)
#' d = df_twoStage()
#' baseArgs = baseArgsFn(
#'   e = d$d$e, t = d$d$t, r = d$d$r, tStar = 10,
#'   design = list(c = d$d$c, N_two_stage = d$N), riskGroup = list(K = 2),
#'   rSummary = "median", bootstrap = FALSE)
#' ph1 = piHatFn(baseArgs)
#' ph1
#' set.seed(1)
#' d = df_randomSample()
#' baseArgs = baseArgsFn(
#'   e = d$e, t = d$t, r = d$r, tStar = 10,
#'   design = "randomSample", riskGroup = list(K = 3),
#'   rSummary = "median", bootstrap = FALSE)
#' ph2 = piHatFn(baseArgs)
#' ph2
#' 
#' @export

piHatFn = function(baseArgs = FALSE, extraArgs = FALSE) {
  lambdaHat = if("lambdaHat" %in% names(extraArgs)) extraArgs$lambdaHat else lambdaHatFn(baseArgs)
  
  structure(sapply(1:baseArgs$K, function(kkk) {
    lambdaDot = colSums( lambdaHat[[kkk]]$lambdaHat )
    sum(lambdaHat[[kkk]]$lambdaHat[1, ] * c(1, cumprod(1 - lambdaDot)[-length(lambdaDot)]))
  }), .Names = paste("k", 1:baseArgs$K, sep = ""))
}

# Wed Mar 16 10:59:17 PDT 2011
