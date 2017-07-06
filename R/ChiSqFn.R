#' \code{ChiSqFn}
#' 
#' Compute the Hosmer-Lemseshow Chi-squared goodness-of-fit statistic
#' and corresponding p-value using \code{davies}
#' 
#' @param baseArgs A list provided by \code{baseArgsFn}.
#' This function calls either calls \code{gammaHatFn},
#' \code{piHatFn} and \code{SigmaFn} or uses values
#' calculated by them
#' so all objects requried by either of these functions are
#' also required by \code{ChiSqFn}.  Also required here are
#' \code{K}, \code{N_two_stage}, and \code{rSummary}
#' 
#' @param extraArgs A list of named elements which can contain
#' \code{gammaHat}, \code{lambdaHat}, \code{piHat}, and \code{Sigma}.
#' This argument gives access to intermediate values 
#' previously calculated.
#' 
#' @return A named vector holding the goodness of fit statistic
#' and the corresponding p-value.  
#' 
#' @examples
#' set.seed(1)
#' d = df_randomSample()
#' baseArgs = baseArgsFn(
#'   e = d$e, t = d$t, r = d$r, tStar = 10,
#'   design = "randomSample", riskGroup = list(K = 3),
#'   rSummary = "median", bootstrap = FALSE)
#' ChiSqFn(baseArgs)
#'   
#' @export

ChiSqFn = function(baseArgs = FALSE, extraArgs = FALSE) {
  gammaHat = if("gammaHat" %in% names(extraArgs)) extraArgs$gammaHat else gammaHatFn(baseArgs)
  piHat = if("piHat" %in% names(extraArgs)) extraArgs$piHat else piHatFn(baseArgs, extraArgs)
  Sigma = if("Sigma" %in% names(extraArgs)) extraArgs$Sigma else SigmaFn(baseArgs, extraArgs)

  yyy = piHat - baseArgs$rSummary
  SigmaPi = Sigma[baseArgs$K:(2 * baseArgs$K - 1), baseArgs$K:(2 * baseArgs$K - 1)]

  #<<< DJ adding for v0.01-01 Thu May 12 09:18:20 PDT 2011
  if(baseArgs$K == 1) SigmaPi = as.matrix(SigmaPi)
  #>>>

  ## Pearson = sum(baseArgs$N) * yyy %*% solve(SigmaPi) %*% yyy
  ## Pearson_pval = 1 - pchisq(Pearson, baseArgs$K)

  sigma = sqrt(sapply(1:baseArgs$K, function(kkk) SigmaPi[kkk, kkk]) / sum(baseArgs$N_two_stage))
  HosmerLemeshow = sum( gammaHat * (piHat - baseArgs$rSummary)^2 / sigma^2 )
  HL_pval =  davies(HosmerLemeshow, lambda = gammaHat)$Qq

  ## c(Pearson = Pearson, Pearson_pval = Pearson_pval,
  ##   HosmerLemeshow = HosmerLemeshow, HL_pval = HL_pval)
  c(HosmerLemeshow = HosmerLemeshow, HL_pval = HL_pval)
}

# Wed Mar 16 11:05:14 PDT 2011
