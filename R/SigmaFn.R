#' \code{SigmaFn} the covariance matrix of \code{(gammaHat[1], ..., gammaHat[K-1], piHat)}
#' 
#' Sigma is one of the important products of the rmap package.  
#' It is gotten by calculating (1) the asymptotic covariance matrix of the 
#' \code{c(gammaHat[1], ..., gammaHat[K-1], lambda)} under random sampling, 
#' (2) adjusting for two-stage sampling if appropriate, 
#' and (3) transforming the covariance matrix of 
#' \code{(gammaHat[1], ..., gammaHat[K-1], piHat)} using the delta method.
#' From \code{Sigma}, we can obtain confidence intervals for \code{pi}.  
#' \code{Sigma} is also an intermediate step for obtaining 
#' the Hosmer-Lemeshow statistic.
#' 
#' @param baseArgs A list provided by \code{baseArgsFn}.
#' This function calls either calls
#' \code{gammaHatFn} and \code{lambdaHatFn} or uses values
#' calculated by them
#' so all objects requried by either of these functions are
#' also required by \code{SigmaFn}.  Also required here is
#' \code{N_two_stage}.
#' 
#' @param extraArgs A list of named elements which can contain
#' \code{gammaHat}, \code{lambdaHat}, \code{piHat}, and \code{Sigma}.
#' This argument gives access to intermediate values 
#' previously calculated.
#' 
#' @return A symmetric square matrix with numbers of rows and 
#' columns equal to \code{2 * K - 1}. 
#' See Equation (94) in "rmap-formulas-v02.pdf" from the website.
#' 
#' @examples 
#' set.seed(1)
#' d = df_twoStage()
#' baseArgs = baseArgsFn(
#'   e = d$d$e, t = d$d$t, r = d$d$r, tStar = 10,
#'   design = list(c = d$d$c, N_two_stage = d$N), riskGroup = list(K = 2),
#'   rSummary = "median", bootstrap = FALSE)
#' ph1 = pi_hat_fn(baseArgs)
#' ph1
#' Sigma1 = SigmaFn(baseArgs)
#' Sigma1
#' set.seed(1)
#' d = df_randomSample()
#' baseArgs = baseArgsFn(
#'   e = d$e, t = d$t, r = d$r, tStar = 10,
#'   design = "randomSample", riskGroup = list(K = 3),
#'   rSummary = "median", bootstrap = FALSE)
#' ph2 = pi_hat_fn(baseArgs)
#' ph2
#' Sigma2 = SigmaFn(baseArgs)
#' Sigma2
#' 
#' @export

SigmaFn = function(baseArgs = FALSE, extraArgs = FALSE) {
  Der = if("Der" %in% names(extraArgs)) extraArgs$Der else DerFn(baseArgs, extraArgs)
  V2Stage = if("V2Stage" %in% names(extraArgs)) extraArgs$V2Stage else V2StageFn(baseArgs, extraArgs)
  t(Der) %*% V2Stage %*% Der
}

# Wed Mar 16 11:04:00 PDT 2011
