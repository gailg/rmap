#' \code{piHatSummaryFn}
#' 
#' @param baseArgs A list provided by \code{baseArgsFn}.
#' This function calls either calls
#' \code{piHatFb} and \code{SigmaFn} or uses values
#' calculated by them
#' so all objects requried by either of these functions are
#' also required by \code{piHatSummaryFn}.  Also required here are 
#' \code{K}, \code{N_two_stage}, and \code{rSummary}.
#' 
#' @param extraArgs A list of named elements which can contain
#' \code{gammaHat}, \code{lambdaHat}, \code{piHat}, and \code{Sigma}.
#' This argument gives access to intermediate values 
#' 
#' @return A data.frame containing a row for each risk group and six columns
#' \itemize{
#' \item{\code{r}: }{ The risk summary, determined by \code{rSummary} for each risk group.
#' }
#' \item{\code{piHat}: }{ The estimated \code{pi}.
#' }
#'  \item{\code{Sigma}: }{ The standard deviation of \code{piHat}.
#' }
#' \item{\code{lower}: }{ The lower bound of the \code{pi} confidence interval whose level is 
#'   determined by \code{confidenceLevel}.
#' }
#'  \item{\code{upper}: }{ The upper bound of the confidence interval.
#' }
#' \item{\code{inCI}: }{ A character equal to "yes" if the risk summary is within 
#'   the confidence interval.
#' }
#' }
#' @examples 
#' set.seed(1)
#' d = df_twoStage(KKK = 3)
#' ba = baseArgsFn(
#'   e = d$d$e, t = d$d$t, r = d$d$r, tStar = 10,
#'   design = list(c = d$d$c, N_two_stage = d$N), 
#'   riskGroup = list(k = d$d$k), rSummary = "median", bootstrap = FALSE)
#' piHatSummaryFn(ba)
#' 
#' @export

piHatSummaryFn = function(baseArgs = FALSE, extraArgs = FALSE) {
  piHat = if("piHat" %in% names(extraArgs)) extraArgs$piHat else piHatFn(baseArgs, extraArgs)
  Sigma = if("Sigma" %in% names(extraArgs)) extraArgs$Sigma else SigmaFn(baseArgs, extraArgs)

  sigma = sqrt(sapply(baseArgs$K:(2 * baseArgs$K - 1), function(kkk) Sigma[kkk, kkk]) / sum(baseArgs$N_two_stage))

  piHatCIs = prob_CI_Fn(piHat, sigma)
  
  piHatSummary = data.frame( r = baseArgs$rSummary, piHat = piHat, sigma = sigma,
    lower = piHatCIs[, "lower"],
    upper = piHatCIs[, "upper"],
    inCI = ifelse(baseArgs$rSummary <= piHatCIs[, "upper"] & baseArgs$rSummary >= piHatCIs[, "lower"], "yes", "no"))

  piHatSummary
}

# Wed Mar 16 11:04:47 PDT 2011
