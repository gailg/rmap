#' \code{V2StageFn}
#' 
#' V2Stage is the covariance matrix for 
#' \code{c(gammaHat[1], ..., gamma[K-1], lambdaHat)} under two stage sampling.  
#' It is an intermediate step in the calculation of \code{Sigma} 
#' which is used to obtain confidence intervals for \code{pi} 
#' and to obtain the Hosmer-Lemeshow statistic. 
#' This function is usually called internally by grouped \code{rmap}
#' See Equations (80-84) in "rmap-formulas-v02.pdf" from the website.
#' 
#' @param baseArgs A list provided by \code{baseArgsFn}.
#' This function calls either calls
#' \code{gammaHatFn} and \code{lambdaHatFn} or uses values
#' calculated by them
#' so all objects requried by either of these functions are
#' also required by \code{uuuFn}.  Also required here is
#' \code{N_two_stage}.
#' 
#' @param extraArgs A list of named elements which can contain
#' \code{gammaHat}, \code{lambdaHat}, \code{piHat}, and \code{Sigma}.
#' This argument gives access to intermediate values 
#' previously calculated.
#' 
#' @return A symmetric square matrix with numbers of rows and columns equal to
#' \code{(K-1) + 2 * (M_1 + ... + M_K)}.  
#' This matrix is the covariance of the following 
#' random variables where the subscripts for \code{lambdaHat} are ordered so that 
#' \code{k} grows most slowly, \code{e} grows next most slowly, 
#' and \code{m} grows fastest: 
#' \code{\{gammaHat_k : k = 1, ..., K-1\}, \{lambdaHat_\{k,e,m\} : k = 1, ... K; e = 1,2; m = 1, ..., M_k\}}
#' 
#' @examples
#' options(digits = 3)
#' options(scipen = 10)
#' set.seed(5)
#' ddd = df_twoStage(NTotal = 10, K = 2)
#' d = ddd$d
#' d[9,] = d[1,]
#' d[9,]$e = 0
#' d[9,]$k = 1
#' d[1,]$e = 0
#' d[7,]$e = 0
#' d[3,]$e = 2
#' d[8,]$e = 0
#' d[8,]$c = "B"
#' rownames(d) = NULL
#' d  # A very doctored data set that will give 
#' # a tiny example with quite general features.
#' N = ddd$N
#' N["B"] = 6
#' N  #First stage numbers
#' n = ddd$n
#' n["B"] = 3
#' n  # Second stage numbers
#' d_k_equals_1 = cbind(
#'   d[d$k == 1,c("e", "t", "c")], 
#'   aaa = N[d[d$k == 1,]$c]/n[d[d$k == 1,]$c], 
#'   k = 1)
#' d_k_equals_1 # The relevant columns of d for k = 1.
#' d_k_equals_2 = cbind(
#'   d[d$k == 2,c("e", "t", "c")], 
#'   aaa = N[d[d$k == 2,]$c]/n[d[d$k == 2,]$c], 
#'   k = 2)
#' d_k_equals_2 # Ditto for k = 2.
#' baseArgs = baseArgsFn(
#'   e = d$e, t = round(d$t,2), r = round(d$r,2), tStar = 10,
#'   design = list(N_two_stage = N, c = d$c),
#'   riskGroup = list(k = d$k), rSummary = "median", bootstrap = FALSE)
#' V2StageFn(baseArgs)
#' 
#' @export

V2StageFn = function(baseArgs = FALSE, extraArgs = FALSE) {
  VVV = if("VVV" %in% names(extraArgs)) extraArgs$VVV else VVVFn(baseArgs, extraArgs)
  if(!all(baseArgs$N_two_stage == baseArgs$n_two_stage)) {
    B2 = if("B2" %in% names(extraArgs)) extraArgs$B2 else B2Fn(baseArgs, extraArgs)
    VVV + VVV %*% B2 %*% VVV
  } else {
    VVV
  }
}

# Wed Mar 16 11:03:13 PDT 2011
