#' \code{B2Fn}
#' 
#' \code{B2} is an intermediate component used to compute \code{Sigma}.
#' This function is necessary only under two-stage sampling, and is one 
#' of the more computationally-intensive parts of the rmap package.  
#' 
#
#' @param baseArgs A list provided by \code{baseArgsFn}.
#' This function calls either calls
#' \code{gammaHatFn} and \code{lambdaHatFn} or uses values
#' calculated by them
#' so all objects requried by either of these functions are
#' also required by \code{uuuFn}.  Also required here are 
#' \code{c}, \code{k},
#' \code{K},  \code{N_two_stage}, and code{n_two_stage}.
#' 
#' @param extraArgs A list of named elements which can contain
#' \code{gammaHat}, \code{lambdaHat}, \code{piHat}, and \code{Sigma}.
#' This argument gives access to intermediate values 
#' previously calculated.
#' 
#' @return A symmetric square matrix with numbers of rows and 
#' columns equal to \code{K - 1 + 2*(M_1 + ... M_K)}. 
#' See Equations (83) and (84) in "rmap-formulas-v02.pdf" from the website.
#' 
#' @examples 
#' options(digits = 3)
#' options(scipen = 10)
#' set.seed(5)
#' ddd = df_twoStage(NTotal = 10, KKK = 2)
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
#'    # a tiny example with quite general features.
#' N = ddd$N
#' N["B"] = 6
#' N  #First stage numbers
#' n = ddd$n
#' n["B"] = 3
#' n  #Second stage numbers
#' d_k_equals_1 = cbind(
#'   d[d$k == 1,c("e", "t", "c")], 
#'   aaa = N[d[d$k == 1,]$c]/n[d[d$k == 1,]$c], 
#'   k = 1)
#' d_k_equals_1 #The relevant columns of d for k = 1.
#' d_k_equals_2 = cbind(
#'   d[d$k == 2,c("e", "t", "c")], 
#'   aaa = N[d[d$k == 2,]$c]/n[d[d$k == 2,]$c], 
#'   k = 2)
#' d_k_equals_2 #Ditto for k = 2.
#' baseArgs = baseArgsFn(
#'   e = d$e, t = round(d$t,2), r = round(d$r,2), tStar = 10,
#'   design = list(N_two_stage = N, c = d$c),
#'   riskGroup = list(k = d$k), 
#'   rSummary = "median", bootstrap = FALSE)
#' B2Fn(baseArgs)
#' 
#' @export

B2Fn = function(baseArgs = FALSE, extraArgs = FALSE) {
  gammaHat = if("gammaHat" %in% names(extraArgs)) extraArgs$gammaHat else gammaHatFn(baseArgs)
  lambdaHat = if("lambdaHat" %in% names(extraArgs)) extraArgs$lambdaHat else lambdaHatFn(baseArgs)
  uuu = if("uuu" %in% names(extraArgs)) extraArgs$uuu else uuuFn(baseArgs)
  
  cUni = names(baseArgs$N_two_stage)
  muHat = t(sapply(cUni, function(ccc) colMeans(uuu[baseArgs$c[order(baseArgs$k)] == ccc, ])))   ###DJDJ
                                        # Had to reorder based on the order of $k to get results consistent with rime 0.00.008

  # FLAG!  This is really slow.  We need to look at it later. 17/34
  PhiHat = structure(lapply(cUni, function(ccc) {
    uuu_c = uuu[baseArgs$c[order(baseArgs$k)] == ccc, ]    ###DJDJ
                                        # Had to reorder based on the order of $k to get results consistent with rime 0.00.008
    uuT = vvTsumFnC(uuu_c)
    uuT / nrow(uuu_c)
  }), .Names = cUni) 

  omegaHat = baseArgs$N_two_stage / sum(baseArgs$N_two_stage)

  pHat = baseArgs$n_two_stage / baseArgs$N_two_stage

  # FLAG! This is also really slow.  10/34 total seconds
  PhiHatPart = apply(array(unlist(mapply(PhiHatFn, omegaHat, pHat, baseArgs$n_two_stage, PhiHat, SIMPLIFY = FALSE)),
      c(ncol(uuu), ncol(uuu), length(cUni))), c(1,2), sum)

  multiplier = omegaHat * ((1 - pHat)/pHat) * baseArgs$n_two_stage / (baseArgs$n_two_stage - 1)
  muHatPart = vvTsumFn(muHat, multiplier)
  
  PhiHatPart - muHatPart
}

# Wed Mar 16 11:02:54 PDT 2011
# Tue May  3 14:50:33 PDT 2011 -- now calls vvTsumFnC instead of vvTsumFn.
