#' The vector of partical derivatives of the loglikelihood
#' 
#' \code{u[n]} is the vector of partial deriviatives of the
#' loglikelihood of the \code{n}-th observation with respect
#' to the paramgers 
#' \code{c(gamma[1], ..., gamma[K-1], lambda)}
#' 
#' @param baseArgs A list provided by \code{baseArgsFn}.
#' This function calls either calls
#' \code{gammaHatFn} and \code{lambdaHatFn} or uses values
#' calculated by them
#' so all objects requried by either of these functions are
#' also required by \code{uuuFn}
#' 
#' @param extraArgs A list of named elements which can contain
#' \code{gammaHat}, \code{lambdaHat}, \code{piHat}, and \code{Sigma}.
#' This argument gives access to intermediate values 
#' previously calculated.
#' 
#' @return A matrix with one row for each observation and one
#' column for each parameter requiring estimation.  The observations
#' are ordered by their risk group membership beginning with
#' \code{k = 1} up to \code{k = K}.  The parameters are ordered
#' \code{gamma[1]}, ..., \code{gamma[K-1]}, followed by 
#' \code{lambda[kem]} with \code{k} moving most slowly, 
#' \code{e = 1, 2} moving next most slowly, and then 
#' \code{m = 1, ..., M_k} moving the fastest.
#' The matrix has \code{N} rows (where \code{N} is the number of
#' observations), and
#' \code{K - 1 + 2(M_1 + ... + M_K)} columns.  See Equations
#' (34) and (52) in "rmap-formulas-v02.pdf" from the website.
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
#' d  # A very doctored data set that will give a 
#'    # tiny example with quite general features.
#' N = ddd$N
#' N["B"] = 6
#' N  #First stage numbers
#' n = ddd$n
#' n["B"] = 3
#' n  #Second stage numbers
#' d_k_equals_1 = cbind(
#'     d[d$k == 1, c("e", "t", "c")], 
#'     aaa = N[d[d$k == 1,]$c]/n[d[d$k == 1,]$c], 
#'     k = 1)
#' d_k_equals_1 #The relevant columns of d for k = 1.
#' d_k_equals_2 = cbind(
#'     d[d$k == 2,c("e", "t", "c")], 
#'     aaa = N[d[d$k == 2,]$c]/n[d[d$k == 2,]$c], 
#'     k = 2)
#' d_k_equals_2 #Ditto for k = 2.
#' e = d$e
#' t = round(d$t, 2)
#' r = round(d$r, 2)
#' tStar = 10
#' design = list(N_two_stage = N, c = d$c)
#' riskGroup = list(k = d$k)
#' rSummary = "median"
#' bootstrap = FALSE
#' baseArgs = baseArgsFn(
#'   e, t, r, tStar,
#'   design, riskGroup, rSummary, bootstrap)
#'  uuu = uuuFn(baseArgs)
#'  uuu
#'  
#' @export

uuuFn = function(baseArgs = FALSE, extraArgs = FALSE) {
  gammaHat = if("gammaHat" %in% names(extraArgs)) extraArgs$gammaHat else gammaHatFn(baseArgs)
  lambdaHat = if("lambdaHat" %in% names(extraArgs)) extraArgs$lambdaHat else lambdaHatFn(baseArgs)
  eventUni = 0:2
  
  uuuGamma = sapply(1:(baseArgs$K - 1), function(kkk) {
    (baseArgs$k[order(baseArgs$k)] == kkk) / gammaHat[kkk] - (baseArgs$k[order(baseArgs$k)] == baseArgs$K) / gammaHat[baseArgs$K] ###DJDJ
  })
                                        # Had to reorder based on the order of $k to get results consistent with rime 0.00.008

  uuuLamba = BlockDiagFn(lapply(1:baseArgs$K, function(kkk) {
    do.call(cbind, structure(lapply(eventUni[-1], function(eee) {
      
      tau_k = lambdaHat[[kkk]]$tau
      DDD_k = lambdaHat[[kkk]]$DDD
      lambdaHat_k = lambdaHat[[kkk]]$lambdaHat
      NAR_k = lambdaHat[[kkk]]$NAR
      
      term1 = sapply(seq_along(tau_k), function(m) DDD_k[[eee]][,m] / lambdaHat_k[eee,m])
      term1 = ifelse(is.nan(term1), 0, term1)
      term2 = sapply(seq_along(tau_k), function(m) (!DDD_k[[eee]][,m]) / (1 - lambdaHat_k[eee,m]) )
      term2 = sapply(seq_along(tau_k), function(m)
        (1-DDD_k[[1]][,m]-DDD_k[[2]][,m]) / (1 - lambdaHat_k[1,m]- lambdaHat_k[2,m]) )
      term2 = ifelse(is.nan(term2) | is.infinite(term2), 0, term2)
      NAR_k * (term1 - term2)
      
    }), .Names = paste("e", eventUni[-1], sep = "") ))
  }))

  uuu = if(baseArgs$K > 1) {
    cbind(uuuGamma, uuuLamba)
  } else {
    uuuLamba
  }
  uuu
}

# Wed Mar 16 11:01:13 PDT 2011
