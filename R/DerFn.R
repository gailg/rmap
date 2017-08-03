#' \code{DerFn}
#' 
#'  This function is usually called internally by 
#'  grouped \code{rmap}.  This function computes \code{Der},  
#'  the derivative of \code{c(gammaHat[1], ..., gamma[K-1], piHat)} 
#'  with respect to \code{c(gammaHat[1], ..., gamma[K-1], lambdaHat)}. 
#'  \code{Der} is a matrix used in the delta method to compute 
#'  \code{Sigma}, the covariance matrix for parameters 
#'  \code{c(gammaHat[1], ..., gamma[K-1], piHat)}.
#'  
#' @param baseArgs A list provided by \code{baseArgsFn}.
#' This function calls either calls
#' \code{gammaHatFn} and \code{lambdaHatFn} or uses values
#' calculated by them
#' so all objects requried by either of these functions are
#' also required by \code{uuuFn}.  Also required here is
#' \code{K}.
#' 
#' @param extraArgs A list of named elements which can contain
#' \code{gammaHat}, \code{lambdaHat}, \code{piHat}, and \code{Sigma}.
#' This argument gives access to intermediate values 
#' previously calculated.
#' 
#' @return A matrix.  Define \code{K} to be the number of risk groups.  
#' There is a column for each parameter 
#' \code{(gammaHat[1], ..., gamma[K-1], piHat)} for a total of 
#' \code{2 * K - 1} columns.  Define \code{M_k} to be the 
#' length of \code{tau} (see \code{lambdaHatFn}) for the \code{k}th 
#' risk group.  The number of rows in this
#' \code{matrix will be K - 1 + 2 * (M_1 + ... + M_K)} .  
#' See equation (92) in "rmap-formulas-v01.pdf" from the website.
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
#' d  # A very doctored data set that will give a 
#' # tiny example with quite general features.
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
#'   riskGroup = list(k = d$k), rSummary = "median", bootstrap = FALSE)
#' Der = DerFn(baseArgs)
#' Der
#' 
#' @export

DerFn = function(baseArgs = FALSE, extraArgs = FALSE) {
  lambdaHat = if("lambdaHat" %in% names(extraArgs)) extraArgs$lambdaHat else lambdaHatFn(baseArgs)

  lambdaBoxes = lapply( 1:baseArgs$K, function(kkk) {
    lambdaHat[[kkk]]$lambdaHat
  })

  DerBoxes = lapply(seq_len(baseArgs$K), function(kkk) {
    lambdaBoxes_k = lambdaBoxes[[kkk]]
    lambdaDot = colSums(lambdaBoxes_k)
    num = cumprod(1 - lambdaDot)
    MMM = length(num)
    Der2 = -sapply(seq_len(MMM), function(mmm) {
      if(mmm == MMM) {  
        0
      } else {
        sum(sapply((mmm+1):MMM, function(mPrime) {
          lambdaBoxes_k[1, mPrime] * num[mPrime - 1] / (1 - lambdaDot[mmm])
        }))
      }
    })
    Der1 = c(1, num[-length(num)]) + Der2
    matrix(c(Der1, Der2), ncol = 1)
  })

  Der_lambda = BlockDiagFn(DerBoxes)
  Der = if(baseArgs$K > 1) {
    BlockDiagFn(list(diag(rep(1, baseArgs$K - 1)), Der_lambda))
  } else {
    Der_lambda
  }

  Der
}

# Wed Mar 16 11:03:38 PDT 2011
