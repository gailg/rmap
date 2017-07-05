#' \code{VVVFn}
#' 
#' \code{VVV} is the covariance matrix for 
#' \code{c(gammaHat[1], ..., gamma[K-1], lambdaHat)} under random sampling.
#' It is an intermediate step in the calculation of \code{Sigma}
#' which is used to obtain confidence intervals for pi 
#' and to obtain the Hosmer-Lemeshow
#' statistic. This function is usually called internally by grouped 
#' \code{rmap}
#' 
#' @param baseArgs A list provided by \code{baseArgsFn}.
#' This function calls either calls
#' \code{gammaHatFn} and \code{lambdaHatFn} or uses values
#' calculated by them
#' so all objects requried by either of these functions are
#' also required by \code{uuuFn}.  Also required here are 
#' \code{K}, \code{k}, \code{N_two_stage}
#' 
#' @param extraArgs A list of named elements which can contain
#' \code{gammaHat}, \code{lambdaHat}, \code{piHat}, and \code{Sigma}.
#' This argument gives access to intermediate values 
#' previously calculated.
#' 
#' @return A block-diagonal matrix.  Define \code{K} to be 
#' the number of risk groups.  Define \code{M_k} to be the 
#' length of tau for risk group \code{k} (see \code{lambdaHatFn}).
#' Down the diagonal of \code{VVV} are \code{K+1} submatrixes or boxes.
#' The first box has dimension \code{(K-1) x (K-1)} 
#' and is given by equation (51) in "rmap-formulas-v02.pdf" from the website.
#' The second box is for \code{k = 1}, and so on until the (\code{K+1})th 
#' box for \code{k = K}.  The \code{k}th box has row headings
#' \code{\{(e1,m)| e1 = 1, 2 and m = 1, ... M_k\}} where \code{e1} 
#' grows more slowly than \code{m}, and  column headings 
#' \code{\{(e2,m)| e2 = 1, 2 and m = 1, ... M_k\}}. The entry in each 
#' cell in the box is \code{V_\{k, e1, e2, m\}} given in Equation (75) 
#' in "rmap-formulas-v02.pdf" from the website. 
#' The number of rows and columns of \code{VVV} is 
#' \code{(K-1) + 2 * (M_1 + ... + M_K)}.
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
#'   e = d$e, t = round(d$t,2), r = round(d$r,2), 
#'   tStar = 10, design = list(N_two_stage = N, c = d$c),
#'   riskGroup = list(k = d$k), rSummary = "median", bootstrap = FALSE)
#' lambdaHat = lambdaHatFn(baseArgs)
#' lambdaHat[["k1"]]$tau
#' lambdaHat[["k2"]]$tau
#' VVV = VVVFn(baseArgs)
#' VVV # The column and row labels are 
#'     #  c(gamma1, c(kem = 111,112,113,121,122,123,211,212,221,222))
#' 
#' @export

VVVFn = function(baseArgs = FALSE, extraArgs = FALSE) {
  gammaHat = if("gammaHat" %in% names(extraArgs)) extraArgs$gammaHat else gammaHatFn(baseArgs)
  lambdaHat = if("lambdaHat" %in% names(extraArgs)) extraArgs$lambdaHat else lambdaHatFn(baseArgs)

  VVVgamma = if(length(gammaHat) >= 3) {
    diag(gammaHat[1:(baseArgs$K - 1)]) - (gammaHat[1:(baseArgs$K - 1)] %*% t(gammaHat[1:(baseArgs$K - 1)] ) )
  } else if(length(gammaHat) == 2) {
    gammaHat[1:(baseArgs$K - 1)] - (gammaHat[1:(baseArgs$K - 1)] %*% t(gammaHat[1:(baseArgs$K - 1)] ) )
  } else if(length(gammaHat) == 1) {
    matrix(0, 0, 0)
  }

  lambdaDenoms = lapply(seq_len(baseArgs$K), function(kkk) lambdaHat[[kkk]]$denom)

  lambdaBoxes = lapply( 1:baseArgs$K, function(kkk) {
    lambdaHat[[kkk]]$lambdaHat
  })

  VBoxes = lapply( seq_len(baseArgs$K), function(kkk) {
    lambdaBoxes_k = lambdaBoxes[[kkk]]
    MMM = ncol(lambdaBoxes_k)
    squares = lapply(seq_len(MMM), function(mmm) {
      lambda1and2 = lambdaBoxes_k[, mmm]
      diag(lambda1and2) - ( lambda1and2 %*% t(lambda1and2) )
    })
    lambDenoms_k = lambdaDenoms[[kkk]]
    squaresArray = array( unlist(squares) / rep(lambDenoms_k, rep(4, length(lambDenoms_k))),
      dim = c(2, 2, MMM))
    ## V_k = sum(baseArgs$N)  * rbind(cbind( diag(squaresArray[1,1,]), diag(squaresArray[1,2,])),
    ##   cbind( diag(squaresArray[2,1,]), diag(squaresArray[2,2,])))
    V_k = if(MMM ==1) {
      sum(baseArgs$N_two_stage) * squaresArray[,,1]
    } else {
      sum(baseArgs$N_two_stage)  * rbind(cbind( diag(squaresArray[1,1,]), diag(squaresArray[1,2,])),
                               cbind( diag(squaresArray[2,1,]), diag(squaresArray[2,2,])))
    }
    V_k
  })

  VVVlambda = BlockDiagFn(VBoxes)
  
  VVV = if(baseArgs$K > 1 ) {
    BlockDiagFn(list(VVVgamma, VVVlambda))
  } else {
    VVVlambda
  }

  VVV  
}
## Gail
## Thu Apr 28 13:01:22 PDT 2011
## Our previous definition for V_k
    ## V_k = sum(baseArgs$N)  * rbind(cbind( diag(squaresArray[1,1,]), diag(squaresArray[1,2,])),
    ##   cbind( diag(squaresArray[2,1,]), diag(squaresArray[2,2,])))
## does not work when there is just one tau in a risk group.
## I enclosed the definition in an if statement so MMM = 1 can be handled correctly.
