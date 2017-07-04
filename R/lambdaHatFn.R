#' Compute lambdaHat, NAR, DDD, tau, and denom for each risk group.
#' 
#' This function, one of the first internal calculations that occur inside 
#' grouped \code{rmap}, puts together 
#' several data structures that are used in subsequent calculations 
#' namely in \code{uuuFn} and \code{VVVFn}
#' 
#' @param baseArgs A list provided by \code{baseArgsFn}.
#' The objects required here are
#' \code{e}, \code{t}, \code{c}
#' \code{K}, \code{k},
#' \code{N_two_stage}, \code{n_two_stage}, and
#' \code{tStar}.
#' 
#' @return A named list of \code{K} lists, one list for each risk group 
#' containing the objects
#' \itemize{
#'   \item{\code{tau}: }{ Ordered failure times for current risk group.  
#'   It is the vector of unique times for subjects in risk group \code{k} 
#'   for which that subject's event (\code{e}) is \code{1} (disease) or \code{2} 
#'   (death from other causes).  The length of \code{tau} is \code{M_k}.
#'   See equation (23) in "rmap-formulas-v02.pdf" from the website.
#'   }
#'   \item{\code{NAR}: }{  Number at Risk at times \code{tau}. 
#'   A logical matrix with one row for each subject in risk group \code{k} 
#'   and one column for each \code{tau}.  The \code{(n, m)}th element of 
#'   \code{NAR} is \code{TRUE} if the \code{n}-th subject in risk group \code{k}
#'    is at risk at time \code{tau[m]}. In other words, \code{NAR[n, m]} is 
#'    \code{TRUE} if \code{t[n] >= tau[m]}. 
#'   }
#'   \item{\code{DDD}: }{ Disease or Death at times \code{tau}.  
#'   \code{DDD} is a list containing two elements.   The first element is for 
#'   \code{e = 1} (disease), and the second element is for \code{e = 2} 
#'   (death from other causes).  Both elements in \code{DDD} will be a logical 
#'   matrix with one row for each subject in risk group \code{k} and one column 
#'   for each \code{tau}.  The \code{(n, m)}th element of \code{DDD[[1]]}
#'    will be \code{TRUE} if \code{e[n] == 1} and \code{t[n] == tau[m]}, and 
#'    \code{FALSE} otherwise.  Likewise, the \code{(n, m)}th element of 
#'    \code{DDD[[2]]} will be \code{TRUE} if \code{e[n] == 2} and 
#'    \code{t[n] == tau[m]}, and \code{FALSE} otherwise.  
#'    See equation (30) in "rmap-formulas-v02.pdf" from the website.
#'   }
#'   \item{\code{denom}: }{ The number at risk at times \code{tau}
#'   If using random sampling, this will simply be the column-wise sums of 
#'   \code{NAR}.  If using two-stage sampling, these values will be weighted 
#'   according to two-stage sampling weights.  See equation (61) in 
#'   "rmap-formulas-v02.pdf" from the website.
#'   }
#'   \item{\code{lambdaHat}: }{ The estimated hazard rate at times 
#'   \code{tau} for event \code{e = 1} (disease) or event \code{e = 2 }
#'   (death from other causes). \code{lambdaHat} is a list with K elements.  
#'   A numeric matrix with two rows (for \code{e = 1} and \code{e = 2})
#'   and \code{M_k} columns, where \code{M_k} is the length of \code{tau}.
#'   See equation (68) "rmap-formulas-v02.pdf" from the website where 
#'   lambda_{kem} is the hazard of e at time \code{tau[m]}.
#'   }
#' }
#' @examples 
#' options(digits = 3)
#' options(scipen = 10)
#' set.seed(5)
#'  ddd = df_twoStage(NTotal = 10, KKK = 2)
#'  d = ddd$d
#'  d[9,] = d[1,]
#'  d[9,]$e = 0
#'  d[9,]$k = 1
#'  d[1,]$e = 0
#'  d[7,]$e = 0
#'  d[3,]$e = 2
#'  d[8,]$e = 0
#'  d[8,]$c = "B"
#'  rownames(d) = NULL
#'  d  # A very doctored data set that will give a 
#'    # tiny example with quite general features.
#'  N = ddd$N
#'  N["B"] = 6
#'  N  #First stage numbers
#'  n = ddd$n
#'  n["B"] = 3
#'  n  #Second stage numbers
#'  d_k_equals_1 = cbind(
#'     d[d$k == 1,c("e", "t", "c")], 
#'     aaa = N[d[d$k == 1,]$c]/n[d[d$k == 1,]$c], 
#'     k = 1)
#'  d_k_equals_1 #The relevant columns of d for k = 1.
#'  d_k_equals_2 = cbind(
#'     d[d$k == 2,c("e", "t", "c")], 
#'     aaa = N[d[d$k == 2,]$c]/n[d[d$k == 2,]$c], 
#'     k = 2)
#'  d_k_equals_2 #Ditto for k = 2.
#'  e = d$e
#'  t = round(d$t, 2)
#'  r = round(d$r, 2)
#'  tStar = 10
#'  design = list(N_two_stage = N, c = d$c)
#'  riskGroup = list(k = d$k)
#'  rSummary = "median"
#'  bootstrap = FALSE
#'  baseArgs = baseArgsFn(
#'   e, t, r, tStar,
#'   design, riskGroup, rSummary, bootstrap)
#'   lambdaHat = lambdaHatFn(baseArgs)
#'   names(lambdaHat)
#'   names(lambdaHat$k1)
#'   lambdaHat[["k1"]]$tau
#'   lambdaHat[["k1"]]$lambdaHat
#'   lambdaHat[["k2"]]$tau
#'   lambdaHat[["k2"]]$lambdaHat
#' @export

lambdaHatFn = function(baseArgs) {
  aaa = baseArgs$N_two_stage / baseArgs$n_two_stage
  tStar = baseArgs$tStar
  eventUni = 0:2
  structure(lapply(1:baseArgs$K, function(kkk) {
    ind = baseArgs$k == kkk
    t_k = baseArgs$t[ind]
    e_k = baseArgs$e[ind]
    c_k = baseArgs$c[ind]
    tau = sort(unique(t_k[e_k != 0 & t_k < tStar]))  # GG 2017-05-26 this to compensate for allowing t bigger than tStar
    NAR = sapply(tau, function(tauThis) t_k >= tauThis)
    
    DDD = lapply(eventUni[-1], function(eee) {
      sapply(tau, function(tauThis) (t_k == tauThis) & (e_k == eee))
    })
    
    denom = colSums(do.call(rbind,lapply(1:sum(ind), function(index) {
      NAR[index, ] * aaa[c_k][index]
    })))
    
    lambdaHat = do.call(rbind, lapply(DDD, function(DDD1) {
      colSums(do.call(rbind,lapply(1:sum(ind), function(index) {
        DDD1[index, ] * aaa[c_k][index]
      })))/denom
    }))
    
    tauNames = paste("tau", seq_along(tau), sep = "")
    DDD = lapply(DDD, function(DDD1) {colnames(DDD1) = tauNames; DDD1})
    colnames(lambdaHat) = colnames(NAR) = tauNames
    rownames(lambdaHat) = paste("event", seq_along(eventUni[-1]), sep = "")
    
    list(lambdaHat = lambdaHat, NAR = NAR, DDD = DDD, tau = tau, denom = denom)
  }), .Names = paste("k", 1:baseArgs$K, sep = ""))
}
## Gail
## Thu Apr 28 13:03:20 PDT 2011
## The t-sapply in
##     denom = colSums(t(sapply(1:sum(ind), function(index) {
##       NAR[index, ] * aaa[c_k][index]
##     })))
## and 
## lambdaHat = t(sapply(DDD, function(DDD1) {
##   colSums(t(sapply(1:sum(ind), function(index) {
##     DDD1[index, ] * aaa[c_k][index]
##   }))) / denom
## }))
## does not do the right thing when there is one tau in a risk group.
## For example, where we expect a matrix with one column, t-sapply gives us a vector.
## Replace with do.call-rbind-lapply.
