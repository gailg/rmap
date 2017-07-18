#' The hazard for each risk group
#' 
#' Compute \code{lambdaHat}, \code{NAR}, \code{tau}, and \code{denom}
#' for each risk group.
#' 
#' @param baseArgs A list provided by \code{baseArgsFn}.
#' Objects required by \code{lambdaHatFn} are
#' \code{c},
#' \code{e},
#' \code{K},
#' \code{k},
#' \code{t},
#' \code{tStar}, and
#' \code{weight}.
#' 
#' @return A list with \code{K} lists, each list containing
#' \itemize{
#' \item{\code{tau}: }{
#' Ordered failure times for the risk group where failure time
#' can be times for which \code{e = 1} or {2}.
#' }
#' \item{\code{NAR}: }{Number at risk at time \code{tau}.  
#' A logical matrix with one row for each subject in the risk
#' group and one column for each \code{tau}.  
#' Indexing the subjects in the risk group with \code{n}
#' and the elements of \code{tau} with \code{m}, 
#' \code{NAR[n, m]} is \code{TRUE} if \code{t[n] >= tau[m]}.
#' See equation (29) in "rmap-formulas-v02.pdf" from the website.
#' }
#' \item{\code{DDD}: }{Disease or death at times \code{tau}.
#' \code{DDD} is a list of two elements, the first for 
#' \code{e = 1} (outcome) and the second for \code{e = 2} (death).
#' Both elements is a logical matrix with one row for each
#' subject in the risk group and one column for each \code{tau}.
#' The \code{(n, m)}-th element of  \code{DDD[[1]]} is \code{TRUE}
#' if \code{e[n] == 1} and \code{t[n] == tau[m]}.
#' The \code{(n, m)}-th element of \code{DDD[[2]]} is \code{TRUE}
#' if \code{e[n] == 2} and \code{t[n] == tau[m]}.
#' See equation (30) in "rmap-formulas-v02.pdf" from the website.
#' }
#' \item{\code{denom}: }{The number at risk at time \code{tau}
#' calculated by getting the column-wise weighted sums of \code{NAR}.
#' See equation(61)
#' }
#' \item{\code{lambdaHat}: }{The estimated hazard rate at times 
#' \code{tau} for event = 1 (disease) or event = 2 (death from 
#' other causes).
#' }
#' }
#' 
#' @examples 
#' set.seed(1)
#' twoStageSample = df_twoStage(20)
#' tStar = 10
#' xxx = twoStageSample$d
#' e = xxx$e
#' t = round(xxx$t, 1)
#' r = xxx$r
#' N = twoStageSample$N
#' design = list(N_two_stage = N, c = xxx$c)
#' riskGroup = list(K = 2)
#' rSummary = "mean"
#' bootstrap = FALSE
#' baseArgs = baseArgsFn(e, t, r, tStar, design, riskGroup, rSummary, bootstrap)
#' with(baseArgs, data.frame(e, t, r, k, weight))
#' lambdaHat = lambdaHatFn(baseArgs)
#' names(lambdaHat)
#' lambdaHat[["k1"]]
#' lambdaHat[["k2"]]
#' 
#' @export 
lambdaHatFn = function(baseArgs) {
  tStar = baseArgs$tStar
  eventUni = 0:2
  structure(lapply(1:baseArgs$K, function(kkk) {
    ind = baseArgs$k == kkk
    t_k = baseArgs$t[ind]
    e_k = baseArgs$e[ind]
    c_k = baseArgs$c[ind]
    weight_k = baseArgs$weight[ind]
    tau = sort(unique(t_k[e_k != 0 & t_k < tStar]))  # GG 2017-05-26 this to compensate for allowing t bigger than tStar
    NAR = sapply(tau, function(tauThis) t_k >= tauThis)
    DDD = lapply(eventUni[-1], function(eee) {
      sapply(tau, function(tauThis) (t_k == tauThis) & (e_k == eee))
    })
    denom = colSums(do.call(rbind,lapply(1:sum(ind), function(index) {
      NAR[index, ] * weight_k[index]
    })))
    lambdaHat = do.call(rbind, lapply(DDD, function(DDD1) {
      colSums(do.call(rbind,lapply(1:sum(ind), function(index) {
        DDD1[index, ] * weight_k[index]
      })))/denom
    }))
    tauNames = paste("tau", seq_along(tau), sep = "")
    DDD = lapply(DDD, function(DDD1) {colnames(DDD1) = tauNames; DDD1})
    colnames(lambdaHat) = colnames(NAR) = tauNames
    rownames(lambdaHat) = paste("event", seq_along(eventUni[-1]), sep = "")
    list(lambdaHat = lambdaHat, NAR = NAR, DDD = DDD, tau = tau, denom = denom)
  }), .Names = paste("k", 1:baseArgs$K, sep = ""))
}