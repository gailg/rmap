#' gammaHat is the proportion of subjects in each risk group
#' 
#' This function computes gammaHat, the proportion of subjects in each
#' risk group.  This function is called internally by 
#' grouped \code{rmap}.
#' It takes two-stage sampling weights into account if a two-stage 
#' sampling design was used.  
#' 
#' @param baseArgs A list provided by \code{baseArgsFn}.
#' The objects required here are
#' \code{c}
#' \code{K}, \code{k},
#' \code{N_two_stage}, and \code{n_two_stage}
#' 
#' @return A numeric vector with one element for each risk group.  
#' The \code{k}-th element of this vector is the proportion of 
#' subjects in risk group k.  
#' See equation (45) in "rmap-formulas-v02.pdf" from the website.
#' 
#' @examples 
#' set.seed(1)
#' d = df_twoStage()
#' e = d$d$e
#' t = d$d$t
#' r = d$d$r
#' tStar = 10
#' design = list(c = d$d$c, N_two_stage = d$N)
#' riskGroup = list(K = 2)
#' rSummary = "median"
#' bootstrap = FALSE
#' ba = baseArgsFn(e, t, r, tStar, 
#'   design, riskGroup, rSummary, bootstrap)
#' gh1 = gammaHatFn(ba)
#' gh1
#' 
#' set.seed(1)
#' d = df_randomSample()
#' baseArgs = baseArgsFn(e, t, r, tStar,
#'   design, riskGroup, rSummary, bootstrap)
#' gh2 = gammaHatFn(baseArgs)
#' gh2
#' 
#' @export

gammaHatFn = function(baseArgs) {
  aaaPerson = baseArgs$weight
  Nk = sapply(1:baseArgs$K, function(kkk) sum(aaaPerson[baseArgs$k==kkk]))
  Nk / sum(Nk)
}

# Wed Mar 16 10:48:40 PDT 2011
