#' Compute the sum of \code{v \%*\% t(v) \%*\% a}
#' 
#' The sum of \code{v \%*\% t(v) \%*\% a} (read \code{v vT a})
#' is an internal 
#' calculation needed to eventually compute the value \code{B2}, 
#' which is used in the two-stage sampling estimate 
#' of the covariance matrix, \code{Sigma}. There is a C-level 
#' version of this function, \code{vvTsumFnC}. 
#' The C-level function is preferred because 
#' it is faster and potentially uses less memory.
#' 
#' @param uuu A matrix, where each row represents a subject
#' and each column represents a partial derivative. 
#' See equation (52) in "rmap-formulas-v02.pdf" from the website.
#' 
#' @param mult A multiplier currenlty not used.
#' 
#' @return A square matrix, with dimensions: 
#' code{(K - 1 + 2 sum_k (M_k) )}, where \code{K} is 
#' the total number of risk groups, \code{k} indexes the risk groups, 
#' and \code{M_k} is the number of unique times to 
#' death or disease for the k-th risk group.
#' 
#' @examples
#' set.seed(1)
#' # A smaller-than-normal dataset: 
#' d = df_twoStage(NTotal = 20)
#' 
#' e = d$d$e
#' t = d$d$t
#' r = d$d$r
#' tStar = 10
#' design = list(c = d$d$c, N_two_stage = d$N)
#' riskGroup = list(K = 2)
#' rSummary = "median" 
#' bootstrap = FALSE
#' baseArgs = baseArgsFn(e, t, r, tStar, design, riskGroup, rSummary, bootstrap)
#' # The innards of B2Fn:
#' uuu = uuuFn(baseArgs)
#' cUni = names(baseArgs$N_two_stage)
#' ccc = cUni[1]
#' uuu_c = uuu[baseArgs$c[order(baseArgs$k)] == ccc, ]
#' uuT_R = vvTsumFn(uuu_c)
#' uuT_R
#' 
#' @export
#' 
vvTsumFn = function(uuu, mult = rep(1, nrow(uuu))) {
  NB1 = matrix( rep(0, ncol(uuu)^2), nrow = ncol(uuu))          
  for(n in 1:nrow(uuu)) {                                           
    row = uuu[n, ]
    u_uT_a = (row %*% t(row) ) * mult[n]
    NB1 = NB1 + u_uT_a
  }
  NB1
}

# Wed Mar 16 11:02:23 PDT 2011
