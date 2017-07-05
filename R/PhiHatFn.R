#' PhiHat is an intermediate step in the two-stage sampling variance
#' 
#' \code{PhiHatFn} computes the "PhiHat part" of equation (82) 
#' in "rmap-formulas-v02.pdf" from the website.  
#' The "PhiHat part" of this equation refers to: 
#' \\hat{\\omega_c} \\frac{1 - \\hat{p_c}}{\\hat{p_c}} \\frac{\\bar{N_c}}{\\bar{N_c} - 1} \\hat{\\Phi_c}
#' This function computes the above formula for one sampling category.
#' This function is called by \code{B2Fn} and computes the \code{c}-th
#' value of \code{PhiHatPart}.  \code{B2Fn} sums over \code{c}.
#' 
#' @param omegaHat The proportion of first stage subjects in the \code{c}-th category.  
#' See equation (7) in "rmap-formulas-v02.pdf" from the website.
#' @param pHat The proportion of first stage subjects in the c-th category
#' that made it to the second stage.  See equation (8) in "rmap-formulas-v02.pdf" 
#' from the website. 
#' @param nnn The number of second stage subjects in the \code{c}-th risk group. 
#' @param PhiHat A square matrix for the \code{c}-th risk group.  
#' See the first term in equation (84) in "rmap-formulas-v01.pdf" 
#' from the website.  The dimension of this matrix is 
#' \code{(K - 1) + 2 * (M_1 + ... + M_K)} where \code{M_k} 
#' is the number of unique times to disease (\code{e = 1}) or death 
#' (\code{e = 2}) in the \code{k}-th risk group.  
#' 
#' @return A symmetric square matrix with numbers of rows and columns
#' equal to \code{(K - 1) + 2 * (M_1 + ... + M_K)}.
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
#'   riskGroup = list(k = d$k), rSummary = "median", bootstrap = FALSE)
#' uuu = uuuFn(baseArgs)
#' cUni = names(baseArgs$N_two_stage)
#' PhiHat = structure(lapply(cUni, function(ccc) {
#'   uuu_c = uuu[baseArgs$c[order(baseArgs$k)] == ccc, ]   
#'   uuT = vvTsumFn(uuu_c)
#'   uuT / nrow(uuu_c)
#' }), .Names = cUni)
#' PhiHat
#' omegaHat = baseArgs$N_two_stage / sum(baseArgs$N_two_stage)
#' omegaHat
#' pHat = baseArgs$n_two_stage / baseArgs$N_two_stage
#' pHat
#' PhiHatFn(omegaHat[1], pHat[1], 
#'   baseArgs$n_two_stage[1], PhiHat[[1]]) # Vanishes because pHat = 1
#' PhiHatFn(omegaHat[2], pHat[2], 
#'   baseArgs$n_two_stage[2], PhiHat[[2]])
#' PhiHatPart = apply(
#'   array(unlist(mapply(PhiHatFn, 
#'                       omegaHat, 
#'                       pHat, 
#'                       baseArgs$n_two_stage, 
#'                       PhiHat, SIMPLIFY = FALSE)), 
#'         c(ncol(uuu), ncol(uuu), length(cUni))), 
#'   c(1,2), sum)
#' PhiHatPart
#' 
#' @export
PhiHatFn = function(omegaHat, pHat, nnn, PhiHat){
   omegaHat * ((1 - pHat)/pHat) * PhiHat * nnn / (nnn - 1) 
 }

# Wed Mar 16 11:01:35 PDT 2011
