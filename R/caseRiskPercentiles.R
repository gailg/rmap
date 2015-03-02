caseRiskPercentiles = function(cutoff, e, t, tStar, r, rAnother = NULL, ...){
  epsilon = 0.01
  design = "randomSample"
  riskGroup = list(
    ungrouped = list(epsilon = epsilon, tStar = tStar))
  bootstrap = FALSE
  rSummary = "mean"
  extraArgs = FALSE
  r1 = r
  baseArgs = baseArgsFn(
    e = e, t = t, r = r1, 
    design = design, riskGroup = riskGroup, rSummary = "mean", 
    bootstrap = FALSE, multicore = FALSE, verbose = FALSE)
  crp1 = CRP_Fn(baseArgs, extraArgs = extraArgs)$CRP
  if(is.null(rAnother)){
    crp = list(
      N_cases = length(crp1),
      the_proportion_of_crps_above_cutoff = mean(crp1 > cutoff),
      crp = crp1)
  } else{
    r2 = rAnother
    baseArgs = baseArgsFn(
      e = e, t = t, r = r2, 
      design = design, riskGroup = riskGroup, rSummary = "mean", 
      bootstrap = FALSE, multicore = FALSE, verbose = FALSE)
    crp2 = CRP_Fn(baseArgs, extraArgs = extraArgs)$CRP
    plot(crp1, crp2,  ...)
    abline(a = 0, b = 1, col = "red", lty = 2, cex = 0.5)
    abline(h = cutoff, col = "blue", lty = 2, cex = 0.5)
    abline(v = cutoff, col = "blue", lty = 2, cex = 0.5)
    crp = list(N_cases = length(crp1),
               the_proportion_of_points_below_diagonal = mean(crp1 > crp2),
               the_proportion_of_crps_above_cutoff_for_model_r = mean(crp1 > cutoff),
               the_proportion_of_crps_above_cutoff_for_model_rAnother = mean(crp2 > cutoff),
               crp1 = crp1,
               crp2 = crp2)
  }
  class(crp) = c("crp", class(crp))
  crp
}
