rmap_individual = function(e, t, r, t_star, design, risk_group, N_bootstraps, 
                          confidence_level = 0.95, N_cores = 0, verbose = FALSE){
  r_summary = "mean"
  baseArgs = base_args_fn(
    e, t, r, t_star, design, risk_group, r_summary, N_bootstraps, 
    confidence_level, N_cores, verbose)
  rmap_individual_fn(baseArgs)
}