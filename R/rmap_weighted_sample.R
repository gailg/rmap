rmap_weighted_sample = function (
  e, t, r, category, target_category, t_star, cutoffs, N_bootstraps){
  design = list(target_category = target_category, category = category)
  risk_group = list(cutoffs = cutoffs)
  r_summary = "mean"
  confidence_level = 0.95
  rmap_answers = rmap(e, t, r, t_star, design, risk_group, r_summary, 
                      N_bootstraps, confidence_level)
  epsilon = length(e)^(-1/3)
  risk_group = list(epsilon = epsilon)
  individual = rmap_individual(e, t, r, t_star, design, risk_group,
                               N_bootstraps, confidence_level)
  
  list(rmap_answers = rmap_answers,
       individual = individual)
}