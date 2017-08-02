rmap_random_sample = function(e, t, r, t_star, K, N_bootstraps){
  design = "random_sample"
  risk_group = list(K = K)
  r_summary = "mean"
  confidence_level = 0.95
  rmap_answers = rmap(e, t, r, t_star, design, risk_group, r_summary, 
                      N_bootstraps, confidence_level)
  epsilon = length(e)^(-1/3)
  individual = rmap_individual(e, t, r, t_star, design, epsilon, 
                               N_bootstraps, confidence_level)
  list(rmap_answers = rmap_answers,
       individual = individual)
}
