probability_outcome_fn = function(lambda_1, lambda_2, t_star){
  lambda_1 /(lambda_1 + lambda_2) * (1 - exp(-(lambda_1 + lambda_2)*t_star))
}