base_args_boot_fn = function(baseArgs, seed) {
  set.seed(seed)
  N_c = length(baseArgs$cohort_category)
  N_p = length(baseArgs$target_category)
  
  bootstrap_sample = sample(1:N_c, N_c, replace = TRUE)
  target_bootstrap_sample = sample(1:N_p, N_p, replace = TRUE)
  KKK = baseArgs$K
  e_boo = baseArgs$e[bootstrap_sample]
  t_boo = baseArgs$t[bootstrap_sample]
  r_boo = baseArgs$r[bootstrap_sample]
  c_boo = baseArgs$c[bootstrap_sample]
  k_boo = baseArgs$k[bootstrap_sample]
  target_category_boo = baseArgs$target_category[target_bootstrap_sample]
  N_in_risk_group = unlist(lapply(seq(1, KKK, by = 1), function(kkk){
    sum(k_boo == kkk)
  }))
  N_in_risk_group_message = paste(
    "risk groups", 
    paste(which(N_in_risk_group == 0), collapse = ", "),
    "are empty")
  N_nonzero_events = unlist(lapply(seq(1, KKK, by = 1), function(kkk){
    e_inside_pi_hat = ifelse(t_boo[k_boo == kkk] > tStar, 0, e_boo[k_boo == kkk])
    sum(e_inside_pi_hat > 0)
  }))
  
  weight_0 = weight_fn(c_boo, target_category_boo)
  weight_code = weight_0$code
  weight_message = weight_0$message
  category_weights_boo = weight_0$category_weights
  weight_boo = unname(category_weights_boo[c_boo]) 
  
  boot_code = if( weight_code == 0 && all(N_in_risk_group > 0) ) {
    0
  } else {
    1
  }
  boot_message = if( weight_code != 0 ){
    weight_message
  } else {
    paste("weight ok but", N_in_risk_group_message)
  }
  
  base_args_boot = list(e = e_boo,
                        t = t_boo,
                        r = r_boo,
                        c = c_boo,
                        k = k_boo,
                        weight = weight_boo,
                        boot_code = boot_code,
                        boot_message = boot_message,
                        bootstrap_sample = bootstrap_sample,
                        target_bootstrap_sample = target_bootstrap_sample,
                        category_weights_boo = category_weights_boo,
                        K = baseArgs$K,
                        N_nonzero_events = N_nonzero_events,
                        target_category_boo = target_category_boo,
                        tStar = baseArgs$tStar)
  base_args_boot
} 