base_args_boot_fn = function(baseArgs) {
  if(baseArgs$sampling == "weighted"){
    N_c = length(baseArgs$c)
    N_p = length(baseArgs$target_category)
    
    bootstrap_sample = sample(1:N_c, N_c, replace = TRUE)
    target_bootstrap_sample = sample(1:N_p, N_p, replace = TRUE)
    e_boo = baseArgs$e[bootstrap_sample] # need_outside_if
    t_boo = baseArgs$t[bootstrap_sample] # need_outside_if
    r_boo = baseArgs$r[bootstrap_sample] # need_outside_if
    c_boo = baseArgs$c[bootstrap_sample] # need_outside_if
    k_boo = baseArgs$k[bootstrap_sample] # need_outside_if
    target_category_boo = baseArgs$target_category[target_bootstrap_sample]
    weight_0 = weight_fn(c_boo, target_category_boo)
    weight_code = weight_0$code                          # need_outside_if
    weight_message = weight_0$message                    # need_outside_if
    category_weights_boo = weight_0$category_weights
    weight_boo = category_weights_boo[c_boo]             # need_outside_if
  } else {
    indices = suppressWarnings({
      sample(1:sum(baseArgs$n_two_stage), sum(baseArgs$N_two_stage), replace = TRUE,
             prob = (baseArgs$N_two_stage / baseArgs$n_two_stage )[baseArgs$c])
    })
    keep = rbinom(sum(baseArgs$N_two_stage), 
                  1, 
                  (baseArgs$n_two_stage / baseArgs$N_two_stage )[baseArgs$c[indices]])
    indicesTwoStage = indices[as.logical(keep)]
    e_boo = baseArgs$e[indicesTwoStage] # need_outside_if
    t_boo = baseArgs$t[indicesTwoStage] # need_outside_if
    r_boo = baseArgs$r[indicesTwoStage] # need_outside_if
    c_boo = baseArgs$c[indicesTwoStage] # need_outside_if
    k_boo = baseArgs$k[indicesTwoStage] # need_outside_if                     
    c_uni = names(baseArgs$N_two_stage)
    n_two_stage_boo = structure(sapply(c_uni, function(c1){
      sum(c_boo == c1)
    }), .Names = c_uni)
    N_two_stage_boo = structure(sapply(c_uni, function(c1){
      sum(baseArgs$c[indices] == c1)
    }), .Names = c_uni)
    aaa = N_two_stage_boo / n_two_stage_boo
    weight_boo = aaa[c_boo]                            # need_outside_if
    weight_code = 0                                    # need_outside_if
    weight_message = "ok"                              # need_outside_if
  }
  N_in_risk_group = unlist(lapply(seq(1, baseArgs$K, by = 1), function(kkk){
    sum(k_boo == kkk)
  }))
  N_in_risk_group_message = paste(
    "risk groups",
    paste(which(N_in_risk_group == 0), collapse = ", "),
    "are empty")
  N_nonzero_events = unlist(lapply(seq(1, baseArgs$K, by = 1), function(kkk){
    e_inside_pi_hat = ifelse(t_boo[k_boo == kkk] > tStar, 0, e_boo[k_boo == kkk])
    sum(e_inside_pi_hat > 0)
  }))
  boot_code = if( weight_code == 0 && all(N_in_risk_group > 0) ) {
    0
  } else {
    1
  }
  boot_message = if( weight_code == 0 ){
    "All systems go"
  } else {
    "Something wrong with weight or N_in_risk_group"
  }
  base_args_boot = list(
    boot_code = boot_code,
    boot_message = boot_message,
    c = c_boo,
    e = e_boo,
    epsilon = baseArgs$epsilon,
    K = baseArgs$K,
    k = k_boo,
    N_nonzero_events = N_nonzero_events,
    r = r_boo,
    sampling = baseArgs$sampling,
    t = t_boo,
    tStar = baseArgs$tStar,
    verbose = baseArgs$verbose,
    weight = weight_boo)
  base_args_boot
}