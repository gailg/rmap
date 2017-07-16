pi_sd_boot_fn = function(baseArgs){
  random_seeds = sample(1:1e8, baseArgs$N_bootstraps, replace = FALSE)
  boo_0 = lapply(1:baseArgs$N_bootstraps, function(n_bootstrap){
    set.seed(random_seeds[n_bootstrap])
    base_args_boot = base_args_boot_fn(baseArgs)
    concordance_boo = concordance_fn(base_args_boot)$concordance
    pi_hat_boo = if(base_args_boot$boot_code == 0) {
      pi_hat_fn(base_args_boot) 
    } else {
      base_args_boot$boot_message
    }
    list(code = base_args_boot$boot_code,
         concordance = concordance_boo,
         n_bootstrap = n_bootstrap,
         pi_hat = pi_hat_boo)
  })
  code_boo = unlist(lapply(boo_0, `[[`, "code"))
  concordance_boo = unlist(lapply(boo_0, `[[`, "concordance"))
  pi_hat_boo = do.call(rbind, lapply(boo_0, `[[`, "pi_hat")[code_boo == 0])
  concordance_ci = percentile_ci_fn(baseArgs, concordance_boo)
  pi_ci = data.frame(do.call(rbind, lapply(seq(1, baseArgs$K, by = 1), function(kkk){
    percentile_ci_fn(baseArgs, pi_hat_boo[, kkk])
  })))
  pi_sd = unlist(lapply(seq(1, baseArgs$K, by = 1), function(kkk){
    sd(pi_hat_boo[, kkk])
  }))
  lower = pi_ci$lower
  upper = pi_ci$upper
  pi_in_ci = ifelse(lower <= baseArgs$rSummary & baseArgs$rSummary <= upper, "yes", "no")
  sigma = apply(pi_hat_boo, 2, sd)
  pi_sd_boot = data.frame(sd_boot = sigma, 
                          lower = lower, 
                          upper = upper, 
                          pi_in_ci_boot = pi_in_ci)
  list(pi_sd_boot = pi_sd_boot,
       concordance_ci = concordance_ci)
}