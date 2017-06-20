weighted_rmap_fn = function(baseArgs){
  concordance = concordance_fn(baseArgs)
  roc = concordance$roc
  concordance = concordance$concordance
  sensitivity = roc$sensitivity
  specificity = roc$specificity
  one_minus_specificity = 1 - specificity
  roc_df_0 = unique(data.frame(one_minus_specificity, sensitivity))
  df_for_roc_plot = roc_df_0[order(roc_df_0$one_minus_specificity, roc_df_0$sensitivity), ]
  gamma_hat = unlist(lapply(seq(1, baseArgs$K, by = 1), function(kkk){
    sum(baseArgs$weight[baseArgs$k == kkk])
  })) / sum(baseArgs$weight)
  pi_hat = pi_hat_fn(baseArgs)
  r_bar = baseArgs$rSummary
  randomSeeds = sample(1:1e8, baseArgs$N_bootstraps, replace = FALSE)
  boo_0 = lapply(1:baseArgs$N_bootstraps, function(n_bootstrap){
    seed = randomSeeds[n_bootstrap]
    base_args_boot = base_args_boot_fn(baseArgs, seed)
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
  alpha = 1 - baseArgs$confidence_level
  percentile_ci_fn = function(concordance_boo){
    ci = quantile(concordance_boo, c(alpha/2, 1 - alpha/2))
    names(ci) = c("lower", "upper")
    ci
  }
  concordance_ci = percentile_ci_fn(concordance_boo)
  concordance_ci["lower"]
  pi_ci = data.frame(do.call(rbind, lapply(seq(1, baseArgs$K, by = 1), function(kkk){
    percentile_ci_fn(pi_hat_boo[, kkk])
  })))
  pi_sd = unlist(lapply(seq(1, baseArgs$K, by = 1), function(kkk){
    sd(pi_hat_boo[, kkk])
  }))
  pi_in_ci = unlist(lapply(seq(1, baseArgs$K, by = 1), function(kkk){
    if(pi_ci[kkk, ]$lower <= pi_hat[kkk] & pi_hat[kkk] <= pi_ci[kkk, ]$upper){
      "yes"
    } else {
      "no"
    }
  }))
  sigma_boo = apply(pi_hat_boo, 2, sd)
  gof_statistic = sum(gamma_hat * (pi_hat - r_bar)^2 / sigma_boo^2)
  p_value = davies(gof_statistic, lambda = gamma_hat)$Qq
  gof = data.frame(gof_statistic = gof_statistic, p_value = p_value)
  concordance_summary = data.frame(100 * round(as.matrix(
    data.frame(concordance, lower = concordance_ci["lower"], upper = concordance_ci["upper"])
  ), 4))
  row.names(concordance_summary) = NULL
  pi_summary = cbind(
    round(as.matrix(cbind(data.frame(gamma_hat, r = r_bar, pi_hat, sd = pi_sd), pi_ci)), 4), 
    data.frame(in_ci = pi_in_ci))
  
  list(
    concordance_summary = concordance_summary,
    df_for_roc_plot = df_for_roc_plot,
    gof = gof,
    pi_summary = pi_summary)
}