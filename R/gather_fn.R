gather_fn = function(bootstraps_raw, all_rho) {
  pi_hat_boo = do.call(rbind, lapply(bootstraps_raw, function(one_bootstrap) {
    answer = rep(NA, length(all_rho))
    answer[all_rho %in% one_bootstrap[, "rho"]] = one_bootstrap[, "pi_hat"]
    answer
  }))
  colnames(pi_hat_boo) = paste("rho_", seq_along(all_rho), sep = "")
  rownames(pi_hat_boo) = paste("bs", seq_len(length(bootstraps_raw)), sep = "")
  pi_hat_boo
}