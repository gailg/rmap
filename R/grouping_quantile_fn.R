grouping_quantile_fn = function(r, N_grouping){
  N = length(r)
  K = N_grouping
  d = data.frame(n = 1:N, r)
  k = cut(1:N, K, labels = FALSE)
  d_reordered = cbind(d[order(r), ], k)
  d_reordered[order(d_reordered$n), ]$k
}
