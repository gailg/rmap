regression_example_2_fn = function(e1, Lambda1, Lambda1_den, rrr){
  rBar = mean(rrr)
  weight = exp(abs(rrr - rBar)) / exp(max(rrr) - rBar)
  U1 = sum((e1 - Lambda1) * weight)
  V1 = sum(Lambda1_den * weight^2)
  example_2 = U1^2 / V1
  p2 = pchisq(example_2, df = 1, lower.tail = FALSE)
  list(T = data.frame(T = example_2, p = p2))
}
