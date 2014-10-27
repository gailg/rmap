regression_example_1_fn = function(e1, Lambda1, Lambda1_den){
  U1 = sum(e1 - Lambda1)
  V1 = sum(Lambda1_den)
  example_1 = U1^2 / V1
  p1 = pchisq(example_1 , df = 1, lower.tail = FALSE)
  list(T = data.frame(T = example_1 , p = p1))
}
