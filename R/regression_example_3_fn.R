regression_example_3_fn = function(e1, Lambda1, Lambda1_den, inside_each_grouping){
  names(inside_each_grouping)
  variable = inside_each_grouping$variable
  grouping = inside_each_grouping$grouping
  grouping_name_uni = unique(sort(grouping))
  work = do.call(rbind,
    lapply(grouping_name_uni, function(grouping_name){
      these = grouping == grouping_name
      N = sum(these)
      x = mean(variable[these])
      U1 = sum(e1[these] - Lambda1[these])
      V1 = sum(Lambda1_den[these]) 
      delta = ifelse(V1 == 0, 0, U1 / sqrt(V1))
      data.frame(N, x, delta)
    }))
  example_3 = with(work, sum(delta^2))
  df = length(grouping_name_uni)
  p3 = pchisq(example_3, df = df, lower.tail = FALSE)
  list(T = data.frame(T = example_3, p = p3), 
       delta = work)
}

