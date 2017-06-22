grouping_fn = function(rrr, user_grouping){
  variable = if("variable" %in% names(user_grouping)){
    user_grouping$variable
  } else {
    rrr
  }
  grouping = if("k" %in% names(user_grouping)){
    user_grouping$k
  } else if("K" %in% names(user_grouping)){
    grouping_quantile_fn(variable, user_grouping$K)
  } else if("cutoffs" %in% names(user_grouping)){
    as.numeric(cut(variable,
                   breaks = user_grouping$cutoffs,
                   include.lowest = TRUE))
  }
  list(variable = variable, grouping = grouping)
}
