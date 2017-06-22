grouping_rounded_fn = function(r, sim_params){
  rMax = sim_params$grouping_rounded_rMax
  grouping = findInterval(r, vec = seq(0, 90, by = 10)/100)
  max_grouping = max(grouping)
  list(
    variable = grouping,
    grouping = grouping) 
}

