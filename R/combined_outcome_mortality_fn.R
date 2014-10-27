combined_outcome_mortality_fn = function(data, example_fn, output = list(), ...){
  with(data, list(
    combined = part_of_result_fn(
      output, example_fn, e_combined, Lambda_combined, Lambda_combined_den, ...),
    outcome = part_of_result_fn(
      output, example_fn, e_outcome, Lambda_outcome, Lambda_outcome, ...),
    mortality = part_of_result_fn(
      output, example_fn, e_mortality, Lambda_mortality, Lambda_mortality_den, ...)
                  ))
}

part_of_list_fn = function(the_list, the_part = NULL, ...){
  if(is.null(the_part)){
    the_list
  } else {
    part_of_list_fn(the_list[[the_part]], ...)
  }
}

part_of_result_fn = function(the_part, example_fn, ...){
  do.call(part_of_list_fn, c(list(example_fn(...)), the_part))
}
