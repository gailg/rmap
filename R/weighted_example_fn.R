weighted_example_fn = function(NNN){
  x_digits = 2
  t_digits = 2
  t_star = 10
  cohort_sampling_probability_dictionary = 
    cohort_2_sampling_probability_dictionary = c(A = 1, B = 0.05, C = 1,    D = 0.05)
  target_sampling_probability_dictionary = c(A = 1, B = 1,    C = 1,    D = 1)
  survival_data = FALSE
  target_sample = cohort_fn(
    NNN, target_sampling_probability_dictionary, survival_data, t_star, x_digits, t_digits)
  cohort_sample = cohort_fn(
    NNN, cohort_sampling_probability_dictionary, survival_data, t_star, x_digits, t_digits)
  list(
    cohort_sampling_probability_dictionary = cohort_sampling_probability_dictionary,
    cohort_sample = cohort_sample,
    target_sample = target_sample,
    t_star = t_star)
}