#' Generate a risk data set which is sampled according to a sampling probability dictionary.
#'
#' Obtain a sample that oversamples and undersamples according to categories
#' defined by two normal covariates x_1 and x_2.
#' The categories are A, B, C, D determined by
#' whether or not x_1 and x_2 fall below or above their their respective means.
#'
#' @param NNN The number of people in the original random sample before
#' oversampling and undersampling.
#' @param sampling_probability_dictionary A named vector,
#' eg \code{c(A = 1, B = 0.05, C = 1, D = 0.05))}, with names
#' \code{A}, \code{B}, \code{C}, \code{D},
#' and values equal to the probability that a person in the corresponding
#' category is "kept" in the oversampling.  A probability = 1 directs this
#' function to keep everyone in its corresponding category.
#' @param survival_data A logical equal to TRUE if survival data
#' (e = 0 if censored or dead, e = 1 if disease) rather than competing risk data
#' (e = 2 if dead) is desired.
#' @param t_star A positive real number equal to the right end point of the
#' duration of the study.
#' @param x_digits The number of digits of x_1 and x_2 to round to.
#' @param t_digits THe number of digits of t to round to.
#'
#' @return A data frame with columns \code{x_1}, \code{x_2}, \code{category},
#' \code{sam} (the sampling probability), \code{r_A}, \code{r_B}, \code{t_0},
#' \code{t_1}, \code{ttt}, \code{eee}.
#'
#' @examples
#' NNN = 13
#' cohort_sampling_probability_dictionary = c(A = 1, B = 0.05, C = 1, D = 0.05)
#' target_sampling_probability_dictionary = c(A = 1, B = 1,    C = 1, D = 1)
#' survival_data = FALSE
#' t_star = 10
#' x_digits = 2
#' t_digits = 2
#' cohort_fn(NNN, cohort_sampling_probability_dictionary, survival_data, t_star, x_digits, t_digits)
#'
#' @export
#'
cohort_fn = function(NNN, sampling_probability_dictionary, survival_data, t_star, x_digits, t_digits){
  lambda_0 = lambda_2 = 0.056
  #-------------------------------------------------------------------- lambda_1
  mu_1 = -2.50
  mu_2 = 0.50
  sigma_1 = sqrt(0.640)
  sigma_2 = sqrt(0.562)
  x_1 = rnorm(NNN, mu_1, sigma_1)
  x_2 = rnorm(NNN, mu_2, sigma_2)
  lambda_1 = exp(x_1 + x_2)
  #---------------------------------------------------------- t_0, t_1, t_2, ttt
  t_0 = rexp(NNN, rate = lambda_0)
  t_1 = rexp(NNN, rate = lambda_1)

  ttt = if(survival_data){
    pmin(t_0, t_1)
  } else {
    t_2 = rexp(NNN, rate = lambda_2)
    pmin(t_0, t_1, t_2)
  }
  eee = if(survival_data){
    ifelse(t_1    <= ttt, 1, 0)
  } else {
    ifelse(t_1    <= ttt, 1,
    ifelse(t_2    <= ttt, 2, 0))
  }
  r_A = if(survival_data){
    1 - exp(-exp(x_1 + x_2))
  } else {
    probability_outcome_fn(lambda_1, lambda_2, t_star)
  }
  r_B = if(survival_data){
    1 - exp(-exp(x_1 + 0.5 * x_2))
  } else {
    probability_outcome_fn(exp(x_1 + 0.5 * x_2), lambda_2, t_star)
  }
  z_1 = (x_1 - mu_1)/sigma_1
  z_2 = (x_2 - mu_2)/sigma_2
  category = ifelse(z_1 <  0 & z_2 < 0, "A",
                    ifelse(z_1 <  0 & z_2 >= 0, "B",
                           ifelse(z_1 >= 0 & z_2 < 0, "C", "D")))
  sam = sampling_probability = sampling_probability_dictionary[category]
  keep = rbinom(NNN, 1, prob = sam)
  x_1 = round(x_1, x_digits)
  x_2 = round(x_2, x_digits)
  t_0 = round(t_0, t_digits)
  t_1 = round(t_1, t_digits)
  ttt = round(ttt, t_digits)
  df = data.frame(x_1, x_2, category, sam, r_A, r_B, t_0, t_1, ttt, eee,  stringsAsFactors = FALSE)
  df = df[as.logical(keep), ]
  rownames(df) = 1:nrow(df)
  df
}

