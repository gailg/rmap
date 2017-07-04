#' Calculate the probability of disease during a given time interval
#'
#' Assuming that disease and death are independent and each follow an exponential distribution
#' this function calculates the probability of disease during [0, t_star]
#' given exponential hazards lambda_1 and lambda_2 respectively for disease and death.
#'
#' @param lambda_1 A positive real number reflecting the exponential rate of disease.
#' @param lambda_2 A positive real number reflecting the exponential rate of death.
#' @param t_star A positive number reflecting the duration of the study.
#'
#' @return A positive number equal to the probability of disease during the interval [0, t_star].
#'
#' @examples
#' lambda_1 = 0.057
#' lambda_2 = 0.056
#' t_star = 10
#' probability_outcome_fn(lambda_1, lambda_2, t_star)
#'
#' @export
#'
probability_outcome_fn = function(lambda_1, lambda_2, t_star){
  lambda_1 /(lambda_1 + lambda_2) * (1 - exp(-(lambda_1 + lambda_2)*t_star))
}
# GG "2017-06-26 10:55:11 PDT"
