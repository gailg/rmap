#' @title A two-stage sample like \code{df_twoStage}
#' but with two risk models
#' 
#' @description 
#' Generate a two-stage sample of people who are subject 
#' to censoring and competing risk but in the data.frame
#' \code{d}, replace the column \code{r} with \code{r1}
#' and add another column called \code{r2} that is 
#' a noisy version of \code{r1}.
#' 
#' 
#' @param NTotal
#' An integer equal to the number of people in the 
#' random sample.
#' 
#' @param distribution
#' A character string equal to \code{lognormal} or
#' \code{beta}.  This is the distribution used to generate
#' \code{eta1} which is the rate of the exponential 
#' that generates the times to disease.  Mean time to 
#' disease is \code{1/eta1}
#' 
#' @param param1
#' A number that determines the first parameter 
#' of the distribution that generates rate of disease.  
#' If \code{distribution = lognormal}, \code{log(t1)} 
#' is normal with mean \code{param1}.  If 
#' \code{distribution = beta}, \code{param1} is the 
#' \code{shape1} parameter of \code{rbeta}.
#' 
#' @param param2
#' A number that determines the second parameter of 
#' the distribution that generates rate of disease.  
#' If \code{distribution = lognormal}, \code{log(t1)} is 
#' normal with sd \code{param2}.  If 
#' \code{distribution = beta}, \code{param2} is the 
#' \code{shape2} parameter of \code{rbeta}.
#' 
#' @param eta0
#' A positive number that determines the rate of censoring.  
#' Times to censoring are generated
#' according to \code{rexp} with \code{rate} parameter 
#' \code{eta0}.
#' 
#' @param eta2
#' A positive number that determines the rate of death.  
#' Times to death are generated according to \code{rexp} with 
#' \code{rate} parameter \code{eta2}.
#' 
#' @param tStar
#' A positve number that determines the length of time of
#' the study.
#' 
#' @param K
#' A positive integer that determines the number of risk groups. 
#' This function assigns the subjects into approximately 
#' equal-sized risk groups. The risk group assignment is 
#' recorded in the column \code{k}, \code{1} for the people 
#' with the smallest risk \code{r}, and so on up to \code{K} 
#' for the people with the largest risk.
#' 
#' @param p
#' A named vector containing the two elements \code{p["A"]} 
#' the resampling probability for category \code{"A"}, and
#' \code{p["B"]} the resampling probability for 
#' category \code{"B"}.
#' 
#' @return A list with three elements
#' \itemize{
#' \item{\code{d}: }{
#' A data.frame with
#' \code{NTotal} rows and with columns
#' \itemize{
#' \item{\code{e}: }{
#' Disease status equal to \code{0} for censored,
#' \code{1} for outcome, and \code{2} for death from 
#' other causes.
#' }
#' \item{\code{t}: }{
#' Time of event \code{e}.
#' }
#' \item{\code{w}: }{
#' The rate of the
#' exponential time to disease.
#' }
#' \item{\code{r1}: }{
#' The probability of disease given the covariate \code{w}.
#' }
#' \item{\code{r2}: }{
#' A noisy version of \code{r1}.
#' \code{r2 = plogis(qlogis(r1) + rnorm(length(r1), mean = 0, sd = 0.5))}
#' }
#' \item{\code{c}: }{
#' A vector of \code{"A"}s.  This vector stores the two-stage
#' category under two-stage sampling.  For random samples, 
#' all subjects are considered to be from the same category 
#' \code{"A"}.
#' }
#' \item{\code{k}: }{
#' A vector of integers representing the risk group assignment 
#' based on the risk vector \code{r} and the number of
#' risk groups \code{K}.
#' }
#' }
#' }
#' \item{\code{N}: }{
#' A named vector of two elements.  \code{N["A"]} and \code{N["B"]}
#' are the number of people in the first stage who fell into
#' categories \code{"A"} and \code{"B"} respectively.
#' }
#' \item{\code{n}: }{
#' A named vector of two elements.  \code{n["A"]} and \code{n["B"]}
#' are the number of people in the second stage who fell into
#' categories \code{"A"} and \code{"B"} respectively.
#' }
#' }
#' @examples 
#' set.seed(1)
#' df_twoStage_r1_r2(20)
#'        
#' @export

df_twoStage_r1_r2 = function(NTotal = 1000, distribution = "lognormal", param1 = -1.8, 
  param2 = 0.4, eta0 = 0.1, eta2 = 0.1, tStar = 10, K = 5, p = c(A = 1, B = 0.5)) {
  ddd = df_twoStage(NTotal = NTotal, distribution = distribution, param1 = param1, 
    param2 = param2, eta0 = eta0, eta2 = eta2, tStar = tStar, K = K, p = p)
  df = ddd$d
  NNN = ddd$N
  nnn = ddd$n
  r1 = df$r
  r2 = plogis(qlogis(r1) + rnorm(length(r1), mean = 0, sd = 0.5))
  df = df[, c("e","t","c")]
  df = cbind(df, data.frame(r1,r2))
  list(d = df, N = NNN, n = nnn)
}

# Wed Mar 16 11:17:18 PDT 2011
