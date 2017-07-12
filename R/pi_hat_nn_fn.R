#' A pi_hat for each distinct assigned risk
#' 
#' An epsilon kernel nearest neighbor estimate of outcome
#' probabilty for each distinct vluae of assigned risk
#' 
#' @param baseArgs A list provided by \code{baseArgsFn}.
#' The objects required to run \code{pi_hat_nn_fn} are
#' \code{e}, \code{t}, \code{r}, \code{c}, 
#' \code{N_two_stage}, and \code{n_two_stage}
#' 
#' @return A data.frame containing the columns
#' \code{rho} an ordered vector of distinct assigned risks
#' and \code{pi_hat} the corresponding
#' epsilon kernel nearest neighbor
#' estimate of outcome probability
#' 
#' @examples 
#' set.seed(1)
#' xxx = df_randomSample(40)
#' e = xxx$e
#' t = xxx$t
#' r = round(xxx$r, 3)
#' tStar = 10
#' design = "randomSample"
#' epsilon = nrow(xxx)^(-1/3)
#' riskGroup = list(epsilon = epsilon)
#' rSummary = "mean"
#' bootstrap = 20
#' confidenceLevel = 0.95
#' multicore = FALSE
#' verbose = TRUE
#' baseArgs = baseArgsFn(e, t, r, tStar, design, riskGroup, rSummary, bootstrap,
#'                       confidenceLevel, multicore, verbose)
#' e = baseArgs$e
#' t = baseArgs$t
#' tStar = baseArgs$tStar
#' baseArgs$e = ifelse(t > tStar, 0, e)
#' baseArgs$t = ifelse(t > tStar, tStar, t)
#' estimate = pi_hat_nn_fn(baseArgs)
#' estimate
#' ggplot(estimate, aes(x = rho, y = pi_hat)) + geom_line() 
#' @export

pi_hat_nn_fn = function(baseArgs) {
  aaa = (baseArgs$N_two_stage / baseArgs$n_two_stage)[baseArgs$c]
  GFn = ecdf2Stg(baseArgs$r, aaa)
  rho = sort(unique(baseArgs$r))
  NNs = t(sapply(rho, function(rho1) {
    abs(GFn(baseArgs$r) - GFn(rho1)) < baseArgs$epsilon
  }))
  ctr = 0
  NNEs = apply(NNs, 1, function(NN1) {
    ctr <<- ctr + 1
    if(baseArgs$verbose && (ctr %% 100 == 0))
      print(paste("PID: ", Sys.getpid(), " ", date(), " pi_hat_nn_fn: Iteration ",
                  ctr, " of ", nrow(NNs), sep = ""))
    baseArgsNN1 = baseArgsFn(
      e = baseArgs$e[NN1],
      t = baseArgs$t[NN1],
      r = baseArgs$r[NN1],
      tStar = baseArgs$tStar,
      design = list(N_two_stage = tapply(aaa[NN1], names(aaa[NN1]), sum),
                    c = baseArgs$c[NN1]),
      riskGroup = list(K = 1),
      rSummary = "mean",
      bootstrap = FALSE)
    piHatFn(baseArgsNN1)
  })
  data.frame(rho = rho, pi_hat = NNEs)
}