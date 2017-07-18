#' A pi_hat for each distinct assigned risk
#' 
#' An epsilon kernel nearest neighbor estimate of outcome
#' probabilty for each distinct value of assigned risk
#' 
#' @param baseArgs A list provided by \code{baseArgsFn}.
#' The objects required to run \code{pi_hat_nn_fn} are
#' \code{c},
#' \code{e},
#' \code{epsilon},
#' \code{r}
#' \code{sampling},
#' \code{t}
#' \code{verbose}, and
#' \code{weight}.
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
pi_hat_nn_fn = function(baseArgs){
  aaa = baseArgs$weight
  GFn = ecdf_two_stage(baseArgs$r, aaa)
  rho = sort(unique(baseArgs$r))
  NNs = t(sapply(rho, function(rho1) {
    abs(GFn(baseArgs$r) - GFn(rho1)) < baseArgs$epsilon
  }))
  NNs
  ctr = 0
  if(FALSE){
    kkk = 1
  }
  nearest_neighbor_estimates_0 = lapply(seq(from = 1, to = nrow(NNs), by = 1), function(kkk){
    NN1 = NNs[kkk, ]
    ctr <<- ctr + 1
    if(baseArgs$verbose && (ctr %% 100 == 0))
      print(paste("PID: ", Sys.getpid(), " ", date(), " pi_hat_nn_fn: Iteration ",
                  ctr, " of ", nrow(NNs), sep = ""))
    e = baseArgs$e[NN1]
    t = baseArgs$t[NN1]
    r = baseArgs$r[NN1]
    k = rep(1, length(e))
    weight = baseArgs$weight[NN1]
    tStar = baseArgs$tStar
    riskGroup = list(k = k)
    rSummary = "mean"
    bootstrap = FALSE
    design = if(baseArgs$sampling == "weighted"){
      list(w = weight)
    } else {
      N_two_stage = tapply(aaa[NN1], names(aaa[NN1]), sum)
      c = baseArgs$c[NN1]
      design = list(N_two_stage = N_two_stage, c = c)
    }
    baseArgsNN1 = try(baseArgsFn(e, t, r, tStar, design, riskGroup, rSummary, bootstrap))
    "try-error" %in% class(baseArgsNN1) 
    if( "try-error" %in% class(baseArgsNN1) ){
      print(class(baseArgsNN1))
      "try-error"
    } else {
      pi_hat_fn(baseArgsNN1)
    }
  })
  error = any(unlist(lapply(nearest_neighbor_estimates_0, function(this){
    is.character(this) && this == "try-error"
  })))
  if(error){
    "error"
  } else {
    data.frame(rho, pi_hat = unlist(nearest_neighbor_estimates_0))
  }
}