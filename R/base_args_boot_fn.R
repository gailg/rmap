#' A bootstrap sample version of \code{baseArgsFn}
#' 
#' Obtain a bootstrap sample from \code{baseArgs} and
#' create the objects needed to run \code{concordance_fn}
#' and \code{pi_hat_fn}
#' 
#' @param baseArgs A list provided by \code{baseArgsFn}.
#' Objects requred by \code{base_args_boot_fn} are
#' \code{c},
#' \code{e},
#' \code{epsilon},
#' \code{K},
#' \code{k},
#' \code{N_two_stage},
#' \code{n_two_stage},
#' \code{r},
#' \code{sampling},
#' \code{t},
#' \code{target_category},
#' \code{tStar}
#' 
#' @return A list containing 
#' \itemize{
#' \item{\code{boot_code}: }{An integer equal to 0
#' provided there are no errors because of empty cohort
#' categories or empty risk groups.
#' }
#' \item{\code{boot_message}: }{A character string 
#' that tries to explain the source of the error.
#' Not used.
#' }
#' \item{\code{c}: }{
#' A bootstrap version of \code{baseArgs$c}
#' }
#' \item{\code{e}: }{
#' A bootstrap version of \code{baseArgs$e}
#' }
#' \item{\code{epsilon}: }{
#' Equal to \code{baseArgs$epsilon}
#' }
#' \item{\code{K}: }{
#' Equal to \code{baseArgs$K}
#' }
#' \item{\code{k}: }{
#' A bootstrap version of \code{baseArgs$k}
#' }
#' \item{\code{N_nonzero_events}: }{An integer vector of 
#' length \code{K} counting the number events in each
#' risk group that occurred before \code{tStar}
#' and were not censored.
#' }
#' \item{\code{r}: }{
#' A bootstrap version of \code{baseArgs$r}
#' }
#' \item{\code{sampling}: }{
#' Equal to \code{baseArgs$sampling}
#' }
#' \item{\code{t}: }{
#' A bootstrap version of \code{baseArgs$t}
#' }
#' \item{\code{tStar}: }{
#' Equal to \code{baseArgs$tStar}
#' }
#' \item{\code{verbose}: }{
#' Equal to \code{baseArgs$verbose}
#' }
#' \item{\code{weight}: }{
#' A bootstrap version of \code{baseArgs$weight},
#' gotten by samplng first from the full population 
#' using \code{N_two_stage} or the target population 
#' (using \code{target_category}), then sampling to obtain 
#' the second stage sample or cohort sample,
#' then calculating the appropriate weights.
#' }
#' }
#' @examples 
#' #-------------------------------------------------- A weighted example
#' set.seed(1)
#' # I tried to choose a small number producing no errors from baseArgsFn
#' NNN = 48 
#' N_bootstrap_reps = 3
#' cutoffs = c(0, 0.20, 1)
#' weighted_example = weighted_example_fn(NNN)
#' cohort_sampling_probability_dictionary = 
#'      weighted_example$cohort_sampling_probability_dictionary
#' cohort_sample = weighted_example$cohort_sample
#' target_sample = weighted_example$target_sample
#' tStar = weighted_example$t_star
#' which_model = "r_B" 
#' cohort_category = cohort_sample$category
#' target_category = target_sample$category
#' r = round(cohort_sample[[which_model]], 2)
#' e = cohort_sample$eee
#' t = cohort_sample$ttt
#' cohort_sample
#' design = list(targetCategory = target_category, c = cohort_category)
#' epsilon = nrow(cohort_sample)^(-1/3)
#' riskGroup = list(epsilon = epsilon)
#' rSummary = "mean"
#' bootstrap = N_bootstrap_reps
#' baseArgs = 
#'   baseArgsFn(e, t, r, tStar, design, riskGroup, rSummary, bootstrap) 
#' baseArgs
#' # The pi_hat part of rmap requires times truncated at tStar
#' e = baseArgs$e
#' t = baseArgs$t
#' tStar = baseArgs$tStar
#' baseArgs$e = ifelse(t > tStar, 0, e)
#' baseArgs$t = ifelse(t > tStar, tStar, t)
#' base_args_boot = base_args_boot_fn(baseArgs)
#' base_args_boot
#' # The next part shows how I use base_args_boot inside pi_hat_nn_fn
#' aaa = base_args_boot$weight
#' GFn = ecdf_two_stage(base_args_boot$r, aaa)
#' rho = sort(unique(base_args_boot$r))
#' NNs = t(sapply(rho, function(rho1) {
#'   abs(GFn(base_args_boot$r) - GFn(rho1)) < base_args_boot$epsilon
#' }))
#' look = ifelse(NNs, 1, 0)
#' dimnames(look) = list(1:nrow(NNs),1:ncol(NNs))
#' # look(NNs) ia a matrix with J = (number of unique r) rows 
#' # and N columns
#' # The j-th row of look(NNs) shows the neighborhood for the tau[j]
#' # A 1(TRUE) in the n-th column says the n-th subject lands in
#' # this neighborhood
#' look
#' # Examine the jjj-th neighborhood jjj = 1
#' jjj = 1
#' NN1 = NNs[jjj, ]
#' NN1 
#' e = base_args_boot$e[NN1]
#' t = base_args_boot$t[NN1]
#' r = base_args_boot$r[NN1]
#' k = rep(1, length(e))
#' weight = base_args_boot$weight[NN1]
#' # The data associated with the jjj-th neighborhood
#' data.frame(e, t, r, k, weight)
#' tStar = base_args_boot$tStar
#' riskGroup = list(k = k)
#' rSummary = "mean"
#' bootstrap = FALSE
#' design = if(base_args_boot$sampling == "weighted"){
#'   list(w = weight)
#' } else {
#'   N_two_stage = tapply(aaa[NN1], names(aaa[NN1]), sum)
#'   c = base_args_boot$c[NN1]
#'   design = list(N_two_stage = N_two_stage, c = c)
#' }
#' design
#' # This call to baseArgsFn gets the data ready for a call to pi_hat_fn
#' base_args_bootNN1 = 
#'   baseArgsFn(e, t, r, tStar, design, riskGroup, rSummary, bootstrap)
#' base_args_bootNN1
#' pi_hat_fn(base_args_bootNN1)
#' 
#' #------------------------------------------- A two-stage sample example
#' options(digits = 5)
#' set.seed(9)
#' NNN = 31
#' twoStageSample = df_twoStage(NNN)
#' xxx = twoStageSample$d
#' row.names(xxx) = NULL
#' nrow(xxx)
#' xxx
#' e = xxx$e
#' t = xxx$t
#' r = round(xxx$r, 2)
#' N = twoStageSample$N
#' design = list(N_two_stage = N, c = xxx$c)
#' epsilon = nrow(xxx)^(-1/3)
#' riskGroup = list(epsilon = epsilon)
#' rSummary = "mean"
#' bootstrap = 100
#' set.seed(1)
#' baseArgs = 
#'   baseArgsFn(e, t, r, tStar, design, riskGroup, rSummary, bootstrap)
#' list(design = design,
#'      baseArgs_N_two_stage = baseArgs$N_two_stage, 
#'      baseArgs_n_two_stage = baseArgs$n_two_stage,
#'      N_two_stage_over_n_two_stage = 
#'        baseArgs$N_two_stage/baseArgs$n_two_stage,
#'      baseArgs_weight = baseArgs$weight)
#' e = baseArgs$e
#' t = baseArgs$t
#' tStar = baseArgs$tStar
#' baseArgs$e = ifelse(t > tStar, 0, e)
#' baseArgs$t = ifelse(t > tStar, tStar, t)
#' base_args_boot = base_args_boot_fn(baseArgs)
#' base_args_boot
#' # And this is an example of how I use the base_args_boot
#' baseArgs$design
#' aaa = base_args_boot$weight
#' GFn = ecdf_two_stage(base_args_boot$r, aaa)
#' rho = sort(unique(base_args_boot$r))
#' NNs = t(sapply(rho, function(rho1) {
#'   abs(GFn(base_args_boot$r) - GFn(rho1)) < base_args_boot$epsilon
#' }))
#' look = ifelse(NNs, 1, 0)
#' dimnames(look) = list(1:nrow(NNs),1:ncol(NNs))
#' # look(NNs) ia a matrix with J = (number of unique r) rows 
#' # and N columns
#' # The j-th row of look(NNs) shows the neighborhood for the tau[j]
#' # A 1(TRUE) in the n-th column says the n-th subject lands in 
#' # this neighborhood
#' look
#' # Examine the jjj-th neighborhood jjj = 1
#' jjj = 1
#' NN1 = NNs[jjj, ]
#' NN1 
#' e = base_args_boot$e[NN1]
#' t = base_args_boot$t[NN1]
#' r = base_args_boot$r[NN1]
#' k = rep(1, length(e))
#' weight = base_args_boot$weight[NN1]
#' # The data associated with the jjj-th neighborhood
#' data.frame(e, t, r, k, weight)
#' tStar = base_args_boot$tStar
#' riskGroup = list(k = k)
#' rSummary = "mean"
#' bootstrap = FALSE
#' design = if(base_args_boot$sampling == "weighted"){
#'   list(w = weight)
#' } else {
#'   N_two_stage = tapply(aaa[NN1], names(aaa[NN1]), sum)
#'   c = base_args_boot$c[NN1]
#'   design = list(N_two_stage = N_two_stage, c = c)
#' }
#' design
#' table(design$c)
#' # This call to baseArgsFn gets the data ready for a call to pi_hat_fn
#' base_args_bootNN1 = 
#'   baseArgsFn(e, t, r, tStar, design, riskGroup, rSummary, bootstrap)
#' base_args_bootNN1
#' pi_hat_fn(base_args_bootNN1)
#' 
#' @export
base_args_boot_fn = function(baseArgs) {
  if(baseArgs$sampling == "weighted"){
    N_c = length(baseArgs$c)
    N_p = length(baseArgs$target_category)
    
    bootstrap_sample = sample(1:N_c, N_c, replace = TRUE)
    target_bootstrap_sample = sample(1:N_p, N_p, replace = TRUE)
    e_boo = baseArgs$e[bootstrap_sample] # need_outside_if
    t_boo = baseArgs$t[bootstrap_sample] # need_outside_if
    r_boo = baseArgs$r[bootstrap_sample] # need_outside_if
    c_boo = baseArgs$c[bootstrap_sample] # need_outside_if
    k_boo = baseArgs$k[bootstrap_sample] # need_outside_if
    target_category_boo = baseArgs$target_category[target_bootstrap_sample]
    weight_0 = weight_fn(c_boo, target_category_boo)
    weight_code = weight_0$code                          # need_outside_if
    weight_message = weight_0$message                    # need_outside_if
    category_weights_boo = weight_0$category_weights
    weight_boo = category_weights_boo[c_boo]             # need_outside_if
  } else {
    indices = suppressWarnings({
      sample(1:sum(baseArgs$n_two_stage), sum(baseArgs$N_two_stage), replace = TRUE,
             prob = (baseArgs$N_two_stage / baseArgs$n_two_stage )[baseArgs$c])
    })
    keep = rbinom(sum(baseArgs$N_two_stage), 
                  1, 
                  (baseArgs$n_two_stage / baseArgs$N_two_stage )[baseArgs$c[indices]])
    indicesTwoStage = indices[as.logical(keep)]
    e_boo = baseArgs$e[indicesTwoStage] # need_outside_if
    t_boo = baseArgs$t[indicesTwoStage] # need_outside_if
    r_boo = baseArgs$r[indicesTwoStage] # need_outside_if
    c_boo = baseArgs$c[indicesTwoStage] # need_outside_if
    k_boo = baseArgs$k[indicesTwoStage] # need_outside_if                     
    c_uni = names(baseArgs$N_two_stage)
    n_two_stage_boo = structure(sapply(c_uni, function(c1){
      sum(c_boo == c1)
    }), .Names = c_uni)
    N_two_stage_boo = structure(sapply(c_uni, function(c1){
      sum(baseArgs$c[indices] == c1)
    }), .Names = c_uni)
    aaa = N_two_stage_boo / n_two_stage_boo
    weight_boo = aaa[c_boo]                            # need_outside_if
    weight_code = 0                                    # need_outside_if
    weight_message = "ok"                              # need_outside_if
  }
  N_in_risk_group = unlist(lapply(seq(1, baseArgs$K, by = 1), function(kkk){
    sum(k_boo == kkk)
  }))
  N_in_risk_group_message = paste(
    "risk groups",
    paste(which(N_in_risk_group == 0), collapse = ", "),
    "are empty")
  N_nonzero_events = unlist(lapply(seq(1, baseArgs$K, by = 1), function(kkk){
    e_inside_pi_hat = ifelse(t_boo[k_boo == kkk] > tStar, 0, e_boo[k_boo == kkk])
    sum(e_inside_pi_hat > 0)
  }))
  boot_code = if( weight_code == 0 && all(N_in_risk_group > 0) ) {
    0
  } else {
    1
  }
  boot_message = if( weight_code == 0 ){
    "All systems go"
  } else {
    "Something wrong with weight or N_in_risk_group"
  }
  base_args_boot = list(
    boot_code = boot_code,
    boot_message = boot_message,
    c = c_boo,
    e = e_boo,
    epsilon = baseArgs$epsilon,
    K = baseArgs$K,
    k = k_boo,
    N_nonzero_events = N_nonzero_events,
    r = r_boo,
    sampling = baseArgs$sampling,
    t = t_boo,
    tStar = baseArgs$tStar,
    verbose = baseArgs$verbose,
    weight = weight_boo)
  base_args_boot
}