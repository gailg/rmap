#' @title A translator to go from \code{hadley_spelling} to \code{camelCase}
#' 
#' @description I started a long time ago adopting camel case, but 
#' I now see the error of my ways and can no longer stomach the use
#' of camel case
#' 
#' @param risk_group All arguments are the same as for \code{rmap} 
#' except this one which can also include 
#' \code{risk_group = list(epsilon = epsilon)}
#' 
#' @return The same list as that described in \code{baseArgsFn}.
#' 
#' @examples
#' #--------------------------------------------------- random sample
#' data(random_sample_example)
#' xxx = random_sample_example
#' head(xxx)
#' e = xxx$e
#' t = xxx$t
#' r = xxx$r
#' t_star = 10
#' design = "random_sample"
#' risk_group = list(K = 4)
#' r_summary = "mean"
#' N_bootstraps = 100
#' set.seed(1)
#' baseArgs = base_args_fn(e, t, r, t_star, design, risk_group, r_summary, N_bootstraps)
#' names(baseArgs)
#' 
#' @export
base_args_fn = function(e, t, r, t_star, design, risk_group, r_summary, N_bootstraps, 
                confidence_level = 0.95, N_cores = 0, verbose = FALSE){
  tStar = t_star
  riskGroup = risk_group
  rSummary = r_summary
  bootstrap = if(N_bootstraps == 0){
    FALSE
  } else {
    N_bootstraps
  }
  confidenceLevel = confidence_level
  multicore = if(N_cores > 1){
    TRUE
  } else {
    FALSE
  }
  design = if(is.character(design) && design == "random_sample"){
    "randomSample"
  } else if("N_first_stage" %in% names(design) && "category" %in% names(design)){
    list(c = design$category,
         N_two_stage = design$N_first_stage)
  } else if("N_target" %in% names(design) && "category" %in% names(design)){
    N_target = design$N_target
    target_category = unlist(lapply(names(N_target), function(nombre){
      repeats = N_target[nombre]
      rep(nombre, repeats)
    }))
    list(c = design$category,
         target_category = target_category)
  } else if ("target_category" %in% names(design) && "category" %in% names(design)){
    list(c = design$category,
         target_category = design$target_category)
  } else {
    stop(paste("Design must be 'randomSample' or a list containing 'category'",
               "and one of 'N_first_stage', 'N_target', or 'target_category'"))
  }
  baseArgs = baseArgsFn(e, t, r, tStar, design, riskGroup, rSummary, bootstrap, 
                        confidenceLevel, multicore, verbose)
  baseArgs
}