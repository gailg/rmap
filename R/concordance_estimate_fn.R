#' The concordance and the roc plot
#' 
#' Calculate the concordance and produce a data.frame that gives the roc plot
#' 
#' @param baseArgs A list provided by \code{baseArgsFn}.  The objects
#' required by \code{concordance_estimate_fn} are \code{e}, \code{t}, \code{r},
#' \code{weight}, and \code{tStar}.
#' 
#' @return A list that contains
#' \itemize{
#' \item{\code{concordance}: }{A number between 0 and 1 equal to the concordance
#' or the probability that the assigned risk of a randomly selected case exceeds 
#' that of a randomly selected control, if there were no censoring or competing
#' risk. See zzzzz on the website.
#' }
#' \item{\code{df_for_roc_plot}: }{A data.frame containing the columns
#' \code{one_minus_specificity} and \code{sensitivity}, which can be plotted
#' to produce an roc plot.
#' }
#' }
#' @examples 
#' set.seed(1)
#' twoStageSample = df_twoStage(60)
#' tStar = 10
#' xxx = twoStageSample$d
#' e = xxx$e
#' t = round(xxx$t, 1)
#' r = xxx$r
#' N = twoStageSample$N
#' design = list(N_two_stage = N, c = xxx$c)
#' riskGroup = list(K = 2)
#' rSummary = "mean"
#' bootstrap = 100
#' baseArgs = baseArgsFn(e, t, r, tStar, design, riskGroup, rSummary, bootstrap)
#' concordance_estimate = concordance_estimate_fn(baseArgs)
#' concordance_estimate
#' df_for_roc_plot = concordance_estimate$df_for_roc_plot
#' ggplot(df_for_roc_plot, aes(x = one_minus_specificity, y = sensitivity)) + 
#'   geom_step()
#' 
#' @export

concordance_estimate_fn = function(baseArgs){
  concordance = concordance_fn(baseArgs)
  roc = concordance$roc
  concordance = concordance$concordance
  sensitivity = roc$sensitivity
  specificity = roc$specificity
  one_minus_specificity = 1 - specificity
  roc_df_0 = unique(data.frame(one_minus_specificity, sensitivity))
  df_for_roc_plot = roc_df_0[order(roc_df_0$one_minus_specificity, roc_df_0$sensitivity), ]
  list(concordance = concordance,
       df_for_roc_plot = df_for_roc_plot)
}