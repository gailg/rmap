#' \code{riskValidateInternalFn}
#' 
#' The workhorse for grouped \code{rmap}
#' 
#' @param baseArgs A list provided by \code{baseArgsFn}.
#'   The objects required to run \code{riskValidateInternalFn}
#'   are \code{e}, \code{t}, \code{r}
#'   \code{c}, \code{confidence_level}
#'   \code{e}, 
#'   \code{K}, \code{k}, 
#'   \code{N_two_stage}, \code{n_two_stage}, \code{nBootstraps}
#'   \code{rSummary},
#'   \code{t}, \code{tStar}, and
#'   \code{weight}
#'   
#' @return A list containing the elements
#' \itemize{
#'   \item{\code{concordance_summary}: }{
#'   A named vector containing the concordance estimate and the upper
#'   and lower limits of a confidence interval with confidence level
#'   \code{confidence_level}.
#'   }
#'   \item{\code{df_for_roc_plot}: }{
#'   A data.frame containing columns \code{one_minus_specificity} and
#'   \code{sensitivity} that can be used to produce an roc plot.
#'   }
#'   \item{\code{gof}: }{
#'   A named vector containing the Hosmer-Lemeslow goodness of fit
#'   statistic and its p_value for testing if the data fit the risk 
#'   model.
#'   }
#'   \item{\code{pi_summary}: }{A data.frame containing 
#'   \code{K} rows and the columns:
#'   \itemize{
#'   \item{\code{gamma_hat}: }{An estimate of the probability of
#'   falling into each risk group.
#'   }
#'   \item{\code{r}: }{A summary of the assigned risk 
#'   values falling into each risk group
#'   which summary determined by\code{rSummary}.
#'   }
#'   \item{\code{pi_hat}: }{The estimated probability of getting the disease
#'   before the end of the study \code{t_star} in each risk group.
#'   }
#'   \item{\code{sd}: } {The estimated asymptotic theory
#'   standard deviation of \code{pi_hat}.
#'   }
#'   \item{\code{lower}: } {The lower bound of the asymptotic theory
#'   confidence interval.
#'   }
#'   \item{\code{upper}: } {The upper bound of the asymptotic theory
#'   confidence interval.
#'   }
#'   \item{\code{in_ci}: } {A character string \code{"yes"} or \code{"no"}
#'   indicating whether or not summary \code{r} falls in the 
#'   asymptotic theory confidence interval.
#'   }
#'   \item{\code{lowerBoot}: } {The lower bound of the bootstrap percentile
#'   confidence interval.
#'   }
#'   \item{\code{upperBoot}: } {The upper bound of the bootstrap percentile
#'   confidence interval.
#'   }
#'   \item{\code{in_ci}: } {The character string \code{"yes"} or \code{"no"}
#'   indicating whether or not \code{r} falls in the 
#'   bootstrap percentile confidence interval.
#'   }
#'   }
#'   }
#' }
#' @examples 
#' #--------------------------------------------------------------randomSample
#' set.seed(1)
#' xxx = df_randomSample()
#' e = xxx$e
#' t = xxx$t
#' r = xxx$r
#' tStar = 10
#' design = "randomSample"
#' riskGroup = list(K = 2)
#' rSummary = "mean"
#' bootstrap = 20
#' baseArgs = baseArgsFn(e, t, r, tStar, design, riskGroup, rSummary, bootstrap)
#' rv = riskValidateInternalFn(baseArgs)
#' with(rv, list(concordance_summary = concordance_summary,
#'               gof = gof,
#'               pi_summary = pi_summary))
#' #--------------------------------------------------------------------twoStage
#' set.seed(1)
#' twoStageSample = df_twoStage()
#' xxx = twoStageSample$d
#' e = xxx$e
#' t = xxx$t
#' r = xxx$r
#' tStar = 10
#' N = twoStageSample$N
#' design = list(N_two_stage = N, c = xxx$c)
#' riskGroup = list(K = 3)
#' rSummary = "mean"
#' bootstrap = 100
#' baseArgs = baseArgsFn(e, t, r, tStar, design, riskGroup, rSummary, bootstrap)
#' rv = riskValidateInternalFn(baseArgs)
#' with(rv, list(concordance_summary = concordance_summary,
#'               gof = gof,
#'               pi_summary = pi_summary))
#' @export

riskValidateInternalFn = function(baseArgs = FALSE, extraArgs = FALSE) {
  gammaHat = gammaHatFn(baseArgs)
  lambdaHat = lambdaHatFn(baseArgs)
  extraArgs = list(gammaHat = gammaHat,
                   lambdaHat = lambdaHat)
  piHat = piHatFn(baseArgs, extraArgs)
  extraArgs$piHat = piHat
  Sigma = SigmaFn(baseArgs, extraArgs)
  extraArgs$Sigma = Sigma
  sigma = sqrt(sapply(baseArgs$K:(2 * baseArgs$K - 1), function(kkk) Sigma[kkk, kkk]) / sum(baseArgs$N_two_stage))
  piHatCIs = prob_CI_Fn(piHat, sigma)
  lower = piHatCIs[, "lower"]
  upper = piHatCIs[, "upper"]
  in_ci = ifelse(lower <= baseArgs$rSummary & baseArgs$rSummary <= upper, "yes", "no")
  pi_summary = data.frame(round(as.matrix(data.frame(
    gamma_hat = gammaHat,
    r = baseArgs$rSummary,
    pi_hat = piHat,
    sd = sigma,
    lower = lower,
    upper = upper)), 4), in_ci)
  
  ChiSq = ChiSqFn(baseArgs, extraArgs)
  concordance = concordance_fn(baseArgs)
  roc = concordance$roc
  concordance = concordance$concordance
  sensitivity = roc$sensitivity
  specificity = roc$specificity
  one_minus_specificity = 1 - specificity
  roc_df_0 = unique(data.frame(one_minus_specificity, sensitivity))
  df_for_roc_plot = roc_df_0[order(roc_df_0$one_minus_specificity, roc_df_0$sensitivity), ]
  if(FALSE){
    ggplot(df_for_roc_plot, aes(x = one_minus_specificity, y = sensitivity)) +
      geom_step()
  }
  if(baseArgs$nBootstraps == 0){
    rv = list(concordance_summary = data.frame(concordance),
              df_for_roc_plot = df_for_roc_plot,
              gof = ChiSq,
              pi_summary = pi_summary)
  } else {
    boo_0 = lapply(1:baseArgs$nBootstraps, function(booIndex){
      baseArgsBoot = baseArgsBootFn(baseArgs)
      concordance = concordance_fn(baseArgsBoot)$concordance
      tStar - baseArgs$tStar
      KKK = baseArgsBoot$K
      k_boo = baseArgsBoot$k
      t_boo = baseArgsBoot$t
      e_boo = baseArgsBoot$e
      N_in_riskGroup = unlist(lapply(seq(1, KKK, by = 1), function(kkk){
        sum(k_boo == kkk)
      }))
      tau_length = lapply(seq(1, KKK, by = 1), function(kkk){
        t_k = t_boo[k_boo == kkk]
        e_k = e_boo[k_boo == kkk]
        tau = sort(unique(t_k[e_k != 0 & t_k < tStar]))
        length(tau)
      })
      tau_length
      
      # what happens when there are no tau?
      piHat = if(any(N_in_riskGroup == 0) || tau_length == 0){
        "error...at least one riskGroup or tau is empty"
      } else {
        piHatFn(baseArgsBoot)
      }
      list(concordance = concordance,
           piHat = piHat)
    })
    concordance_boo = unlist(lapply(boo_0, `[[`, "concordance"))
    concordance_sd = sd(concordance_boo)
    concordance_ci_normal_theory = prob_CI_Fn(concordance, concordance_sd)
    concordance_ci_percentile = percentile_ci_fn(baseArgs, concordance_boo)
    concordance_summary = data.frame(100 * round(as.matrix(
      data.frame(concordance, lower = concordance_ci_percentile["lower"], upper = concordance_ci_percentile["upper"])
    ), 4))
    rownames(concordance_summary) = NULL
    
    piHat_boo_0 = lapply( boo_0, `[[`, "piHat")
    piHat_boo = do.call(rbind, piHat_boo_0[!unlist(lapply(piHat_boo_0, is.character))])
    piHat_sd = apply(piHat_boo, 2, sd)
    piHat_ci = do.call(rbind, lapply(seq(1, baseArgs$K, by = 1), function(kkk){
      percentile_ci_fn(baseArgs, piHat_boo[, kkk])
    }))
    piHat_ci
    
    pi_summary = cbind(
      pi_summary, 
      lowerBoot = piHat_ci[1:baseArgs$K, 1], 
      upperBoot = piHat_ci[1:baseArgs$K, 2],
      inBootCI = ifelse(piHat_ci[1:baseArgs$K, 1] <= baseArgs$rSummary &
                          baseArgs$rSummary <= piHat_ci[1:baseArgs$K, 2], "yes", "no"))
    rv = list(concordance_summary = unlist(concordance_summary),
              df_for_roc_plot = df_for_roc_plot,
              gof = ChiSq,
              pi_summary = pi_summary)
  }
  rv
}

# Wed Mar 23 16:56:53 PDT 2011

# GG "2015-02-26 12:56:14 PST"