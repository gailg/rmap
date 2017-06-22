riskValidateInternalFn = function(baseArgs = FALSE, extraArgs = FALSE) {
  gammaHat = gammaHatFn(baseArgs)
  lambdaHat = lambdaHatFn(baseArgs)
  extraArgs = list(gammaHat = gammaHat,
                   lambdaHat = lambdaHat)
  piHat = piHatFn(baseArgs, extraArgs)
  extraArgs$piHat = piHat
  Sigma = SigmaFn(baseArgs, extraArgs)
  extraArgs$Sigma = Sigma
  sigma = sqrt(sapply(baseArgs$K:(2 * baseArgs$K - 1), function(kkk) Sigma[kkk, kkk]) / sum(baseArgs$N))
  piHatCIs = prob_CI_Fn(piHat, sigma)
  lower = piHatCIs[, "lower"]
  upper = piHatCIs[, "upper"]
  in_ci = ifelse(lower <= baseArgs$rSummary & baseArgs$rSummary <= upper, "yes", "no")
  
  piHatSummary = piHatSummaryFn(baseArgs, extraArgs)
  
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
    rv = list(concordance_summary = concordance_summary,
              df_for_roc_plot = df_for_roc_plot,
              gof = ChiSq,
              pi_summary = pi_summary)
  }
  rv
}

# Wed Mar 23 16:56:53 PDT 2011

# GG "2015-02-26 12:56:14 PST"