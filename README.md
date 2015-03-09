# rmap---v-0.01-07---2015-03-08 23:53:06 PDT
  

<A NAME="top"> </A>

trunk/projects/calibration/07-rmap-v-0.01-06/05-new-to-version-0.01-07/12-2015-03-01-rmap-documentation.Rmd

For documentation see http://stanford.edu/~ggong/rmap
For questions or corrections, please email gailgongster@gmail.com

The R package rmap (Risk Model Assessment Package) contains tools for validating personal risk models uaing a random sample of censored longitudinal cohort data. We assume that the user has created a risk model that assigns to each subject at the time she enters the study, a risk, which is the probability that she will suffer a specific adverse outcome within a given time period, say in the next 10 years.

Model calibration (also called goodness-of-fit) measures how well the model-specified risks agree with persons' subsequent observed outcomes.  For overall calibration, rmap offers two chi-square statistics: **the mean-risk statistic** (Hosmer Lemeshow Chi Square statistic)  and the **observed-event-count statistics**.  For grouped calibration, rmap offers **grouped attribute diagrams** and **group-specific standardized residuals** together with their corresponding summary goodness-of-fit statistics. For individualized calibration, rmap offers  **individualized attribute diagrams**.

Discrimination measures how well a model separates positive and negative outcomes.  The ROC and the concordance statistic are the usual measures, which we leave to other authors.  rmap offers **case-risk percentiles** and for comparing two risk models **scatterplots of case-risk percentiles**, as well as estimates and confidence intervals for **predictive-power-positive** and **predictive-power-negative**.





