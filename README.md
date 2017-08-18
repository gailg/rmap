# Risk Model Assessment Package---rmap---v-0.03.00
  




"2017-08-18 12:46:13 PDT"

For documentation see http://stanford.edu/~ggong/rmap

For questions or corrections, please email gailgongster@gmail.com

Personal predictive models use an individual's covariates to assign him/her a probability of developing a specific disease or outcome within a specified future time period `[0, t_star]` and before a specified competing risk.  Predictive models are often evaluated by comparing their assigned risks to outcome occurence among participants in a longitudinal cohort study.  Performance of such models can be evaluated using two criteria: calibration and concordance. Model calibration (also called goodness-of-fit) measures how well the model-assigned risks agree
with persons' subsequent observed outcomes. `rmap` offers a grouped goodness-of-fit test and grouped and individualized attribute diagrams. Concordance (also called the area under the ROC curve) measures how well a model separates or discriminates positive and negative outcomes.`rmap` offers estimation and confidence intervals for the concordance as well as ROC plots. `rmap` handles three designs for obtaining samples of participants: (1) random sampling, (2) two-stage sampling, and (3) weighted designs.





