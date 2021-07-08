#----------------------------------------------------#
# PLOT_EVAL
#----------------------------------------------------#
#' Plot one or more prediction evaluations
#'
#' @param model_fit Fitted model object from the `gbm_fit`
#' @param test_data Out-of-sample observations to be used as evaluation data
#' @param eval Type of evaluation to be performed. Should be one of: 'pai', 'pei', 'rri'. Defaults to 'pai'.
#' @param cutoff_lower Lower limit of cutoff value to determine a hotspot. Defaults to 0.001.
#' @param cutoff_upper Upper limit of cutoff value to determine a hotspot. Defaults to 0.01.
#' @param cutoff_range Range of values to evaluate against. Defaults to 100.
#'
#'
#'@export

plot_eval <- function(model_fit,
                      test_data,
                      eval = "pai",
                      cutoff_lower = 0.001,
                      cutoff_upper = 0.01,
                      cutoff_range = 100){
  
  # Initialize list, counter, and range of values
  alist <- list()
  eval_range <- seq(from = cutoff_lower, cutoff_upper, length.out = cutoff_range)
  j = 0
  
  # Evaluate function at range of values
  # Fill list j times
  for (i in  eval_range) {
    j <- j + 1
    alist[[j]] <- outcome_eval(
      model_fit = fit1,
      test_data = test_data,
      eval = eval,
      cutoff = i
    )

  }
  
  # Save output as a dataframe
  # compatible for ggplot
  plot_dataframe <- data.frame(eval = unlist(alist),
                               cutoff = eval_range)
  
  # Export plot
  ggplot(plot_dataframe) +
    geom_line(aes(x = cutoff, y = eval), size = 1, color = "darkred") +
    labs(y = toupper(eval), x = "Proportion of Region") +
    theme_bw() +
    theme(axis.text = element_text(size = 12),
          axis.title = element_text(size = 12))
}
