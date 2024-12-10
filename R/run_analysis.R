#' Run Full Analysis
#'
#' Run the complete workflow from data preparation to model evaluation and visualization.
#'
#' @param imputed_data Imputed dataset.
#' @param target_year The target year for prediction.
#' @param filter_vars List of variable names to use as predictors.
#' @param model_list List of models to train.
#' @param split_ratio Proportion of data for training. Default is 0.8.
#' @param pca_variance_threshold Variance threshold for PCA. Default is 0.95.
#' @return A list containing evaluation metrics and the result plot.
#' @export
#' @importFrom dplyr select
#' @importFrom caret createDataPartition preProcess train trainControl
#' @importFrom Metrics rmse
#' @importFrom stats cor predict
#' @importFrom ggplot2 ggplot aes geom_point theme_minimal labs
#' @importFrom tidyr pivot_longer
run_analysis <- function(imputed_data, target_year, filter_vars, model_list, split_ratio = 0.8, pca_variance_threshold = 0.95) {
  prepared <- prepare_data(imputed_data, target_year, filter_vars)
  split <- split_data(prepared$X, prepared$Y, split_ratio)
  pca_results <- perform_pca(split$train$X, split$test$X, pca_variance_threshold)
  models <- train_models(pca_results$X_train_pca, split$train$Y, model_list)
  evaluation <- evaluate_models(models, pca_results$X_test_pca, split$test$Y)
  plot <- plot_results(split$test$Y, evaluation$predictions, model_list)

  return(list(evaluation = evaluation$results, plot = plot))
}
