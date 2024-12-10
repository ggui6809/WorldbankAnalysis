#' Prepare Data
#'
#' Prepare data for modeling by selecting variables and separating predictors and target.
#'
#' @param data A dataset with variable-year columns.
#' @param target_year The target year for prediction.
#' @param filter_vars List of variable names to use as predictors.
#' @return A list containing predictors (`X`) and target (`Y`).
#' @export
prepare_data <- function(data, target_year, filter_vars = NULL) {
  if (!is.null(filter_vars)) {
    filter_vars <- paste0(filter_vars, "_", target_year)
  }
  target <- paste0("life_expectancy_", target_year)
  selected_vars <- c(target, filter_vars)
  data <- data %>%
    dplyr::select(all_of(selected_vars)) %>%
    dplyr::select(where(is.numeric))

  X <- data %>% dplyr::select(-all_of(target))
  Y <- data[[target]]

  return(list(X = X, Y = Y))
}

#' Split Data into Training and Testing Sets
#'
#' Splits predictors and target into training and testing datasets.
#'
#' @param X A matrix or data frame of predictors.
#' @param Y A vector of target values.
#' @param split_ratio The proportion of the data to use for training. Default is 0.8.
#' @return A list containing training and testing datasets.
#' @export
split_data <- function(X, Y, split_ratio = 0.8) {
  set.seed(123)
  train_index <- caret::createDataPartition(Y, p = split_ratio, list = FALSE)

  train_data <- list(X = X[train_index, , drop = FALSE], Y = Y[train_index])
  test_data <- list(X = X[-train_index, , drop = FALSE], Y = Y[-train_index])

  return(list(train = train_data, test = test_data))
}

#' Perform PCA
#'
#' Applies Principal Component Analysis (PCA) to reduce dimensionality.
#'
#' @param X_train Training data for PCA.
#' @param X_test Testing data for PCA.
#' @param variance_threshold The amount of variance to retain. Default is 0.95.
#' @return A list containing the PCA model, transformed training data, and transformed testing data.
#' @export
perform_pca <- function(X_train, X_test, variance_threshold = 0.95) {
  pca_model <- caret::preProcess(X_train, method = "pca", thresh = variance_threshold)
  X_train_pca <- predict(pca_model, X_train)
  X_test_pca <- predict(pca_model, X_test)

  return(list(pca_model = pca_model, X_train_pca = X_train_pca, X_test_pca = X_test_pca))
}

#' Train Models
#'
#' Trains specified machine learning models on the training data.
#'
#' @param X_train_pca Transformed training data after PCA.
#' @param Y_train Target values for training.
#' @param model_list A vector of model names to train (e.g., "lm", "rf", "svm", "gbm").
#' @return A list of trained models.
#' @export
train_models <- function(X_train_pca, Y_train, model_list = c("lm", "rf", "svm", "gbm")) {
  train_data <- cbind(X_train_pca, life_expectancy = Y_train)
  control <- caret::trainControl(method = "cv", number = 5)  # 5-fold cross-validation
  supported_models <- list(
    lm = "lm",              # Linear Regression
    rf = "rf",              # Random Forest
    svm = "svmRadial",      # Support Vector Machines with Radial Kernel
    gbm = "gbm"             # Gradient Boosting Machine
  )
  selected_models <- supported_models[names(supported_models) %in% model_list]
  models <- lapply(names(selected_models), function(model_name) {
    method <- selected_models[[model_name]]
    if (model_name == "gbm") {
      gbm_grid <- expand.grid(
        n.trees = c(50, 100, 150),  # Number of boosting iterations
        interaction.depth = c(1, 2, 3),  # Maximum depth of trees
        shrinkage = 0.1,  # Learning rate
        n.minobsinnode = 10  # Minimum number of observations in terminal nodes
      )
      caret::train(life_expectancy ~ ., data = train_data, method = method, trControl = control, tuneGrid = gbm_grid, verbose = FALSE)
    } else {
      caret::train(life_expectancy ~ ., data = train_data, method = method, trControl = control)
    }
  })
  names(models) <- names(selected_models)
  return(models)
}

#' Evaluate Models
#'
#' Evaluates model performance using RMSE and R-squared metrics.
#'
#' @param models A list of trained models.
#' @param X_test_pca Transformed testing data after PCA.
#' @param Y_test Actual target values for testing.
#' @return A list containing evaluation metrics and predictions for each model.
#' @export
evaluate_models <- function(models, X_test_pca, Y_test) {
  evaluate_model <- function(model, X_test, Y_test) {
    predictions <- predict(model, newdata = X_test)
    rmse_val <- Metrics::rmse(Y_test, predictions)
    r2_val <- cor(Y_test, predictions)^2
    return(list(predictions = predictions, rmse = rmse_val, r2 = r2_val))
  }

  evaluations <- lapply(models, evaluate_model, X_test = X_test_pca, Y_test = Y_test)

  results <- data.frame(
    Model = names(models),
    RMSE = sapply(evaluations, `[[`, "rmse"),
    R2 = sapply(evaluations, `[[`, "r2")
  )

  predictions <- lapply(evaluations, `[[`, "predictions")

  return(list(results = results, predictions = predictions))
}

#' Plot Results
#'
#' Generates a scatter plot comparing actual and predicted values for each model.
#'
#' @param Y_test Actual target values for testing.
#' @param predictions_list A list of predictions from each model.
#' @param model_list A vector of model names to include in the plot.
#' @return A ggplot object.
#' @export
plot_results <- function(Y_test, predictions_list, model_list) {
  test_results <- data.frame(Actual = Y_test)

  for (model in model_list) {
    if (model %in% names(predictions_list)) {
      test_results[[model]] <- predictions_list[[model]]
    }
  }

  test_results_long <- test_results %>%
    tidyr::pivot_longer(cols = -Actual, names_to = "Model", values_to = "Predicted")

  scatter_plot <- ggplot(test_results_long, aes(x = Actual, y = Predicted, color = Model)) +
    geom_point(alpha = 0.7) +
    theme_minimal() +
    labs(
      title = "Predictions vs Actual Life Expectancy",
      x = "Actual Life Expectancy",
      y = "Predicted Life Expectancy",
      color = "Model"
    )

  return(scatter_plot)
}
