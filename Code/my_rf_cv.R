#' Linear Model Function
#'
#' This function fits a linear model.
#'
#' @param k integer representing number of folds.
#' @keywords prediction
#'
#' @return A numeric with the cross validation error.
#'
#' @examples
#' my_rf_cv(2)
#' my_rf_cv(5)
#' my_rf_cv(10)
#'
#' @export
my_rf_cv <- function(k) {
  # remove NAs
  data <- na.omit(my_penguins)
  # assigne a random fold to each observation
  fold <- sample(rep(1:k, length = nrow(data)))
  data$fold <- fold
  # initialize mean square error list
  mse <- list()
  for (i in 1:k) {
    # define indexes of test fold
    inds <- data$fold == i
    # define training data
    training_data <- data[!inds, ]
    # define test data
    test_data <- data[inds, ]
    # train random forest model
    model <- randomForest::randomForest(body_mass_g ~ bill_length_mm + bill_depth_mm + flipper_length_mm, data = training_data, ntree = 100)
    # predict body mass using test fold
    predictions <- predict(model, test_data[, -6])
    # evaluate MSE (mean square error)
    mse[[i]] <- mean((predictions - test_data$body_mass_g) ^2)
  }
  cv_mse <- mean(unlist(mse))
  return(cv_mse)
}
