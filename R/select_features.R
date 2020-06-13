select_features <- function(X, y, options = FALSE) {

  result_lasso <- cv.glmnet(X, y, type.measure = "mse", alpha = 1,
                        family = "binomial", nfolds = 5)
  coefs <- coef(result_lasso, s = "lambda.min")
  selected_features <-  rownames(coefs)[which(!(coefs == 0))[-1]]

  return(selected_features)
}
