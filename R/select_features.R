select_features <- function(X, y, options = FALSE) {

  result_lasso <- cv.glmnet(X, y, type.measure = "mse", alpha = 1,
                        family = "binomial", nfolds = 5)
  coefs <- coef(result_lasso, s = "lambda.min")
  sel_features <- rownames(coefs)[which(!(coefs[,1] == 0))[-1]]
  return(sel_features)
}
