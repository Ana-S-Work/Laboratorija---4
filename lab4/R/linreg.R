linreg <- function(formula, data){
  X <- model.matrix(formula, data)
  y <- data[[all.vars(formula)[1]]]
  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
  y_hat <- X %*% beta_hat
  resid <- y - y_hat
  df <- nrow(X) - ncol(X)
  sigma2 <- sum(resid^2) / df
  var_beta <- sigma2 * solve(t(X) %*% X)
  t_coef <- beta_hat / sqrt(var_beta)
  p_values <- 2 * (1 - pt(abs(t_values), df))
  res <- list(
    reg_coef = beta_hat,
    fitted_values = y_hat,
    residuals = residuals()
    degreees_of_freedome = df
    residual_var = sigma2
    var_reg_coeff = var_beta
  )
  class(res) <- "mult_linreg"
  return(res)
}