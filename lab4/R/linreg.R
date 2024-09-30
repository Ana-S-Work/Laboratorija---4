# linreg <- function(formula, data){
#   X <- model.matrix(formula, data)
#   y <- data[[all.vars(formula)[1]]]
#   beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
#   y_hat <- X %*% beta_hat
#   resid <- y - y_hat
#   df <- nrow(X) - ncol(X)
#   sigma2 <- sum(resid^2) / df
#   var_beta <- sigma2 * solve(t(X) %*% X)
#   t_coef <- beta_hat / sqrt(var_beta)
#   p_values <- 2 * (1 - pt(abs(t_values), df))
#   res <- list(
#     reg_coef = beta_hat,
#     fitted_values = y_hat,
#     residuals = resid
#     degreees_of_freedome = df
#     residual_var = sigma2
#     var_reg_coeff = var_beta
#   )
#   class(res) <- "linreg"
#   return(res)
# }
# 
# print.linreg() <- function(x){
#   
# }


LR_Class <- setRefClass(
  fields = list(
    formula = "formula",    # Formula object
    data = "data.frame",
    reg_coef = "matrix", # Matrix of estimated coefficients
    fitted_values = "matrix", # Predicted values
    residual = "matrix",
    degreees_of_freedome = "numeric",
    residual_var = "numeric", 
    var_reg_coeff = "numeric",
    p_values = "numeric"
    
    
  ),
  methods = list(
    init = function(formula, data) {
      formula <<- formula
      data <<- data
      .self$LR_fit()
    },
    LR_fit = function(){
      X <- model.matrix(formula, data)
      y <- data[[all.vars(formula)[1]]]
      reg_coef <<- solve(t(X) %*% X) %*% t(X) %*% y
      fitted_values <<- X %*% reg_coef
      residual <<- y - fitted_values
      degrees_of_freeedome <<- nrow(X) - ncol(X)
      residual_var <<- sum(resid^2) / degrees_of_freeedome
      var_beta <- residual_var * solve(t(X) %*% X)
      t_coef <- reg_coef / sqrt(var_beta)
      p_values <- 2 * (1 - pt(abs(t_coef), degrees_of_freeedome))
    }
  )
)