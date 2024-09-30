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
  Class = "LR_Class",
  fields = list(
    model_formula = "formula",    
    data = "data.frame",
    reg_coef = "matrix", 
    fitted_values = "matrix", 
    residual = "matrix",
    degrees_of_freedome = "numeric",
    residual_var = "numeric", 
    var_reg_coef = "numeric",
    p_values = "numeric"
    
    
  ),
  methods = list(
    init = function(model_formula, data) {
      model_formula <<- model_formula
      data <<- data
      .self$LR_fit()
    },
    LR_fit = function(){
      X <- model.matrix(model_formula, data)
      y <- data[[all.vars(model_formula)[1]]]
      reg_coef <<- solve(t(X) %*% X) %*% t(X) %*% y
      fitted_values <<- X %*% reg_coef
      residual <<- y - fitted_values
      degrees_of_freedome <<- nrow(X) - ncol(X)
      residual_var <<- sum(residual^2) / degrees_of_freeedome
      var_reg_coef <<- residual_var * solve(t(X) %*% X)
      t_coef <- reg_coef / sqrt(var_reg_coef)
      p_values <<- 2 * (1 - pt(abs(t_coef), degrees_of_freeedome))
    },
    print = function(){
      print(self$reg_coef)
    },
    summary = function() {
      cat("Call:\n")
      #print(self$model_formula)
      cat("\nCoefficients:\n")
      coef_table <- cbind(Estimate = reg_coef, "Std. Error" = var_reg_coef, "p-value" = p_values)
      #print(coef_table)
      cat("\nResidual standard error:", sqrt(residual_var), "on", degrees_of_freedome, "degrees of freedom\n")
    }
  )
)

data("mtcars")

model <- LR_Class$new(model_formula = mpg ~ wt + hp, data = mtcars)
model$summary()