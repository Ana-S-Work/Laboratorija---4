# Define the RC class for multiple linear regression
LinRegRC <- setRefClass(
  "LinRegRC",  # Class name
  fields = list(
    formula = "formula",      # Formula object for the model
    data = "data.frame",      # Data used for the regression
    coefficients = "matrix",  # Coefficients (beta estimates)
    fitted_values = "matrix", # Fitted values (predictions)
    residuals = "matrix",     # Residuals (actual - predicted)
    var_coefficients = "matrix",      # Standard errors of the coefficients
    p_values = "matrix",     # P-values of the coefficients
    df_residual = "numeric",  # Degrees of freedom for residuals
    residual_variance = "numeric"        # Estimate of variance of residuals
  ),
  
  methods = list(
    # Initialize method (called when a new object is created)
    initialize = function(formula, data) {
      formula <<- formula
      data <<- data
      .self$fit_model()  # Call the fit_model method to fit the model
    },
    
    # Method to fit the linear regression model
    fit_model = function() {
      X <- model.matrix(formula, data)    # Design matrix X (independent variables)
      y <- data[[all.vars(formula)[1]]]   # Response vector y (dependent variable)
      
      # Calculate coefficients using the normal equation: (X'X)^(-1) X'y
      coefficients <<- solve(t(X) %*% X) %*% t(X) %*% y
      
      # Predicted values (fitted values)
      fitted_values <<- X %*% coefficients
      
      # Residuals (difference between actual and predicted values)
      residuals <<- y - fitted_values
      
      # Degrees of freedom
      df_residual <<- nrow(X) - ncol(X)
      
      # Estimate of variance 
      residual_variance <<- sum(residuals^2) / df_residual
      
      # Variance-covariance matrix of the coefficients
      var_coefficients <<- residual_variance * solve(t(X) %*% X)
      
      
      # t-values for the coefficients
      se_beta <- sqrt(diag(var_coefficients))
      t_values <- coefficients / se_beta
      
      # Calculate p-values based on the t-distribution
      p_values <<- 2 * (1 - pt(abs(t_values), df_residual))
    },
    
    # Method to print the summary of the regression model
    summary = function() {
      cat("Call:\n")
      print(formula)
      cat("\nCoefficients:\n")
      coef_table <- cbind(Estimate = coefficients, "var beta" = var_coefficients, "p-value" = p_values)
      print(coef_table)
      cat("\nResidual standard error:", sqrt(residual_variance), "on", df_residual, "degrees of freedom\n")
    },
    
    # Method to predict new values based on new data
    predict = function(newdata) {
      X_new <- model.matrix(formula, newdata)  # Create design matrix for new data
      return(X_new %*% coefficients)           # Return predicted values
    },
    
    print = function(){
      
    }
  )
)

# Load the mtcars dataset
data(mtcars)

# Create an instance of the LinRegRC class
model <- LinRegRC$new(formula = mpg ~ wt + hp, data = mtcars)

# Display the summary of the model
model$summary()

