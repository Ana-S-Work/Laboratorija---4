# Define the RC class for multiple linear regression
LinRegRC <- setRefClass(
  "LinRegRC",  # Class name
  fields = list(
    formula = "formula",      # Formula object for the model
    data = "data.frame",      # Data used for the regression
    coefficients = "matrix",  # Coefficients (beta estimates)
    fitted_values = "matrix", # Fitted values (predictions)
    residuals = "matrix",     # Residuals (actual - predicted)
    var_coefficients = "matrix",  # Variance-covariance matrix of the coefficients
    p_values = "numeric",     # P-values of the coefficients (numeric, not matrix)
    df_residual = "numeric",  # Degrees of freedom for residuals
    residual_variance = "numeric"  # Estimate of variance of residuals
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
      
      # Standard errors of the coefficients (square root of diagonal elements)
      se_beta <- sqrt(diag(var_coefficients))
      
      # t-values for the coefficients
      t_values <- coefficients / se_beta
      
      # Calculate p-values based on the t-distribution
      p_values_temp <- 2 * (1 - pt(abs(t_values), df_residual))
      
      # Store p-values as numeric
      p_values <<- as.numeric(p_values_temp)
    },
    
    # Method to print the summary of the regression model
    summary = function() {
      cat("Call:\n")
      print(formula)
      cat("\nCoefficients:\n")
      coef_table <- cbind(Estimate = coefficients, "Std. Error" = sqrt(diag(var_coefficients)), "p-value" = p_values)
      print(coef_table)
      cat("\nResidual standard error:", sqrt(residual_variance), "on", df_residual, "degrees of freedom\n")
    },
    # Method to plot residuals vs fitted and scale-location plot
    plottt = function() {
      # Calculate standardized residuals
      standardized_residuals <- residuals / sqrt(residual_variance)
      
      # Residuals vs Fitted values plot
      residuals_vs_fitted_plot <- ggplot(data = data.frame(
        Fitted = as.vector(fitted_values),
        Residuals = as.vector(residuals)
      ), aes(x = Fitted, y = Residuals)) +
        geom_point(color = "blue") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        labs(title = "Residuals vs Fitted Values", x = "Fitted Values", y = "Residuals") +
        theme_minimal()
      
      # Scale-Location plot (standardized residuals vs fitted values)
      scale_location_plot <- ggplot(data = data.frame(
        Fitted = as.vector(fitted_values),
        Std_Residuals = sqrt(abs(standardized_residuals))
      ), aes(x = Fitted, y = Std_Residuals)) +
        geom_point(color = "blue") +
        geom_smooth(se = FALSE, color = "red", method = "loess") +
        labs(title = "Scale-Location Plot", x = "Fitted Values", y = "Sqrt(|Standardized Residuals|)") +
        theme_minimal()
      
      # Display both plots
      print(residuals_vs_fitted_plot)
      print(scale_location_plot)
    },
    printtt = function() {
      cat("Call:\n")
      print(formula)
      cat("Coefficients:\n")
      # Create a named vector for more precise control over print formatting
      coef_names <- colnames(model.matrix(formula, data))
      coefs <- setNames(as.vector(coefficients), coef_names)
      print(coefs, digits = 2)
    },
    
    # Method to predict new values based on new data
    predict = function(newdata) {
      X_new <- model.matrix(formula, newdata)  # Create design matrix for new data
      return(X_new %*% coefficients)           # Return predicted values
    }
  )
)



data(iris)
model <- LinRegRC$new(formula = Petal.Length ~ Species, data = iris)
model$plottt()

library(ggplot2)



