

#' @title LinRegRC: A Reference Class for Multiple Linear Regression. This class provides a reference implementation of multiple linear regression.It contains methods for fitting the model, calculating residuals, and plotting.
#'
#' @field formula A formula object for the model.
#' @field data A data frame used for the regression analysis.
#' @field coefficients A matrix of the estimated coefficients.
#' @field fitted_values A matrix of the predicted values.
#' @field residuals A matrix of the residuals (actual - predicted).
#' @field var_coefficients A matrix of the variance-covariance matrix of the coefficients.
#' @field p_values A numeric vector of p-values of the coefficients.
#' @field df_residual Degrees of freedom for residuals.
#' @field residual_variance A numeric value representing the variance of the residuals.
#' 
#' @return An object of class `LinRegRC`.
#' 
#' @examples
#' # Create a new LinRegRC object
#' model <- LinRegRC$new(Petal.Length ~ Species, data = iris)
#' model$summary()
#'
#' @name LinRegRC
#' @export LinRegRC

library(ggplot2)



# Define the RC class for multiple linear regression
LinRegRC <- setRefClass(
  "LinRegRC",  # Class name
  fields = list(
    data_name = "character",
    formula = "formula",      # Formula object for the model
    data = "data.frame",      # Data used for the regression
    coefficients = "matrix",  # Coefficients (beta estimates)
    fitted_values = "matrix", # Fitted values (predictions)
    residuals = "matrix",     # Residuals (actual - predicted)
    var_coefficients = "matrix",  # Variance-covariance matrix of the coefficients
    p_values = "numeric",     # P-values of the coefficients (numeric, not matrix)
    df_residual = "numeric",  # Degrees of freedom for residuals
    residual_variance = "numeric",  # Estimate of variance of residuals
    t_values = "matrix"
  ),
  
  methods = list(
    # Initialize method (called when a new object is created)
    initialize = function(formula, data) {
      data_name <<- deparse(substitute(data))
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
      t_values <<- coefficients / se_beta
      
      # Calculate p-values based on the t-distribution
      p_values_temp <- 2 * (pt(abs(t_values), df_residual, lower.tail = FALSE))
      
      # Store p-values as numeric
      p_values <<- as.numeric(p_values_temp)
    },
    
    # Method to print the summary of the regression model
    summary = function() {
      # cat("Call:\n")
      # print(formula)
      # cat("\nCoefficients:\n")
      # coef_table <- cbind(Estimate = coefficients, "Std. Error" = sqrt(diag(var_coefficients)), "t-value" = t_values, "p-value" = p_values)
      # print(coef_table)
      # cat("\nResidual standard error:", sqrt(residual_variance), "on", df_residual, "degrees of freedom\n")
      
      # Print the call (formula)
      cat("Call:\n")
      print(formula)
      
      # Create the coefficient table
      # coef_table <- cbind(
      #   "Estimate" = coefficients,
      #   "Std. Error" = sqrt(diag(var_coefficients)),
      #   "t-value" = t_values,
      #   "p-value" = p_values
      # )
      #coef_table <- data.frame(Estimate = coefficients, `Std. Error` =, )
      
      coef_table <- cbind(
        Estimate = as.vector(coefficients),
        `Std. Error` = sqrt(diag(var_coefficients)),
        `t-value` = as.vector(t_values),
        `p-value` = as.vector(p_values)
      )
      
      # Function to add stars for significance levels
      significance_stars <- function(p_value) {
        ifelse(p_value < 0.001, "***",
               ifelse(p_value < 0.01, "**",
                      ifelse(p_value < 0.05, "*", "")
               )
        )
      }
    
      # Add stars for p-values in the table
      signif <- apply(coef_table[, "p-value", drop = FALSE], 1, significance_stars)
      #browser()   
      # Adjust printout format for expected regex
      coef_table_print <- cbind(
        Estimate = sprintf("% .6f", coef_table[, "Estimate"]),
        `Std. Error` = sprintf("% .6f", coef_table[, "Std. Error"]),
        `t-value` = sprintf("% .6f", coef_table[, "t-value"]),
        `p-value` = sprintf("% .6e", coef_table[, "p-value"]),
        ` ` = signif
      )
      
      # Print the coefficient table
      cat("\nCoefficients:\n")
      print(coef_table_print, quote = FALSE, right = TRUE)
      
      # Print residual standard error and degrees of freedom
      cat("\nResidual standard error:", round(sqrt(residual_variance), 6), "on", df_residual, "degrees of freedom\n")
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
    coef = function() {
      # Create a named vector for more precise control over print formatting
      coef_names <- colnames(model.matrix(formula, data))
      coefs <- setNames(as.vector(coefficients), coef_names)
      return(coefs)
    },
    printtt = function() {
      # cat("Call:\n")
      # print(formula)
      # cat(deparse(formula), "\n")  # Convert formula to string, no environment printing
      # cat("Coefficients:\n")
      # print(coef())
      cat(paste0("LinRegRC(formula = ",deparse(formula), ", data = ", data_name, ")")) 
      cat("\n\nCoefficients:\n")
      print(coef())
      #cat(names(coefficients))
      #cat(paste0("\n", coefficients))
    },
    resid = function(){
      return(residuals)
    } ,
    
    # Method to predict new values based on new data
    pred = function(newdata) {
      X_new <- model.matrix(formula, newdata)  # Create design matrix for new data
      return(X_new %*% coefficients)           # Return predicted values
    }
  )
)



data(iris)
model <- LinRegRC$new(formula = Petal.Length ~ Species, data = iris)
 #model$printtt()
# model$resid()
# model$pred()
# model$coef()
 model$summary()
# modt <- lm (formula = Petal.Length ~ Species, data = iris)
# summary(modt)
#print(iris)


