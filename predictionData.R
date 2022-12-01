#' Create a function which returns a data frame of predicted values along the 
#' scope of two continuous predictors.
#' Have the input be an lm object
#' test1 <- lm(Sepal.Length ~ Petal.Length*Sepal.Width, data = iris)

makePredictionData <- function(lm.obj, nLines = 4){
  # Extract Original Data
  originalData <- lm.obj$model
  
  yName <- names(originalData)[1]
  x1Name <- names(originalData)[2]
  x2Name <- names(originalData)[3]
  
  # Extract Coefficients
  modelCoefs <- lm.obj$coefficients
  ## modelCoefs[1] is intercept
  ## modelCoefs[2] is RoC for x1
  ## modelCoefs[3] is RoC for x2
  ## modelCoefs[4] is Roc for x1*x2
  
  # Create Expanded Data Frame
  x1Vec <- pretty(x = originalData[[x1Name]], n = 20)
  x2Vec <- pretty(x = originalData[[x2Name]], n = nLines)
  
  newData <- expand.grid(newX1 = x1Vec, newX2 = x2Vec)
  names(newData) <- c(x1Name, x2Name)
  
  # Calculate Predicted Values
  prediction <- predict(lm.obj, newdata = newData, type = "response", se.fit = FALSE)
  
  newData[[yName]] <- prediction
  
  # Return
  return(newData)
}

# Example Code ----
# test2 <- makePredictionData(test1)
# 
# ggplot(
#   data = iris,
#   mapping = aes(x = Petal.Length, y = Sepal.Length, color = Sepal.Width)
# ) +
#   geom_jitter() +
#   theme_bw() +
#   geom_smooth(
#     inherit.aes = FALSE,
#     data = test2,
#     mapping = aes(
#       x = Petal.Length,
#       y = Sepal.Length,
#       color = Sepal.Width,
#       group = Sepal.Width
#     ),
#     formula = "y ~ x",
#     method = "lm"
#   )
