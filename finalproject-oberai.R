# A brief explanation of why you chose this data and the source of the data.
# An explanation of the data set and it's variables.
# A correlation matrix
# Scatter plots for all pairwise variables (not dummy variables).
# A description of what the correlation matrix and scatter plots indicate.
# A description of the initial model to be tested.
# An analysis of the overall fit of the model.
# An analysis of the tests of significance for the coefficients.
# Interpret each coefficient that is not a dummy variable or interaction term.
# An explanation of the value of the coefficient of determination.
# A residual plot for each non-dummy predictor in the model with a description of what is indicated.
# A boxplot of the residuals and analysis.
# A QQ-plot of the residuals and analysis.
# Choose one reasonable combination of your predictor values and calculate and interpret the predictions.
# A summarizing paragraph describing how well the model fits the data.
# A written summary of your analysis submitted in an R Markdown file will be worth 100 points.  


# packages
library(ggplot2)
library(GGally) 
library(qqplotr)



# Tesla data set
# Close.Last is the dependent variable
# Volume, Open, High, and Low are the predictors
# Date is excluded from the Data set as the dates are all from the same month and plays no significance
tesla_stock <- read.csv("Tesla-data.csv")



# excluding the "data" column and removing the "$" symbol from the "close", "Open", "High" and "Low" column
tesla_stock_numeric <- tesla_stock[, !names(tesla_stock) %in% "Date"]
tesla_stock_numeric$Close.Last <- as.numeric(gsub("\\$", "", tesla_stock_numeric$Close.Last))
tesla_stock_numeric$Volume <- as.numeric(gsub("\\$", "", tesla_stock_numeric$Volume))
tesla_stock_numeric$Open <- as.numeric(gsub("\\$", "", tesla_stock_numeric$Open))
tesla_stock_numeric$High <- as.numeric(gsub("\\$", "", tesla_stock_numeric$High))
tesla_stock_numeric$Low <- as.numeric(gsub("\\$", "", tesla_stock_numeric$Low))



# correlation matrix
cor_matrix <- cor(tesla_stock_numeric)
print(cor_matrix)



# scatterplot for all pairwise variables
ggpairs(data = tesla_stock_numeric,
        columnLabels = c("Close/Last", "Volume", "Open", "High", "Low"))



# regression model
regression_model <- lm(data = tesla_stock_numeric, formula = Close.Last ~ Volume + Open + High + Low)
summary(regression_model)
anova(regression_model)



# coefficients
regression_model$coefficients



# adjusted R Squared Value
summary(regression_model)$adj.r.squared




ggplot(tesla_stock_numeric, aes(x = Volume, y = regression_model$residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "blue")

ggplot(tesla_stock_numeric, aes(x = Open, y = regression_model$residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "blue")

ggplot(tesla_stock_numeric, aes(x = High, y = regression_model$residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "blue")

ggplot(tesla_stock_numeric, aes(x = Low, y = regression_model$residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "blue")



# Residual Boxplot
residuals <- data.frame(residual = regression_model$residuals)

ggplot(residuals, aes(x = residual)) +
  geom_boxplot()



# qqplot for the normality of the residuals
ggplot(residuals, aes(sample = residual)) +
  stat_qq_point() +
  stat_qq_line() +
  stat_qq_band()




# predictions

new_data <- data.frame(
  Volume = c(125000000),
  Open = c(175),
  High = c(182),
  Low = c(171)
)

predicted_closing_price <- predict(regression_model, newdata = new_data, interval = "confidence")

predicted_closing_price


