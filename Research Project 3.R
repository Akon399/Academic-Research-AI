# Research Project 3.
# Submission Date : 7 August 2023.
# Solution by Akhona Njeje.



# Question 1.


# 1.1. read.table() is a function that reads tabular data in R, in figure 1 we use it to read the stack_data csv.

# 1.2. plot() is a function we use for plotting in R, in figure 1 we using it to plot Age in x-axis vs hipcenter in y-axis.

# 1.3. lm() is a function we use to create a liner regression model in R, in figure 1 we using it to create a model where y = stack.loss.

# 1.4. summarry() is a function that gives us numerical info of a dataset, the info is minimum, maximum, median, 1st & 3rd Quartiles.


# Question 1.


# 1.2. The code in be will produce a graph where the independent variable is Age & the dependent variable is hipcneter.

# 1.3. The correct command is install.packages(), we use this function to install packages like ggplot, ggthemes, shiny etc.


# Question 2.


# 2.1. The USArrests its found in the base R package, meaning it comes pre-installed & we use the data() function to call it.

data("USArrests")

# 2.2. Contains info about arrests for different crimes in 50 states of USA.
#      The variables observed in the dataset are Murder, Assuslt, Urban & Rape, its 4 numeric columns.

head(USArrests)

# Question 2.3:
# i) Urban population percentage & Murder rate:

library(ggplot2)

ggplot(USArrests, aes(x = UrbanPop, y = Murder)) + geom_point() + 
  labs(x = "Urban population", y = "Murder Rate") 
# The relationship is not strongly correlated, between 60 & 70 we have the highest muder rate,
# As the population increases from 30 to 60 we see a increase in murder rate.
# As the population increases futher from 70 to 90 we see slight decrease in murder rate.


# Question 2.3:
# ii) Linear Regression.

model = lm(Murder~UrbanPop, data = USArrests)

ggplot(USArrests, aes(x = UrbanPop, y = Murder)) + geom_point() + 
  labs(x = "Urban population", y = "Murder Rate") + geom_smooth(method = "lm", se = FALSE)

print(summary(model))
# Looking at our model our slope = m = 0.02093 & our constant = c = 6.41594.


# Question 2.3:
# iii) Hypothesis Testing, significance = 5%.

# H_0 : B_1 = 0 vs H_1 : B_1 !=0.

# t = 0.483.
# t_(n-2) = t_(48) = 2.0105.

# t < t_48, then we accept H_0 at 5% signicance level.


# Question 2.3:
# iv) Diagnostic plots.

library(ggplot2)
model = lm(Murder~UrbanPop, data = USArrests)

# Residuals vs Fitted Plot
plot1 = ggplot(model, aes(x = fitted(model), y = residuals(model))) +
  geom_point() + geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ggtitle("Residuals vs Fitted") + xlab("Fitted Values") + ylab("Residuals")
plot1
# Looking at Plot1 the residuals are scattered around the zero line

# Normal Q-Q Plot
plot2 = ggplot(model, aes(sample = residuals(model))) + geom_qq() + geom_qq_line() +
  ggtitle("Normal Q-Q Plot") + xlab("Theoretical Quantiles") + ylab("Sample Quantiles")
plot2
# The normality in Plot2 is met since our residuals follow the line, especially from -5 to 10.

# Scale-Location Plot (Square root of standardized residuals vs. Fitted values)
plot3 = ggplot(model, aes(x = fitted(model), y = sqrt(abs(residuals(model)/cooks.distance(model))))) +
  geom_point() + geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  ggtitle("Scale-Location Plot") + xlab("Fitted Values") + ylab("Square root of |Standardized Residuals|")
plot3
# Our residuals in Plot3 are evenly spread around y = 1.

# Residuals vs Leverage Plot
plot4 = ggplot(model, aes(x = hatvalues(model), y = residuals(model))) +
  geom_point() + geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ggtitle("Residuals vs Leverage") + xlab("Leverage") + ylab("Residuals")
plot4
# Combine plots

library(gridExtra)

combined_plots = grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)

# Display the combined plots

print(combined_plots)
# The models assumptions were fairly met.


# Question 3.

install.packages("MASS")
library(MASS)
data("Pima.tr")
str(Pima.tr)
head(Pima.tr)

# 3.1. Are the assumptions of homogeneous variance & normality of residuals violated?

pima_model = lm(bp ~ npreg+glu+skin+bmi+ped+age, data = Pima.tr)
print(summary(pima_model))

# Residuals vs Fitted Plot to check for homoscedasticity

residuals_df = data.frame(Fitted = fitted(pima_model), Residuals = residuals(pima_model))

# Scatter plot of residuals vs fitted values

plot_homoscedasticity = ggplot(residuals_df, aes(x = Fitted, y = Residuals)) + geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ggtitle("Residuals vs Fitted") + xlab("Fitted Values") + ylab("Residuals")
print(plot_homoscedasticity)
# No, the Residuals & Fitted values seem to be scattered across.

# 3.2. Check the model for leverage points, outliers & influential points.

leverage = hatvalues(pima_model)
std_residuals = rstandard(pima_model)
cooks = cooks.distance(pima_model)

diagnostic_results = data.frame(Subject = rownames(Pima.tr),
                        Leverage = leverage,
                        Std_Residuals = std_residuals,
                        Cooks_Distance = cooks)
print(diagnostic_results)

# 3.3. Use scatter & correlation matrix to examine the existance of collinearity between thec predictors in the model.

install.packages("corrplot")
library(corrplot)



# Scatterplot Matrix

scatterplot_matrix_data = Pima.tr[, c("npreg", "glu", "skin", "bmi", "ped", "bp")]

scatterplot_matrix = ggplot(data = scatterplot_matrix_data, aes(x = npreg, y = glu)) +
  geom_point() + facet_grid(. ~ ., scales = "free") + ggtitle("Scatterplot Matrix") +
  theme_minimal()

print(scatterplot_matrix)

# Correlation Matrix

correlation_matrix = cor(scatterplot_matrix_data)

corrplot(correlation_matrix, method = "color", type = "upper", tl.cex = 0.8)

# 3.4. Compute variance inflation factors(VIF). 

install.packages("car")
library(car)

VIF_values = vif(pima_model)
# Lets combine values.
VIF_results = data.frame(Predictor = names(VIF_values), VIF = VIF_values)
print(VIF_results)

# Alll our values are relatively close to one, indicating minimal collinearity with the other predictors.

# 3.5. Box-Cox method to determine the best transformation on the response.

boxcox_results = boxcox(pima_model, lambda = seq(-2,2, by = 0.1)) # Lets find Optimal Lambda.
opt_lambda = boxcox_results$x[which.max(boxcox_results$y)]
print(paste("Optimal Lambda:", round(opt_lambda, 2)))

# Yes the response variable can be transformed.

# 3.6. Select the best model by using the Akaike Inf Criterion(AIC). Give the fitted model.

library(dplyr)
full_model_aic = AIC(pima_model)
# Create a list of models with different combinations of predictor variables

candidate_models = list(
  model1 = lm(bp ~ npreg + glu + skin + bmi, data = Pima.tr),
  model2 = lm(bp ~ npreg + glu + skin, data = Pima.tr),
  model3 = lm(bp ~ npreg + glu, data = Pima.tr)
)
# Compute AIC for each candidate model
candidate_aic = sapply(candidate_models, AIC)

# Find the model with the lowest AIC
best_model = candidate_models[[which.min(candidate_aic)]]
best_model_name = names(candidate_models)[which.min(candidate_aic)]
best_model_aic = min(candidate_aic)
# Display the best model and its coefficients
summary(best_model)

# 3.7. Use the p-Value approach to test the significance of the regression coefficients.
#      For all the tests, proovide the H0 & H1 & rejection region. 

summary_model = summary(pima_model)
# Extract coefficient estimates, standard errors, and p-values
coefs = coef(summary_model)
coef_estimates = coefs[, 1]
std_errors = coefs[, 2]
p_values = coefs[, 4]

# Define the significance level (alpha)
alpha = 0.05

# Conduct the significance tests for each coefficient
for (i in 1:length(coef_estimates)) {
  # Null hypothesis: The coefficient is not significantly different from zero
  # Alternative hypothesis: The coefficient is significantly different from zero
  # Critical region (Rejection region): p-value < alpha (significant), p-value >= alpha (non-significant)
  if (p_values[i] < alpha) {
    conclusion <- "Significant"
  } else {
    conclusion <- "Not significant"
  }
  cat(paste("Coefficient:", names(coef_estimates)[i], "\n"))
  cat(paste("Estimate:", coef_estimates[i], "\n"))
  cat(paste("Standard Error:", std_errors[i], "\n"))
  cat(paste("P-value:", p_values[i], "\n"))
  cat(paste("Conclusion:", conclusion, "\n"))
  cat("\n")
}
