# Applied Statistics & Machine Learning.
# Topic : Modelling.
# Project Assestment 3.

# head(fat,2)
# model = lm(body.fat ~ . - body.fat.siri - density, data=fat)
# summary(model)

# Question 1.1.

# Aims to understand and analyze body composition, particularly body fat percentage, in individuals.
# The primary purpose of this study is to predict the % of body fat using various body measurements.


# Question 1.2.

# R**2 = 97% of the variation in body fat is explained by the predictors in the model.


# Question 1.3.

# Before we answer the question we must 1st calculate the VIF values.
calculate_vif = function(model) {
  vif_values = NULL
  predictors = names(coef(model))[-1] # Exclude the intercept
  
  for (predictor in predictors) {
    sub_model = lm(as.formula(paste(predictor, "~ .")), data = model$model)
    r_squared = summary(sub_model)$r.squared
    vif = 1 / (1 - r_squared)
    vif_values = c(vif_values, vif)
  }
  
  names(vif_values) = predictors
  return(vif_values)
}

# Calculate the VIF values
vif_values = calculate_vif(model)

# Print the VIF values
print(vif_values)

# Print the VIF values
print(vif_values)

# The predictors weight, BMI, ffweight, chest, abdomen, and hip have VIF values significantly greater than 10,
# This indicates high multicollinearity.

# weight (99.947904) and ffweight (57.342547) are particularly extreme, 
# suggesting that these variables are highly collinear with other predictors.

# The model does show signs of multicollinearity, particularly with the predictors weight, 
# BMI, ffweight, chest, abdomen, and hip.


# Question 1.4.

# (a)

install.packages("matrixcalc")
library(matrixcalc)

# Fit the linear model (same as before)
model = lm(body.fat ~ . - body.fat.siri - density, data=fat)

# Extract the model matrix
model_matrix = model.matrix(model)

# Calculate the condition number
condition_number = kappa(model_matrix, exact = TRUE)

# Print the condition number
print(condition_number)

# Condition Number >= 30: High multicollinearity.
# Given that the condition number is 21807.79 significantly greater than 30, this suggests very high multicollinearity 
# among the variables in the model. 

# (b)

install.packages("GGally")
library(GGally)

# Subset of relevant variables (excluding the response variable)
predictors = fat[, -c(1, 3, 4)]  # Excluding 'case', 'body.fat.siri', 'density'

# Create scatterplot matrix
ggpairs(predictors)

# Calculate pairwise correlation matrix
cor_matrix <- cor(predictors)

# Print the correlation matrix
print(cor_matrix)

# (c)

# The VIF values indicate that there is significant multicollinearity in your model, 
# particularly with the predictors weight, BMI, ffweight, chest, abdomen, and hip.  
# This aligns with the high condition number and the high pairwise correlations observed previously.

# Question 1.5.

# Insignificant Predictors (p-value > 0.05):

# case (p = 0.845051), age (p = 0.617838), height (p = 0.192236), neck (p = 0.799472), hip (p = 0.919994)
# knee (p = 0.301078), ankle (p = 0.083397), bicep (p = 0.077163), wrist (p = 0.436148)

# Fit a new model without the insignificant predictors
model_refined = lm(body.fat ~ weight + BMI + ffweight + chest + abdomen + thigh + forearm, data=fat)
summary(model_refined)

# Yes it influences the collinearity in the model,removing insignificant predictors influences
# multicollinearity potentially reducing it .