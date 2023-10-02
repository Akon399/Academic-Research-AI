# Research Project 4.
# Submission Date : 2 October 2023.
# Solution by Akhona Njeje.


# Question 1.

# 1.1. ANOVA is a statistical method used to compare the means of three or more groups to determine if there are significant differences among them.

# 1.2. ANOVA, the independent variables are categorical and discrete, such as factors like treatment conditions. 
#      Regression analysis, the independent variableS are typically continuous, like age, income, or time.

# 1.3. (One-Way) ANOVA,involves the analysis of variation among groups based on a single categorical factor. 
#      For example, comparing the test scores of students taught by three different teachers.

#      (two-way) ANOVA, involves the analysis of variation based on two categorical factors simultaneously. 
#      For example,examining how both gender and education level affect job satisfaction.

# 1.4. 

# 1.4.1. The treatment variable is the type of calcium channel blocker administered to the patients.
# 1.4.2. The response variable in this study is the change in systolic blood pressure.
# 1.4.3. Might be included in the error term is the patients' dietary habits.Dietary habits,including salt intake,can significantly affect blood pressure.
# 1.4.4. Degrees of freedom(df1 & df2) : df1 = 3 -1 = 2 & df2 = 250 - 3 = 247.



# Question 2.

pigs = read.csv("C:/Users/User/Desktop/Academia Projects/Assignment 4/pigs.csv")
print(pigs)

# 2.1.

library(ggplot2)
# install.packages("reshape2")
library(reshape2)
# Melt the DataFrame into a long format
box_pigs = melt(pigs)

# Create a boxplot
ggplot(data = box_pigs, aes(x = variable, y = value)) +
  geom_boxplot() +
  labs(x = "Treatments", y = "Values") +
  ggtitle("Boxplot of the Data.")

# 2.2.
# Yes theres a differences among the means & the variances are closely homogeneous but,
# treatment 5 has more variability compared to others.

# 2.3.
# y_ij is the observed value.
# u is the population mean.
# a_i is the effect of the ith treatment group.
# E_ij is the error team.

# 2.4.
# Normality.
# Linearity.
# Treatment effects.
# Independence of observation.
# Random sampling.
# Equal group size.
# Homogeneity of Variances.

# 2.5.
library(reshape2)
# Melt the data while specifying the id variable
melted_pigs = melt(pigs, id.vars = NULL)
# Perform ANOVA
result = aov(value ~ variable, data = melted_pigs)
# Print the ANOVA table
summary(result)


# 2.6.

library(reshape2)
# Melt the data while specifying the id variable
melted_pigs = melt(pigs, id.vars = NULL)

# Perform ANOVA
result = aov(value ~ variable, data = melted_pigs)

# Perform Tukey's HSD test
tukey_result = TukeyHSD(result)

# Print the results
print(tukey_result)

# 2.7.

library(ggplot2)
library(carData)
library(car)

# Assuming you still have 'result' as the ANOVA result from previous steps

# Residual Plot
par(mfrow=c(2,2))
plot(result)

# Normality Plot (Q-Q Plot)
qqPlot(result$residuals)

# Homoscedasticity (Constant Variance) Test
spreadLevelPlot(result)

# Shapiro-Wilk Test for Normality of Residuals
shapiro.test(result$residuals)

# Bartlett's Test for Homogeneity of Variances
bartlett.test(result$residuals, melted_pigs$variable)

# Levene's Test for Homogeneity of Variances
leveneTest(result)

# Breusch-Pagan Test for Heteroscedasticity (if you have concerns)
bptest(result)

# Cook's Distance (for identifying influential observations)
cooksd <- cooks.distance(result)
plot(cooksd, pch = "18", main = "Cook's Distance")

# You can add more diagnostic plots and tests as needed.


# Question 3.

# 3.1

brain = read.csv('C:/Users/User/Desktop/Academia Projects/Assignment 4/brain1.csv', sep = ';', header = TRUE)
print(brain)

# Load the necessary libraries if not already loaded
if (!require(ggplot2)) {
  install.packages("ggplot2")
  library(ggplot2)
}

# Create a scatter plot with all points in different colors for columns A, B, C, and D
scatter_plot_all = ggplot(brain, aes(x = Physical.Therapy.programme)) +
  geom_point(aes(y = A, color = "Psychiatric treament A")) +
  geom_point(aes(y = B, color = "Psychiatric treament B")) +
  geom_point(aes(y = C, color = "Psychiatric treament C")) +
  geom_point(aes(y = D, color = "Psychiatric treament D")) +
  scale_color_manual(values = c("Psychiatric treament A" = "red", 
                                "Psychiatric treament B" = "blue", 
                                "Psychiatric treament C" = "green", 
                                "Psychiatric treament D" = "purple")) +
  labs(title = "Psychiatric type & Physical therapy.") +
  xlab("Physical Therapy Program") +
  ylab("Value")

# Display the scatter plot
print(scatter_plot_all)



# 3.2.

# Fit the multiple linear regression model
model = lm(Physical.Therapy.programme ~ A + B + C + D, data = brain)

# Create a data frame for prediction
predict_data = data.frame(
  A = mean(brain$A),
  B = mean(brain$B),
  C = mean(brain$C),
  D = mean(brain$D)
)

# Predict Physical.Therapy.programme using the model
predict_data$Physical.Therapy.programme = predict(model, newdata = predict_data)

# Reshape the data frame for plotting
predict_data = reshape2::melt(predict_data, id.vars = "Physical.Therapy.programme")

# Create line plots for A, B, C, and D
line_plots = ggplot(predict_data, aes(x = variable, y = value, group = 1)) +
  geom_line() +
  labs(title = "Line Plots for A, B, C, and D (Predicted)") +
  xlab("Variables") +
  ylab("Predicted Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create a scatter plot of Physical.Therapy.programme vs. Predicted Physical.Therapy.programme
scatter_plot = ggplot(predict_data, aes(x = "Physical.Therapy.programme", y = Physical.Therapy.programme, group = 1)) +
  geom_point(color = "red", size = 3) +
  geom_line(aes(group = 1), color = "blue", size = 1) +
  labs(title = "Scatter Plot of Physical.Therapy.programme vs. Predicted Physical.Therapy.programme") +
  xlab("") +
  ylab("Physical.Therapy.programme and Predicted Physical.Therapy.programme") +
  theme_minimal()

# Arrange the plots side by side
library(gridExtra)
grid.arrange(line_plots, scatter_plot, ncol = 2)





# Load necessary libraries if not already loaded

# Fit the interaction effect model
model1 <- lm(A ~ Physical.Therapy.programme * A, data = brain)

# Print model summary
summary(model)

# 3.3.

# Fit a two-way ANOVA model
anova_model = aov(A ~ Physical.Therapy.programme + B + Physical.Therapy.programme:B, data = brain)

# Create the ANOVA table
anova_table = summary(anova_model)

# Print the ANOVA table
print(anova_table)


# Question 3.4.1.

library(dplyr)
library(tidyr)

# Stack columns A, B, C, and D into a single column for the response variable.
stacked_data = brain %>%
  pivot_longer(cols = A:D, names_to = "Treatment", values_to = "Value")

# Fit a one-way ANOVA model to test the hypothesis
anova_model = aov(Value ~ Treatment, data = stacked_data)

# Perform the ANOVA test
anova_result = anova(anova_model)

# Print the ANOVA table
print(anova_result)


# Question 3.4.2

# Group the data by Physical.Therapy.programme and sum the values within each group
grouped_data = brain %>%
  group_by(Physical.Therapy.programme) %>%
  summarize(Sum_A = sum(A), Sum_B = sum(B), Sum_C = sum(C), Sum_D = sum(D))

# Combine the summed columns into a single response variable
stacked_data = grouped_data %>%
  pivot_longer(cols = starts_with("Sum_"), names_to = "Treatment", values_to = "Value")

# Fit a one-way ANOVA model to test the hypothesis
anova_model = aov(Value ~ Treatment, data = stacked_data)

# Perform the ANOVA test
anova_result = anova(anova_model)

# Print the ANOVA table
print(anova_result)



# Question 3.4.3.

library(broom)
# Create a new variable "Treatment" that combines Physical.Therapy.programme and Treatment type
stacked_data = brain %>%
  pivot_longer(cols = A:D, names_to = "Treatment", values_to = "Value") %>%
  mutate(Interaction = factor(paste(Physical.Therapy.programme, Treatment, sep = "-")))
# Fit a two-way ANOVA model to test for interaction effects
anova_model = aov(Value ~ Interaction, data = stacked_data)
# Perform the ANOVA test
anova_result = anova(anova_model)
# Print the ANOVA table
print(anova_result)
# Extract the p-value for interaction effects
interaction_p_value = anova_result$"Pr(>F)"[1]
# Print the p-value for interaction effects
cat("P-value for interaction effects:", interaction_p_value, "\n")
# Interpretation based on the p-value
if (interaction_p_value < 0.05) {
  cat("There are significant interaction effects between psychiatric treatment type and physical therapy program.\n")
} else {
  cat("There are no significant interaction effects between psychiatric treatment type and physical therapy program.\n")
}


# Question 3.5.

# Fit a two-way ANOVA model to test for interaction effects
anova_model = aov(Value ~ Interaction, data = stacked_data)

# Extract residuals from the ANOVA model
residuals = residuals(anova_model)

# Create a Q-Q plot to check normality of residuals
qqnorm(residuals)
qqline(residuals)

# Create a histogram of residuals
hist(residuals, breaks = 20, main = "Histogram of Residuals")

# Create a scatterplot of residuals vs. fitted values
plot(fitted(anova_model), residuals, main = "Residuals vs. Fitted Values", xlab = "Fitted Values", ylab = "Residuals")

# Perform a Shapiro-Wilk normality test on residuals
shapiro.test(residuals)

# Perform a Levene's test for homogeneity of variances
library(car)
leveneTest(anova_model)

# Create a plot of residuals vs. the levels of the interaction factor
interaction_plot(stacked_data$Physical.Therapy.programme, stacked_data$Treatment, residuals, xlab = "Physical Therapy Programme", ylab = "Residuals")

# Create a plot of residuals vs. the predicted values
plot(predict(anova_model), residuals, main = "Residuals vs. Predicted Values", xlab = "Predicted Values", ylab = "Residuals")

# Boxplot of residuals by levels of the interaction factor
boxplot(residuals ~ stacked_data$Physical.Therapy.programme * stacked_data$Treatment, main = "Residuals by Interaction", xlab = "Interaction", ylab = "Residuals")

# Summary statistics of residuals
summary(residuals)












