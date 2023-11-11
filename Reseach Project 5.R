# Research Project 5.
# Submission Date : 12 October 2023.
# Solution by Akhona Njeje.

# Question 1.1.

fish = read.csv("C:/Users/User/Desktop/Academia/Assignment 5/Fish.csv", sep = ";")
print(head(fish, 5))

library(ggplot2)

ggplot(fish, aes(x = initial_weight, y = Resulting_weight, color = factor(Diet_type), shape = factor(Diet_type))) +
  geom_point(size = 3) +
  labs(title = "Final Weight vs. Initial Weight of Guppy Fish by Diet Type",
       x = "Initial Weight (x)",
       y = "Final Weight (y)",
       color = "Diet Type",
       shape = "Diet Type") +
  theme_minimal()

# Question 1.2.

ancova_result = aov(initial_weight ~ factor(Diet_type) + Resulting_weight, data = fish)

# Summary of the ANCOVA model
summary(ancova_result)

# There is no significant difference in the initial weight of fish across the three diet types (p = 0.94634 > 0.05).
# Therefore, the null hypothesis cannot be rejected.
# The resulting weight is significantly related to the initial weight (p = 0.00985 < 0.05).
# This analysis suggests that the type of diet does not significantly affect the initial weight of the fish, but the final weight after feeding is significantly related to their initial weight.

# Question 1.3.

anova_result = aov(Resulting_weight ~ factor(Diet_type), data = fish)
summary(anova_result)

# Question 1.4.

model_interaction = lm(Resulting_weight ~ initial_weight * factor(Diet_type), data = fish)

anova_interaction = anova(model_interaction)
# Summary of the ANOVA model
summary(anova_interaction)

# Question 1.5.

# Y_ij = u + a_i + b(X_ij - X_bar) + e.

# Y_ij = The resulting weight of the j-th fish in the i-th diet group.
# u = The overall mean resulting weight across all groups.
# a_i = The effect of the i-th diet type.
# b = The common slope coefficient for the covariate (initial weight).
# X_ij = The initial weight of the j-th fish in the i-th diet group.
# X_bar = The overall mean of the initial weight across all groups.
# e = The random error term for the j-th fish in the i-th diet group.

# Question 1.6.

# Interaction Plot (Homogeneity of Slopes).
ggplot(fish, aes(x = initial_weight, y = Resulting_weight, color = factor(Diet_type))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Interaction Plot for Checking Homogeneity of Slopes")

# Q-Q plot.
model = lm(Resulting_weight ~ initial_weight + factor(Diet_type), data = fish)
qqnorm(residuals(model))
qqline(residuals(model), col = "red")

# Residuals vs. Fitted Values.
plot(fitted(model), residuals(model))
abline(h = 0, col = "red")

