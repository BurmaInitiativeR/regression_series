# Quadratic Regression

################################################################################
library(ggplot2)
library(dplyr)

# import data 
# ref: https://www.kaggle.com/datasets/codebreaker619/salary-data-with-age-and-experience
df <- read.csv("Salary_Data.csv") %>%
  rename(
    "age" = "Age", 
    "income" = "Salary"
  )

# plot income vs age 
ggplot(df, aes(x = age, y = income)) +
  geom_point()

# Fit a quadratic regression model
model_s <- lm(income ~ age, data = df) #simple OLS
model_q <- lm(income ~ age + I(age^2), data = df)  # I(age^2) is used to include the squared term

# Get model summary (including coefficients)
summary(model_s)
summary(model_q)

# prediction 
test_df <- data.frame(age = seq(from = 39, to = 60, by = 1))

test_df$predicted_income <- predict(model_q, test_df) 

# Model fitness - test 
anova(model_s, model_q)

# Calculate predicted values
df$predicted_income_ms <- predict(model_s)
df$predicted_income_mq <- predict(model_q)

# Plot the data, quadratic fit, and linear fit
# (1) simple OLS 
ggplot(df, aes(x = age, y = income)) +
  geom_point() +
  geom_line(aes(y = predicted_income_ms, color = "Linear Fit"), linetype = "dashed") 

# (2) quadratic form
ggplot(df, aes(x = age, y = income)) +
  geom_point() +
  geom_line(aes(y = predicted_income_mq, color = "Quadratic Fit"), )

# (3) combined
ggplot(df, aes(x = age, y = income)) +
  geom_point() +
  geom_line(aes(y = predicted_income_ms, color = "Linear Fit"), linetype = "dashed") +
  geom_line(aes(y = predicted_income_mq, color = "Quadratic Fit"), ) +
  labs(title = "Quadratic Regression: Income vs. Age",
       x = "Age",
       y = "Income") 

# Plot the data, quadratic fit, linear fit, and new predictions
ggplot(df, aes(x = age, y = income)) +
  geom_point() +
  geom_line(aes(y = predicted_income_ms, color = "Linear Fit"), linetype = "dashed") +
  geom_line(aes(y = predicted_income_mq, color = "Quadratic Fit")) +
  geom_point(data = test_df, aes(x = age, y = predicted_income), color = "purple", shape = 17, size = 3) +  # Add predictions as points
  labs(title = "Quadratic Regression: Income vs. Age with Predictions",
       x = "Age",
       y = "Income") +
  scale_color_manual(name = "Model", values = c("Linear Fit" = "blue", "Quadratic Fit" = "red")) 

