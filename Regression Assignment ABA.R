getwd()
setwd("C:/Users/chole/OneDrive/Advanced BA/QMBE-3730/QMBE-3730")

Problem Set

#1 What is the difference between correlation and causation? Give an example.
Correlation is a statistical relationship between two variables, meaning they tend to move together in some way. However, causation means that one variable directly affects or causes the other to change.
Correlation: Ice cream sales and drowning incidents both increase during the summer.

#2 Plot a scatterplot between two variables. Is the relationship linear?


library(ggplot2)

my.plot <- ggplot(data = coffee_shop_revenue, aes(x = Marketing_Spend_Per_Day, y = Daily_Revenue)) + 
  geom_point(aes(color = factor(Number_of_Employees))) +
  ggtitle("Daily Revenue vs Marketing Spend") +
  xlab("Marketing Spend Per Day") + 
  ylab("Daily Revenue") + 
  scale_color_discrete(name = "Number of Employees")

my.plot

#3 Fit a simple linear regression model. Interpret the slope and intercept.

install.packages('caTools')
library(caTools)
split = sample.split(coffee_shop_revenue$Marketing_Spend_Per_Day, SplitRatio = 0.7)
trainingset = subset(coffee_shop_revenue, split == TRUE)
testset = subset(coffee_shop_revenue, split == FALSE)

# Fitting Simple Linear Regression to the Training set
lm.r= lm(formula = Marketing_Spend_Per_Day ~ Daily_Revenue,
         data = coffee_shop_revenue)
#Summary of the model
summary(lm.r)


#4 Report the R² of your model. What does it mean?

Adjusted R-squared:  0.06446 about 6.4% of the variability in the dependent variable is explained by your model. This is relatively low, indicating that your model does not explain much of the variation in the data and may not be a strong predictor.

#5 Check residuals: are they randomly scattered around zero? Plot them.

# Calculate residuals
coffee_shop_revenue$residuals <- lm.r$residuals
coffee_shop_revenue$fitted <- lm.r$fitted.values

# Load ggplot2 if not already loaded
library(ggplot2)

# Plot residuals vs. fitted values
resid_plot <- ggplot(coffee_shop_revenue, aes(x = fitted, y = residuals)) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ggtitle("Residual Plot") +
  xlab("Fitted Values") +
  ylab("Residuals") +
  theme_minimal()

resid_plot

#6 Fit a multiple linear regression model. Which predictors are significant?


lm2= lm(data = coffee_shop_revenue, Daily_Revenue ~ Number_of_Customers_Per_Day + Average_Order_Value + Operating_Hours_Per_Day + Number_of_Employees + Marketing_Spend_Per_Day + Location_Foot_Traffic + Daily_Revenue)

summary(lm2)

Number_of_Customers_Per_Day (p < 2e-16)
Average_Order_Value (p < 2e-16)
Marketing_Spend_Per_Day (p < 2e-16)

#7 Conduct feature scaling and explain when it's necessary.

df_scaled <- as.data.frame(scale(coffee_shop_revenue))
summary(df_scaled)



#8 What is multicollinearity? How would you detect and address it?

Multicollinearity occurs when two or more predictor variables in a regression model are highly correlated, Difficulty in assessing the individual effect of each predictor.


#9 Perform stepwise feature selection (forward, backward, or both).

# Perform backward selection using step() function
backward_model <- step(lm2, direction = "backward")

# Print the summary of the selected model
summary(backward_model)

#10 Report adjusted R². Why might it be better than R²?

0.8913 is the adjusted r squared 
Adjusted r squared is a more refined version  which the adjusted r squared is better than the r squared

#11 Diagnose heteroscedasticity. What are its consequences, and how can you fix it?

install.packages(c("lmtest", "car"))
library(lmtest)
library(car)

bp_test <- bptest(lm2)
print(bp_test)

studentized Breusch-Pagan test

data:  lm2
BP = 4.3496, df = 6, p-value = 0.6295

Since the p-value > 0.05, you fail to reject the null hypothesis of homoscedasticity (equal variance).
This means there is no evidence of heteroscedasticity in your model — your residuals appear to have constant variance.

#12 Explain and compute your predictors' Variance Inflation Factor (VIF).

lm_model <- lm(Daily_Revenue ~ Number_of_Customers_Per_Day + Average_Order_Value + 
                 Operating_Hours_Per_Day + Number_of_Employees + 
                 Marketing_Spend_Per_Day + Location_Foot_Traffic,
               data = coffee_shop_revenue)

# Compute VIF
vif(lm_model)

#13 Fit a non-linear regression model. Of your three models, which one is the “best”? Why?

# Linear Model
linear_model <- lm(Daily_Revenue ~ Number_of_Customers_Per_Day + Average_Order_Value + 
                     Operating_Hours_Per_Day + Number_of_Employees + 
                     Marketing_Spend_Per_Day + Location_Foot_Traffic,
                   data = coffee_shop_revenue)
summary(linear_model)


# Stepwise model (if not already defined)
library(MASS)
null_model <- lm(Daily_Revenue ~ 1, data = coffee_shop_revenue)
full_model <- lm(Daily_Revenue ~ Number_of_Customers_Per_Day + Average_Order_Value + 
                   Operating_Hours_Per_Day + Number_of_Employees + 
                   Marketing_Spend_Per_Day + Location_Foot_Traffic,
                 data = coffee_shop_revenue)
stepwise_model <- step(null_model, scope = list(lower = null_model, upper = full_model),
                       direction = "both")
summary(stepwise_model)

# Non-linear model using log transformation
nonlinear_model <- lm(Daily_Revenue ~ log(Number_of_Customers_Per_Day) + Average_Order_Value + 
                        Operating_Hours_Per_Day + Number_of_Employees + 
                        Marketing_Spend_Per_Day + Location_Foot_Traffic,
                      data = coffee_shop_revenue)
summary(nonlinear_model)

# Compare models
AIC(linear_model, stepwise_model, nonlinear_model)
summary(linear_model)$adj.r.squared
summary(stepwise_model)$adj.r.squared
summary(nonlinear_model)$adj.r.squared

The Stepwise Model has the highest adjusted r squared so that model I think would work the best



