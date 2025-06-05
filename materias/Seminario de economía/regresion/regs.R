# Caution: this clears the Environment 
rm(list=ls()) 

# Installing --------------------------------------------------------------
library(PoEdata)
library(AER)
library(MASS)

# devtools::install_github("https://github.com/ccolonescu/PoEdata")

# Data and plotting -------------------------------------------------------
data("cps_small")

plot(cps_small$educ, cps_small$wage, 
     xlab="education", ylab="wage")

data(food)
head(food)

data("food", package="PoEdata")
plot(food$income, food$food_exp, 
     ylim=c(0, max(food$food_exp)),
     xlim=c(0, max(food$income)),
     xlab="weekly income in $100", 
     ylab="weekly food expenditure in $", 
     type = "p")


# Regression --------------------------------------------------------------

mod1 <- lm(food_exp ~ income, data = food)
b1 <- coef(mod1)[[1]]
b2 <- coef(mod1)[[2]]
smod1 <- summary(mod1)
smod1

plot(food$income, food$food_exp, 
     xlab="weekly income in $100", 
     ylab="weekly food expenditure in $", 
     type = "p")

abline(b1,b2)

mod1$coefficients


# 4 Linear Regression with One Regressor ----------------------------------
# load the the data set in the workspace
data(CASchools)
# Create sample data
STR <- c(15, 17, 19, 20, 22, 23.5, 25)
TestScore <- c(680, 640, 670, 660, 630, 660, 635) 

# Print out sample data
STR #student-teacher ratios
TestScore

# create a scatterplot of the data
plot(TestScore ~ STR,ylab="Test Score",pch=20)

# add the systematic relationship to the plot
abline(a = 713, b = -3)

# compute STR and append it to CASchools
CASchools$STR <- CASchools$students/CASchools$teachers 

# compute TestScore and append it to CASchools
CASchools$score <- (CASchools$read + CASchools$math)/2     

# compute sample averages of STR and score
(avg_STR <- mean(CASchools$STR))
(avg_score <- mean(CASchools$score))

# compute sample standard deviations of STR and score
(sd_STR <- sd(CASchools$STR)) 
(sd_score <- sd(CASchools$score))

# set up a vector of percentiles and compute the quantiles 
(quantiles <- c(0.10, 0.25, 0.4, 0.5, 0.6, 0.75, 0.9))
(quant_STR <- quantile(CASchools$STR, quantiles))
(quant_score <- quantile(CASchools$score, quantiles))

# gather everything in a data.frame 
DistributionSummary <- data.frame(Average = c(avg_STR, avg_score), 
                                  StandardDeviation = c(sd_STR, sd_score), 
                                  quantile = rbind(quant_STR, quant_score))

# print the summary to the console
DistributionSummary

plot(score ~ STR, 
     data = CASchools,
     main = "Scatterplot of Test Score and STR", 
     xlab = "STR (X)",
     ylab = "Test Score (Y)")  

cor(CASchools$STR, CASchools$score)

attach(CASchools) # allows to use the variables contained in CASchools directly

# compute beta_1_hat
(beta_1 <- sum((STR - mean(STR)) * (score - mean(score))) / sum((STR - mean(STR))^2))

# compute beta_0_hat
(beta_0 <- mean(score) - beta_1 * mean(STR))

# print the results to the console
beta_1
beta_0

# estimate the model and assign the result to linear_model
linear_model <- lm(score ~ STR, data = CASchools)

# print the standard output of the estimated lm object to the console 
linear_model

# plot the data
plot(score ~ STR, 
     data = CASchools,
     main = "Scatterplot of Test Score and STR", 
     xlab = "STR (X)",
     ylab = "Test Score (Y)",
     xlim = c(10, 30),
     ylim = c(600, 720))

# add the regression line
abline(linear_model) 

mod_summary <- summary(linear_model)
mod_summary

# Extract R-squared
r_squared <- mod_summary$r.squared
cat("R-squared:", r_squared, "\n")
cat("This means that", round(r_squared * 100, 1), "% of the variance in score is explained by STR.\n")

# Extract Residual Standard Error
ser <- mod_summary$sigma
cat("Residual Standard Error (SER):", ser, "\n")
cat("On average, the deviation of the actual test score from the regression line is", round(ser, 2), "points.\n")
