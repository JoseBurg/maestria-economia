wage <- wooldridge::wage1

model <- lm(wage ~ educ + exper + tenure, data = wage)
model2 <- lm(wage ~ educ + exper + I(exper^2) + tenure, data = wage)
model3 <- lm(log(wage) ~ educ + exper + I(exper^2) + tenure, data = wage)


model_robust <- lmtest::coeftest(model3, vcov. = sandwich::vcovHC(model3, type = "HC1"))

summary(model3)
performance::compare_performance(model, model2, model3)

performance::check_model(model_robust)
