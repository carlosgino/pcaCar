#https://mkmanu.wordpress.com/2014/09/11/what-is-causing-customers-to-churn-attrition-analysis-using-r/


attrition.clients <- read.csv("~/GitHub/pcaCar/Attrition/attrition clients.csv")

summary(attrition.clients)

# ac <- glm (Churn ~ ., data = attrition.clients, family = "binomial") no es buen ajuste


fitlogit <- glm(formula = Churn ~ Gender + Age + Income + FamilySize + Education + 
         Calls + Visits, family = "binomial", data = attrition.clients)

fitlogit

summary(fitlogit)

# Clearly we can observe in the summary that all the variables are significant at least at 95% Confidence.

# Analysis of variances
round(x = anova(fitlogit), digits = 4)


library(aod)
# Variance - Covariance table
round(x = vcov(fitlogit), digits = 4)

# Coefficient of variables in fitted model
round(x = coef(fitlogit), digits = 4)


# Confidence Intervals using profiled log-likelihood in the test
round(x = confint(fitlogit), digits = 4)


# Confidence Intervals using standard errors in the test
round(x = confint.default(fitlogit), digits = 4)

# Calculating odds ratios for the variables
round(x = exp(coef(fitlogit)), digits = 4)


