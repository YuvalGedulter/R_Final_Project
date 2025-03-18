source("Final project A2.R")

data$Treatment_factor <- as.factor(data$Treatment_factor)
data$Height <- as.numeric(data$Height)

# מודל רגרסיה לינארית מרובה
linear_model <- lm(Secretion_EC ~ Treatment_factor + Biomass + Height, data = data)
summary(linear_model)

#Call:
#  lm(formula = Secretion_EC ~ Treatment_factor + Biomass + Height, 
#     data = data)

#Residuals:
#  Min       1Q   Median       3Q      Max 
#-1221.22  -175.84   -19.91   105.40  1539.72 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                  -214.167    112.347  -1.906  0.05866 .  
#Treatment_factorPip            64.393    112.592   0.572  0.56829    
#Treatment_factorSi             -8.222    112.341  -0.073  0.94176    
#Treatment_factorPip+Si         77.214    113.820   0.678  0.49865    
#Treatment_factorNaCl          330.801    116.310   2.844  0.00512 ** 
#  Treatment_factorNaCl+Pip      529.126    119.410   4.431 1.88e-05 ***
#  Treatment_factorNaCl+Si       379.268    120.785   3.140  0.00206 ** 
#  Treatment_factorNaCl+Pip+Si   269.293    119.675   2.250  0.02600 *  
#  Biomass                     11737.587   1416.399   8.287 8.51e-14 ***
#  Height                         23.970     20.563   1.166  0.24573    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 354.5 on 140 degrees of freedom
#(10 observations deleted due to missingness)
#Multiple R-squared:  0.5621,	Adjusted R-squared:  0.5339 
#F-statistic: 19.96 on 9 and 140 DF,  p-value: < 2.2e-16


P1 <- ggplot(data, aes(x = Biomass, y = Secretion_EC, color = Treatment_factor)) +
  geom_point(size = 3, alpha = 0.6) +
  geom_smooth(method='lm', alpha = 0.15) +
  xlab("Biomass (g)") +
  ylab("Secretion EC (µS/cm)") +
  theme_bw()


# מודל רגרסיה לוגיסטית
model <- glm(Normal_EC_binary ~ Treatment_factor + Biomass + Height, family = binomial, data = data)
summary(model)

#Call:
#  glm(formula = Normal_EC_binary ~ Treatment_factor + Biomass + 
#        Height, family = binomial, data = data)

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                   -5.3669     1.5911  -3.373 0.000743 ***
#  Treatment_factorPip            1.0839     1.4092   0.769 0.441823    
#Treatment_factorSi           -15.2078  1299.5596  -0.012 0.990663    
#Treatment_factorPip+Si         1.7108     1.4616   1.171 0.241788    
#Treatment_factorNaCl           4.6390     1.4324   3.239 0.001201 ** 
#  Treatment_factorNaCl+Pip       5.4904     1.4935   3.676 0.000237 ***
#  Treatment_factorNaCl+Si        4.2813     1.4923   2.869 0.004119 ** 
#  Treatment_factorNaCl+Pip+Si    4.6852     1.4898   3.145 0.001662 ** 
#  Biomass                       39.8135    15.4365   2.579 0.009903 ** 
#  Height                        -0.0523     0.1895  -0.276 0.782571    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 180.84  on 148  degrees of freedom
#Residual deviance: 111.36  on 139  degrees of freedom
#(11 observations deleted due to missingness)
#AIC: 131.36

#Number of Fisher Scoring iterations: 17

P2 <- ggplot(data, aes(x = Biomass, y = Normal_EC_binary, color = Treatment_factor)) +
  geom_jitter(width = 0, height = 0.05, alpha = 0.7, size = 3) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, alpha = 0.6) +
  xlab("Biomass (g)") +
  ylab("P (normal salt secretion > 6000µS/cm)") +
  theme_minimal()

install.packages("patchwork")
library(ggplot2)
library(patchwork)
P1 <- P1 + theme(legend.position = "none")
P1 + P2 + plot_layout(guides = "collect")

library(pROC)

data$Height <- as.numeric(data$Height)
predict_probs <- predict(model, newdata = data, type = "response")
predict_probs <- predict(model, type = "response")
valid_idx <- !is.na(data$Normal_EC_binary)
data <- data[valid_idx, ]
predict_probs <- predict_probs[valid_idx]
roc_curve <- roc(data$Normal_EC_binary, predict_probs)
plot(roc_curve, col = "red", main = "ROC curve")
print(roc_curve)
#length(data$Normal_EC_binary)
#length(predict_probs)