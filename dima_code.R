#-------------------------------------------------------------------------------
# PART 1

#-------------- Loading libraries; cleaning and formatting data
library(tidyverse)
library(ggplot2)

# Renaming columns for efficiency 
new_names <- c(
  "page", "participant", "figure_on_page", "sample_type",
  "beta60", "c0", "sex", "age", "weight", "height",
  "alcohol_g", "drinking_h", "max_bac", "peak_min"
)
colnames(full_data) <- new_names

# Summary statistics for data set
summary(full_data)

# Creating '+' beta variable to work with 
full_data <- full_data %>%
  mutate(beta = -beta60)

# Summary statistics for beta
summary(full_data$beta)

# Variability in β
min(full_data$beta)
max(full_data$beta)
sd(full_data$beta)
100 * sd(full_data$beta) / mean(full_data$beta)


#-------------- Description & visualisation of current method 

# Histogram of beta elimination rates
ggplot(full_data, aes(x = beta)) +
  geom_histogram(bins = 30, fill = "red", alpha = 0.7) +
  labs(title = "Distribution of β Elimination Rates",
       x = "β (g/kg/h)",
       y = "Frequency") +
  theme_minimal()

# Boxplot of beta elimination rates 
ggplot(full_data, aes(y = beta)) +
  geom_boxplot(fill = "red", alpha = 0.7) +
  labs(title = "Boxplot of β Elimination Rates",
       y = "β (g/kg/h)") +
  theme_minimal()

# Check for outliers
boxplot.stats(full_data$beta)$out

# Calculate the 2.5th and 97.5th percentiles 
beta_2.5 <- quantile(full_data$beta, 0.025, na.rm = TRUE)
beta_97.5 <- quantile(full_data$beta, 0.975, na.rm = TRUE)
cat(sprintf("2.5th percentile: %.4f g/kg/h\n", beta_2.5))
cat(sprintf("97.5th percentile: %.4f g/kg/h\n", beta_97.5))


#-------------- Exploratory plotting & variable correlation 


# Creating BMI variable 
full_data$BMI <- full_data$weight / (full_data$height/100)^2

# Beta vs. Sex plot
ggplot(full_data, aes(x = sex, y = beta, fill = sex)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "β Elimination Rate by Sex",
       x = "Sex",
       y = "β (g/kg/h)") +
  theme_minimal()

# Beta vs. Age plot
ggplot(full_data, aes(x = age, y = beta)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "β Elimination Rate vs Age",
       x = "Age (years)",
       y = "β (g/kg/h)") +
  theme_minimal()

# Beta vs. Weight plot
ggplot(full_data, aes(x = weight, y = beta)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "β Elimination Rate vs Weight",
       x = "Weight (kg)",
       y = "β (g/kg/h)") +
  theme_minimal()

# Beta vs. Height plot 
ggplot(full_data, aes(x = height, y = beta)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "β Elimination Rate vs Height",
       x = "Height (cm)",
       y = "β (g/kg/h)") +
  theme_minimal()

# Beta vs. BMI plot 
ggplot(full_data, aes(x = BMI, y = beta)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "β Elimination Rate vs BMI",
       x = "BMI",
       y = "β (g/kg/h)") +
  theme_minimal()

# Testing correlation and plot between weight and height  
ggplot(full_data, aes(x = weight, y = height)) +
  geom_point(alpha = 0.6, size = 3, color = "blue") +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Relationship between Weight and Height",
       x = "Weight (kg)",
       y = "Height (cm)") +
  theme_minimal()
cor.test(full_data$weight, full_data$height, use = "complete.obs")

# Testing correlation between weight&height and sex for males
cor.test(full_data$weight[full_data$sex == "male"], 
         full_data$height[full_data$sex == "male"])

# Testing correlation between weight&height and sex for females
cor.test(full_data$weight[full_data$sex == "female"], 
         full_data$height[full_data$sex == "female"])

# Height vs. Weight plot grouped by sex
ggplot(full_data, aes(x = weight, y = height, color = sex)) +
  geom_point(alpha = 0.6, size = 3) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Weight vs Height by Sex",
       x = "Weight (kg)",
       y = "Height (cm)") +
  theme_minimal()

# Testing correlation between BMI and beta
cor.test(full_data$BMI, full_data$beta)


#-------------- Beta model formulation and testing 


# Check model diagnostics
par(mfrow = c(2, 2))
plot(fill)
par(mfrow = c(1, 1))

# General model including variables with trends 
model1 <- lm(beta ~ sex + weight + height + BMI, data = full_data)
summary(model1)

# Current usage model
model0 <- lm(beta ~ 1, data = full_data)
summary(model0)

# Just sex model 
model_sex <- lm(beta ~ sex, data = full_data)
summary(model_sex)

# Just age model
model_age <- lm(beta ~ age, data = full_data)
summary(model_age)

# Just height model 
model_height <- lm(beta ~ height, data = full_data)
summary(model_height)

# Just weight model  
model_weight <- lm(beta ~ weight, data = full_data)
summary(model_weight)

# Just BMI model  
model_BMI <- lm(beta ~ BMI, data = full_data)
summary(model_BMI)

# Model comparison
AIC(model1,model0,model_sex,model_age,model_height,model_weight,model_BMI)

# Sex and height interaction term 
model_height_sex_interaction <- lm(beta ~ sex * height, data = full_data)
summary(model_height_sex_interaction)

# Sex and height additive term 
model_height_sex <- lm(beta ~ sex + height, data = full_data)
summary(model_height_sex)

# Comparison between height, sex, additive, interaction, general model
AIC(model_height,model_height_sex,model_sex,model1)

# Testing correlation between height and sex
full_data$sex_numeric <- ifelse(full_data$sex == "male", 1, 0)
cor.test(full_data$height, full_data$sex_numeric)

# Plot of sex-specific effect of height on beta
ggplot(full_data, aes(x = height, y = beta, color = sex)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
  labs(
    title = "Sex-Specific Effect of Height on Beta",
    x = "Height (cm)",
    y = "Beta",
    color = "Sex"
  ) +
  scale_color_manual(values = c("female" = "red", "male" = "blue")) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")

# ANOVA test and comparison for all height & sex models 
anova(model_height, model_height_sex)  
anova(model_sex, model_height_sex)    
anova(model_sex, model_height_sex_interaction)
anova(model_height_sex,model_height_sex_interaction)
AIC(model_height_sex,model_height_sex_interaction)

# Summary table of calculated statistics 
comparison_final <- data.frame(
  Model = c("Null (current)", 
            "Height only", 
            "Sex only",
            "Height + Sex (additive)",
            "Height * Sex (interaction)",
            "All predictors"),
  AIC = c(AIC(model0), 
          AIC(model_height), 
          AIC(model_sex),
          AIC(model_height_sex),
          AIC(model_height_sex_interaction),
          AIC(model1)),
  R_squared = c(0,
                summary(model_height)$r.squared,
                summary(model_sex)$r.squared,
                summary(model_height_sex)$r.squared,
                summary(model_height_sex_interaction)$r.squared,
                summary(model1)$r.squared)
)

comparison_final <- comparison_final[order(comparison_final$AIC), ]
print(comparison_final)

# 1. LOOCV RMSE for your model
library(boot)
cv_glm <- glm(beta ~ sex * height, data = full_data)
loocv_rmse <- sqrt(mean((cv_glm$residuals)^2))  # or use cv.glm()

# 2. Current method RMSE
beta_2.5 <- quantile(full_data$beta, 0.025)
rmse_current <- sqrt(mean((full_data$beta - beta_2.5)^2))

loocv_rmse
rmse_current

# 3. Residual plot
full_data$resid_current <- full_data$beta - beta_2.5
full_data$resid_model   <- residuals(cv_glm)

ggplot() +
  geom_point(aes(x = height, y = resid_current, color = sex)) +
  geom_hline(yintercept = 0, linetype = "dashed")


# Probabilistic assessment for case study
new_case <- data.frame(sex = "female", height = 160)
pred <- predict(model_height_sex_interaction, newdata = new_case, 
                interval = "prediction", level = 0.95)

beta_hat <- pred[1, "fit"]
beta_lwr <- pred[1, "lwr"]
beta_upr <- pred[1, "upr"]

Ct <- 0.15; t <- 2; limit <- 0.47

C0 <- Ct + beta_hat * t
C0_lwr <- Ct + beta_lwr * t
C0_upr <- Ct + beta_upr * t

# Probability
se_pred <- (C0_upr - C0_lwr) / (2 * 1.96)  # approx
z <- (limit - C0) / se_pred
prob_over <- 1 - pnorm(z)

# Output
cat("C0 point estimate:", round(C0, 4), "\n")
cat("95% PI for C0: [", round(C0_lwr, 4), ",", round(C0_upr, 4), "]\n")
cat("P(C0 > 0.47):", round(prob_over, 5), "\n")

#----------------------------------------------------------
# Check if residuals are normal
shapiro.test(residuals(model_height_sex_interaction))

# 1. Q-Q Plot (MOST IMPORTANT)
par(mfrow = c(2, 2))

qqnorm(residuals(model_height_sex_interaction), 
       main = "Q-Q Plot of Residuals")
qqline(residuals(model_height_sex_interaction), col = "red", lwd = 2)

# 2. Histogram with normal overlay
hist(residuals(model_height_sex_interaction), breaks = 20, 
     probability = TRUE, col = "lightblue",
     main = "Distribution of Residuals",
     xlab = "Residuals")
# Add normal curve
x_seq <- seq(min(residuals(model_height_sex_interaction)), 
             max(residuals(model_height_sex_interaction)), 
             length = 100)
lines(x_seq, dnorm(x_seq, 
                   mean = mean(residuals(model_height_sex_interaction)),
                   sd = sd(residuals(model_height_sex_interaction))),
      col = "red", lwd = 2)

# 3. Density plot
plot(density(residuals(model_height_sex_interaction)), 
     main = "Kernel Density of Residuals", lwd = 2)
lines(x_seq, dnorm(x_seq,
                   mean = mean(residuals(model_height_sex_interaction)),
                   sd = sd(residuals(model_height_sex_interaction))),
      col = "red", lwd = 2, lty = 2)
legend("topright", c("Actual", "Normal"), 
       col = c("black", "red"), lty = c(1, 2), lwd = 2)









# Create height lookup tables
heights_seq <- seq(150, 200, by = 5)

female_ref <- data.frame(
  Height_cm = heights_seq,
  Beta_2.5_percentile = sapply(heights_seq, 
                               function(h) predict_beta_female(h)[,"lwr"])
)

male_ref <- data.frame(
  Height_cm = heights_seq,
  Beta_2.5_percentile = sapply(heights_seq, 
                               function(h) predict_beta_male(h)[,"lwr"])
)


