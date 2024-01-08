# install.packages("forcats")
# install.packages("ggplot2")
# install.packages("glmnet")
# install.packages("tidyverse")
library(glmnet)
library(forcats)
library(dplyr)
library(ggplot2)
library(stats)
library(tidyverse)

df <- read.csv("Zorenberg_data.csv")
summary(df)
sum(is.na(df[,!(names(df) %in% c("age_of_onset"))]))

sum(is.na(df[,!(names(df) %in% c("age_of_onset"))]))
sum(df[,'psychosis_status'])

sum(df$psychosis_status)
summary(df)

columns_to_normalize <- c("genetic_risk_score_hypoxia", "genetic_risk_score_all")
# Z-normalize the specified columns
df[columns_to_normalize] <- scale(df[columns_to_normalize])
typeof(df$genetic_risk_score_all)


#df$psychosis_status <- as.factor(df$psychosis_status)
df$sex <- ifelse(test = df$sex=='male',yes=0, no=1)
df$sex<- as.integer(df$sex)

df$maternal_smoking <- ifelse(test = df$maternal_smoking=='yes',yes=1, no=0)
df$maternal_smoking <-  as.integer(df$maternal_smoking)
df$family_history <- ifelse(test = df$family_history=='yes',yes=1, no=0)
df$family_history <-  as.integer(df$family_history)
sum(df$sex)
sum(df$maternal_smoking)
sum(df$family_history)

help("xtabs")
xtabs(~ psychosis_status + sex, data=df)
xtabs(~ psychosis_status + maternal_smoking, data=df)
xtabs(~ psychosis_status + family_history, data=df)
xtabs(~df$psychosis_status+df$hypoxia_status)


log_mod <- glm(df$psychosis_status ~ df$hypoxia_status
               + df$family_history + df$sex + df$maternal_smoking,
               family = "binomial")
summary(log_mod)
confint(log_mod)
cor(df$sex, df$family_history)
names(df)


# Part 1, Logistic Regression adjsuted for biologically plausible variables:

logistic_1 <-  glm(df$psychosis_status~df$hypoxia_status+df$sex+df$family_history+df$maternal_smoking, family='binomial')
summary(logistic_1)
round(exp(cbind(OR = coef(logistic_1), confint(logistic_1, level = 0.95))),2)
confint(logistic_1, level = 0.95)
vif(logistic_1)

ggplot(df, aes(x = genetic_risk_score_hypoxia, y = age_of_onset)) +
  geom_point() +
  labs(title = "Scatter Plot of Age of Onset by Genetic Risk Score",
       x = "Genetic Risk Score (All)", y = "Age of Onset")

# Box plot
ggplot(df, aes(x = cut(genetic_risk_score_all, breaks = 5), y = age_of_onset)) +
  geom_boxplot() +
  labs(title = "Box Plot of Age of Onset by Genetic Risk Score (All)",
       x = "Genetic Risk Score Groups", y = "Age of Onset") +
  scale_x_discrete(labels = c("Group 1", "Group 2", "Group 3", "Group 4", "Group 5"))

# Filter data for affected and unaffected individuals
affected_data <- df[df$psychosis_status == 1, ]
unaffected_data <- df[df$psychosis_status == 0, ]

# Create histogram with KDE
summary(affected_data)


summary_table <- df %>%
  group_by(psychosis_status) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)
# Display the result
print(summary_table)

# Calculate proportions
proportions_by_group <- df %>%
  group_by(psychosis_status, obstetric_complications_count) %>%
  summarise(count = n()) %>%
  group_by(psychosis_status) %>%
  mutate(proportion = count / sum(count) * 100)

# Display the result
print(proportions_by_group)

contingency_table <- table(df$psychosis_status, df$obstetric_complications_count)
# Perform the chi-square test
chi_square_result <- chisq.test(contingency_table)
# Display the result
print(chi_square_result)

summary(affected_data$obstetric_complications_count)
summary(unaffected_data$obstetric_complications_count)

#install.packages("car")
library(car)

# Create histogram with KDE and add legend and sample size
ggplot() +
#  geom_histogram(data = affected_data, aes(x = genetic_risk_score_all, y = ..density.., fill = "Affected"), 
#                 alpha = 0.4, bins = 20, label = "Affected") +
  geom_density(data = affected_data, aes(x = genetic_risk_score_all, y = ..density.., color = "Affected"), 
               size = 1) +
#  geom_histogram(data = unaffected_data, aes(x = genetic_risk_score_all, y = ..density.., fill = "Unaffected"), 
#                 alpha = 0.4, bins = 30, label = "Unaffected") +
  geom_density(data = unaffected_data, aes(x = genetic_risk_score_all, y = ..density.., color = "Unaffected"), 
               size = 1) +
  labs(title = "Schizophrenia Polygenic Risk Score Distribution",
       x = "Schizophrenia-PRS", y = "") +
#  scale_fill_manual(values = c("Affected" = "blue", "Unaffected" = "green")) +
  scale_color_manual(values = c("Affected" = "blue", "Unaffected" = "green")) +
  theme_minimal() +
  theme(legend.position = "top") +
  guides(fill = guide_legend(title = NULL, override.aes = list(alpha = 1, color = NULL))) +
  annotate("text", x = Inf, y = Inf, hjust = 1.2, vjust = 1.2,
           label = paste("Affected, n =", nrow(affected_data),
                         "\nUnaffected, n =", nrow(unaffected_data)),
           size = 3, color = "black")


# Create histogram with KDE and add legend and sample size
ggplot() +
  geom_density(data = affected_data, aes(x = genetic_risk_score_hypoxia, y = ..density.., color = "Affected"), 
               size = 1) +
  geom_density(data = unaffected_data, aes(x = genetic_risk_score_hypoxia, y = ..density.., color = "Unaffected"), 
               size = 1) +
  labs(title = "Hypoxia Polygenic Risk Score Distribution",
       x = "Hypoxia-PRS", y = "") +
  scale_color_manual(values = c("Affected" = "blue", "Unaffected" = "green")) +
  theme_minimal() +
  theme(legend.position = "top") +
  guides(color = guide_legend(title = NULL, override.aes = list(alpha = 1, fill = NULL))) +
  annotate("text", x = Inf, y = Inf, hjust = 1.2, vjust = 1.2,
           label = paste("Affected, n =", nrow(affected_data),
                         "\nUnaffected, n =", nrow(unaffected_data)),
           size = 3, color = "black")


#  x = "Hypoxia-GRS", y = "Density", labs(title = "KDE of GRS[hypoxia] by Schozephrenia Status") +



X <- as.matrix(df[, !(names(df) %in% c('X', 'psychosis_status', 'age_of_onset', 'obstetric_complications_count'))])

# Response variable
y <- df$psychosis_status


set.seed(126)  # Set seed for reproducibility
# Fit LASSO model with cross-validation
lasso_model <- cv.glmnet(x=X, y=y, family = "binomial")
plot(lasso_model)
# Display the best lambda value selected by cross-validation
best_lambda <- lasso_model$lambda.min
cat("Best Lambda:", best_lambda, "\n")

# Fit the final LASSO model with the best lambda
final_lasso_model <- glmnet(X, y, family = "binomial", alpha = 1, lambda = best_lambda)
summary(final_lasso_model)
coef(final_lasso_model)
coefficients <- coef(final_lasso_model)



n_boot <- 1000
# Initialize a matrix to store bootstrap coefficients
boot_coefs <- matrix(NA, n_boot, 9)
for (i in 1:n_boot) {
  # Sample with replacement from the original data
  sampled_indices <- sample(1:length(y), replace = TRUE)
  X_boot <- X[sampled_indices, ]
  y_boot <- y[sampled_indices]
  
  # Fit LASSO model on bootstrap sample
  boot_model <- glmnet(X_boot, y_boot, family = "binomial", alpha = 1, lambda = best_lambda)
  
  # Extract coefficients for the selected lambda
  boot_coefs[i,] <- coef(boot_model, s = best_lambda)[, 1]
}
ci_lower <- apply(boot_coefs, 2, quantile, 0.025)
ci_upper <- apply(boot_coefs, 2, quantile, 0.975)

exp_ci_lower <- exp(ci_lower)
exp_ci_upper <- exp(ci_upper)
exp_coefficients <- exp(coefficients)

exp_ci_lower
exp_ci_upper
exp_coefficients
coefficients

# Provided coefficients
coefficients <- c(-4.5540895, 2.0130044, 0, 0, 0.2296655, 0, 0, 0.1082970)

glm_fromlasso <- glm(psychosis_status ~ hypoxia_status +
                       maternal_smoking +
                       genetic_risk_score_hypoxia, 
                     family = 'binomial', data=df)
# Create a glm model with the specified coefficients
glm_hypogrs <- glm(psychosis_status ~ hypoxia_status*genetic_risk_score_hypoxia+
                   maternal_smoking +
                   genetic_risk_score_hypoxia,
                 family = "binomial", data = df)

glm_smokinggrs <- glm(psychosis_status ~ hypoxia_status +
                     genetic_risk_score_hypoxia*maternal_smoking,
                   family = "binomial", data = df)

# Display the model summary
summary(glm_fromlasso)
round(exp(cbind(OR = coef(glm_fromlasso), confint(glm_fromlasso, level = 0.95))),2)
summary(glm_model)
summary(glm_hypogrs)
summary(glm_smokinggrs)
summary(logistic_1)


#install.packages("lmtest")
library(lmtest)

# Perform likelihood ratio test
lr_test <- lrtest(logistic_1, glm_fromlasso)
# Display the test results
print(lr_test)


null_model <- glm(psychosis_status ~ 1, family = "binomial", data = df)

# Calculate log-likelihood for both models
log_likelihood_null <- logLik(null_model)
log_likelihood_model <- logLik(logistic_1)

# Calculate McFadden's R-squared
mcfadden_r_squared <- 1 - (logLik(logistic_1) / logLik(null_model))

# Display the result
print(mcfadden_r_squared)


#Part 3

install.packages("pROC")
library(pROC)

# Fit logistic regression models
model_hypoxia_alone <- glm(psychosis_status ~ genetic_risk_score_hypoxia, family = "binomial", data = df)
model_with_covariates <- glm(psychosis_status ~ genetic_risk_score_hypoxia + sex +hypoxia+ maternal_smoking
                             family = "binomial", data = df)
model_with_hypoxia <- glm(psychosis_status ~ genetic_risk_score_hypoxia + sex + hypoxia_status + maternal_smoking,
                             family = "binomial", data = df)
model_only_hypoxia <- glm(psychosis_status ~ hypoxia_status +sex+maternal_smoking,
                          family = "binomial", data = df)

# Create ROC objects with explicit levels and direction
roc_with_covariates <- roc(df$psychosis_status, predicted_probs_with_covariates,
                           direction = ">")

# Predict probabilities
predicted_probs_hypoxia_alone <- predict(model_hypoxia_alone, type = "response")
predicted_probs_with_covariates <- predict(model_with_covariates, type = "response")
predicted_probs_with_hypoxia <- predict(model_with_hypoxia, type = "response")
predicted_probs_only_hypoxia <-  predict(model_only_hypoxia, type="response")

# Create ROC objects
roc_hypoxia_alone <- roc(df$psychosis_status, predicted_probs_hypoxia_alone,)
roc_with_covariates <- roc(df$psychosis_status, predicted_probs_with_covariates)
roc_with_hypoxia <- roc(df$psychosis_status, predicted_probs_with_hypoxia)
roc_only_hypoxia <-  roc(df$psychosis_status, predicted_probs_only_hypoxia)

# Plot ROC curves
plot(roc_only_hypoxia, col = "green", main = "ROC-AUC Hypoxia PRS")
#lines(roc_with_covariates, col = "red")
lines(roc_with_hypoxia, col='orange')
# Add legend
legend("bottomright", legend = c("Hypoxia+Covars, AUC=0.822", "PRS+Covar+Hypoxia, AUC=0.831"), col = c("green",'orange'), lty = 1, bty='n')

# Calculate AUC
auc_hypoxia_alone <- auc(roc_hypoxia_alone)
auc_with_covariates <- auc(roc_with_covariates)
auc_with_hypoxia <- auc(roc_with_hypoxia)
auc_only_hypoxia <- auc(roc_only_hypoxia)
# Display AUC values
cat("AUC - Hypoxia PRS Alone:", auc_hypoxia_alone, "\n")
cat("AUC - Hypoxia PRS with Covariates:", auc_with_covariates, "\n")
cat("AUC - Hypoxia PRS with Covariates and hypoxia:", auc_with_hypoxia, "\n")

delong_test <- roc.test(roc_only_hypoxia, roc_with_hypoxia, method = "delong")
print(delong_test)



model_schiz_prs_alone <- glm(psychosis_status ~ genetic_risk_score_all, family = "binomial", data = df)
model_with_covariates <- glm(psychosis_status ~ genetic_risk_score_all + sex +hypoxia+ maternal_smoking
                             family = "binomial", data = df)
model_with_hypoxia <- glm(psychosis_status ~ genetic_risk_score_hypoxia + sex + hypoxia_status + maternal_smoking,
                          family = "binomial", data = df)
model_only_hypoxia <- glm(psychosis_status ~ hypoxia_status +sex+maternal_smoking,
                          family = "binomial", data = df)

# Create ROC objects with explicit levels and direction
roc_with_covariates <- roc(df$psychosis_status, predicted_probs_with_covariates,
                           direction = ">")

# Predict probabilities
predicted_probs_schiz <- predict(model_schiz_prs_alone, type = "response")
predicted_probs_with_covariates <- predict(model_with_covariates, type = "response")
predicted_probs_with_hypoxia <- predict(model_with_hypoxia, type = "response")
predicted_probs_only_hypoxia <-  predict(model_only_hypoxia, type="response")

# Create ROC objects
roc_hypoxia_alone <- roc(df$psychosis_status, predicted_probs_schiz)
roc_with_covariates <- roc(df$psychosis_status, predicted_probs_with_covariates)
roc_with_hypoxia <- roc(df$psychosis_status, predicted_probs_with_hypoxia)
roc_only_hypoxia <-  roc(df$psychosis_status, predicted_probs_only_hypoxia)

# Plot ROC curves
plot(roc_hypoxia_alone, col = "green", main = "ROC-AUC Hypoxia PRS")
#lines(roc_with_covariates, col = "red")
lines(roc_with_covariates, col='orange')
# Add legend
legend("bottomright", legend = c("Hypoxia+Covars, AUC=0.822", "PRS+Covar+Hypoxia, AUC=0.831"), col = c("green",'orange'), lty = 1, bty='n')

# Calculate AUC
auc_hypoxia_alone <- auc(roc_hypoxia_alone)
auc_with_covariates <- auc(roc_with_covariates)
auc_with_hypoxia <- auc(roc_with_hypoxia)
auc_only_hypoxia <- auc(roc_only_hypoxia)
