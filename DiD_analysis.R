#==========================================================================#
# Title:          impact_evaluation_youth_employment_icf
# 
# Author:         Khin Yadanar Lin
# Last updated:   July 14, 2024
#
# Reviewer:       n/a
# Review date(s): n/a

# Description:    This script imports the raw data from the "csdid_example_V1_test" file provided by ICF,
#                 and conducts a Difference-in-Differences (DiD) regression analysis to evaluate the impact.
#
#




#==========================================================================#
# SET UP 
# 
# 1. Load packages required for this R script to run
# 2. Set a working directory for inputs (the .csv file with raw data), and any outputs
# 3. Load the raw survey data
# 4. Conduct data cleaning and Exploratory Data Analysis (EDA)


# Clear (this command removes any R objects in the project environment that might have been created in prior sessions)
rm(list = ls())

# Data Preparation

# Load necessary packages
library(dplyr)    # Data manipulation
library(ggplot2)  # Data visualization
library(plm)      # Panel data linear models
library(lmtest)   # Diagnostic tests for linear models
library(sandwich) # Robust covariance matrix estimators

# Set the working directory
getwd()
setwd("C:/Yadanar/Job Application/Data Analysis_Interview Assignments")

# Load data
raw_data <- read.csv("C:/Yadanar/Job Application/Data Analysis_Interview Assignments/csdid_example_v1_tested.csv")


#---- EXPLORATORY DATA ANALYSIS & DATA CLEANING ----#

# View the data structure and calculate means of columns
View(raw_data)
str(raw_data)
sapply(raw_data, mean, na.rm=TRUE)

# Count the number of LAs in the dataset
nrow(raw_data)

# Check for duplicates in the 'la_id' column
duplicates <- data.frame(table(raw_data$la_id))
duplicates[duplicates$Freq > 1,]  # Display any duplicate records

# Check missing values
missing_values <- is.na(raw_data)
summary(missing_values)

# Count observations per treatment group
year_counts <- raw_data %>%
  count(treat)
print(year_counts)

# Transforming 'treat' and creating 'post_treat' variables for DiD regression analysis
# Assuming treat variable is 1 for treated and some other value (e.g., "never_treated") for control
new_data <- raw_data %>%
  mutate(
    treat = ifelse(treat == "never treated", 0, as.numeric(treat)),  # Ensure 'treat' is binary
    post_treat = ifelse(year == 2005, 1, 0)  # Create 'post_treat' variable indicating post-intervention period
  ) %>%
  select(-first_treat)  # Drop 'first_treat' variable as it is not needed

# Verify transformations by checking the first few and last few rows
tail(new_data)
head(new_data)

# Checking data types to ensure correctness
str(new_data)
View(new_data)

# Changing data types to prepare for analysis
new_data <- new_data %>%
  mutate(
    year = factor(year),         
    treat = factor(treat),       
    post_treat = factor(post_treat), 
    la_id = factor(la_id)        
  )

# Splitting data into treatment and control groups for comparison
treatment_group <- new_data %>% filter(treat == 0)
control_group <- new_data %>% filter(treat == 1)

# Plotting histograms to visualize the distribution of the log population (lpop) variable
hist(control_group$lpop, 
     main = "Histogram of Population for Control Group", 
     xlab = "Log Population (lpop)", 
     col = "lightblue", 
     border = "black")

hist(treatment_group$lpop, 
     main = "Histogram of Population for Treatment Group", 
     xlab = "Log Population (lpop)", 
     col = "lightblue", 
     border = "black")


# Plotting histograms to visualize the distribution of the outcome variables - log employment (lemp)
hist(control_group$lemp, 
     main = "Histogram of Youth Employment for Control Group", 
     xlab = "Log Youth Employment (lemp)", 
     col = "lightblue", 
     border = "black")

hist(treatment_group$lemp, 
     main = "Histogram of Youth Employment for Treatment Group", 
     xlab = "Log Youth Employment (lemp)", 
     col = "lightblue", 
     border = "black")




#==========================================================================#
#CONDUCT A DID ANALYSIS
#
# 1. Compare the means of log-population (lpop) between treatment and control groups (if the difference is statistically significant, the variable will be included as a control variable) 
# 2. Check the Parallel Trend Assumption by using Parallel Trends Plot and Pre-trend regression analysis
# 3. Transform the dataset into a panel dataset ("la_id","year" as index)
# 4. Run a DiD regression model
# 5. Calculate robust standard errors to account for heteroscedasticity and autocorrelation in panel data


# Calculate summary statistics for the population by treatment group
population_summary <- new_data %>%
  group_by(treat) %>%
  summarise(
    mean_population = mean(lpop, na.rm = TRUE),   # Mean of the log population
    sd_population = sd(lpop, na.rm = TRUE),       # Standard deviation of the log population
    n = n()                                       # Number of observations
  )

print("Population Summary:")
print(population_summary)

# Perform a t-test to compare the population means between treatment and control groups
t_test_result <- t.test(lpop ~ treat, data = new_data)

print("T-Test Result:")
print(t_test_result)

# Interpretation of t-test result:
# The t-test indicates that the difference in population means between the two groups is statistically significant at 0.05 significance level.


#---- PARALLEL TREND ASSUMPTION CHECK ----#

# Visual Inspection

# Summarise mean log employment (lemp) by year and treatment group
plot_data <- new_data %>%
  group_by(year, treat) %>%
  summarise(mean_lemp = mean(lemp, na.rm = TRUE))

print(plot_data)

# Create the parallel trends plot using ggplot2

# Include labels of treatment and control groups for data visualisation
plot_data <- plot_data %>%
  mutate(treat = factor(treat, levels = c(0, 1), labels = c("Control", "Treatment")))

ggplot(plot_data, aes(x = year, y = mean_lemp, color = treat, group = treat)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  geom_vline(xintercept = 2, linetype = "dashed", color = "red") +
  annotate("text", x = 2.3, y = max(plot_data$mean_lemp), label = "Post-Intervention", color = "red", angle = 0) +
  annotate("text", x = 1.7, y = max(plot_data$mean_lemp), label = "Pre-Intervention", color = "red", angle = 0) +
  labs(title = "Parallel Trends Plot",
       x = "Year",
       y = "Average Log of Youth Employment (lemp)",
       color = "Group") +
  theme_minimal() +
  theme(axis.line = element_line(size = 1, colour = "black"),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))

# Statistical Test for Parallel Trends Assumption
# Pre-trend Analysis
# Filter out post-treatment data to focus on pre-treatment trends
pre_treatment_data <- subset(new_data, post_treat == 0)

# Fit the regression model using pre-treatment data
model <- lm(lemp ~ year * treat + lpop, data = pre_treatment_data)

# Print the summary of the regression model
summary(model)


#---- FITTING THE DID REGRESSION MODEL ----#

# Transforming dataset into panel data format
panel_data <- pdata.frame(new_data, index = c('la_id', 'year'))

# Fit the DiD model
did_model <- plm(lemp ~ treat * post_treat + lpop, 
                 data = panel_data, 
                 effect = "twoways", 
                 model = "within")

# Print the summary of the DiD model
summary(did_model)

# Calculate robust standard errors to account for common issues in panel data (heteroscedasticity and autocorrelation)
robust_se <- vcovHC(did_model, method = "arellano", type = "HC1")

# Summary of the DiD model with robust standard errors
coeftest(did_model, vcov = robust_se)
