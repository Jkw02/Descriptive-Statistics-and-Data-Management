
# Set working directory
setwd("")


# Install packages 
install.packages("haven")
install.packages("tidyverse")
install.packages("tableone")


# Load packages
library(haven)      # for reading Stata dataset files
library(tidyverse)  # for manipulating data (includes multiple packages)
library(tableone)   # for creating summary statistics tables


# Load dataset 0
data0 <- read_dta("Dataset_0.dta")

# Print the name and label of all variables
for (var in names(data0)) {
  label <- attr(data0[[var]], "label")
  cat(var, ":", label, "\n")
}

# Loop through the variables in the dataset and summarise the data
summary(data0)


# Load dataset 1
data1 <- read_dta("Dataset_1.dta")

# Print the name and label of all variables
## Note: this is advanced code as it's a function not built into R unlike Stata
for (var in names(data1)) {
  label <- attr(data1[[var]], "label")
  cat(var, ":", label, "\n")
}

# Loop through the variables in the dataset and summarize the data
summary(data1)



# Append/join the datasets together
data <- rbind(data0, data1)
# Sort by ID
data <- data %>% 
  arrange(id)


# Reshape/pivot the dataset from long to wide
data_wide <- data %>%
  pivot_wider(id_cols = c(id, everything()), 
              names_from = time, 
              values_from = bmi, 
              names_prefix = "bmi")


# Summarise bmi0 and bmi1
summary(data_wide$bmi0)
summary(data_wide$bmi1)


# Check for errors (BMI too low)
## ifelse(condition, value if true, value if false)
## i.e. replace with NA if BMI too low
data_wide <- data_wide %>%
  mutate(
    bmi0 = ifelse(bmi0 < 10, NA, bmi0),
    bmi1 = ifelse(bmi1 < 10, NA, bmi1)
  )


# Create "BMI Change" variable
data_wide <- data_wide %>%
  mutate(bmi_change = bmi1 - bmi0)


# Summarise bmi0 and bmi1 (double check)
summary(data_wide$bmi0)
summary(data_wide$bmi1)


# Summarise data BMI variables by intervention
# Bariatric == 0 (Lifestyle Intervention)
data_wide_b0 <- data_wide %>%
  filter(bariatric == 0)

## Summary statistics
summary(data_wide_b0$bmi0)
summary(data_wide_b0$bmi_change)
summary(data_wide_b0$age)

## Table of summary statistics
vars_b0 <- c("sex", "education", "CVD", "diuretics")
tab1_b0 <- CreateTableOne(vars = vars_b0, data = data_wide_b0, factorVars = vars_b0)
print(tab1_b0, showAllLevels = TRUE)


# Bariatric == 1 (Bariatric Surgery)
data_wide_b1 <- data_wide %>%
  filter(bariatric == 1)

## Summary statistics
summary(data_wide_b1$bmi1)
summary(data_wide_b1$bmi_change)
summary(data_wide_b1$age)

## Table of summary statistics
vars_b1 <- c("sex", "education", "CVD", "diuretics")
tab1_b1 <- CreateTableOne(vars = vars_b1, data = data_wide_b1, factorVars = vars_b1)
print(tab1_b1, showAllLevels = TRUE)


# Plot histogram
# Assign Labels for "bariatric"
custom_labels <- c("0" = "Lifestyle Intervention", "1" = "Bariatric Surgery")

# Plot
## Error indicates that the NA values have been ignored
ggplot(data_wide, aes(x = bmi_change)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 0.5, fill = "blue", color = "black") + # to get percentages
  facet_wrap(~ bariatric, ncol = 1, labeller = labeller(bariatric = custom_labels)) +  # faceting by bariatric, one column
  labs(title = "Histogram of BMI Change by Bariatric Status", x = "BMI Change (kg/m^2)", y = "Density")
