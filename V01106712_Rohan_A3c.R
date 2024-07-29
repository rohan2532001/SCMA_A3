# Install and load necessary libraries
install.packages("AER")
library(AER)

# Load the data
data <- read.csv("C:\\Users\\HP\\Desktop\\NSSO68.csv")

# View the first few rows and check the structure of the data
head(data)
str(data)

# Replace 'dependent_variable' and 'independent_variable_1', 'independent_variable_2', etc.
# with actual column names from your dataset.

# Assuming the dependent variable is 'dependent_variable' and the independent variables are 'independent_variable_1', 'independent_variable_2', ...
# Ensure all variables are correctly named and are numeric

# Check if the dependent variable is a factor and convert it to numeric
if (is.factor(data$dependent_variable)) {
  data$dependent_variable <- as.numeric(as.character(data$dependent_variable))
}

# Repeat this check and conversion for all independent variables if necessary
# Example:
# if (is.factor(data$independent_variable_1)) {
#   data$independent_variable_1 <- as.numeric(as.character(data$independent_variable_1))
# }

# Fit a Tobit model
tobit_model <- tobit(dependent_variable ~ independent_variable_1 + independent_variable_2 + independent_variable_3, data = data, left = 0)

# Summary of the model
summary(tobit_model)

# Extracting coefficients
coef(tobit_model)

# Marginal effects
install.packages("margins")
library(margins)
margins(tobit_model)
