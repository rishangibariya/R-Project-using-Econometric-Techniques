# Load necessary libraries
# install.packages("lmtest")  # Uncomment to install if needed
# install.packages("car")     # Uncomment to install if needed
# install.packages("readxl")  # Uncomment to install if needed
library(lmtest)
library(car)
library(readxl)

# 1. Load your dataset from Sheet 1 of the Excel file
data <- read_excel("C:\Users\risha\OneDrive\Desktop\codes\R\Project9\ecotrix n.xlsx", sheet = 1)

# 2. Check the column names to confirm the variable names
names(data)

# 3. Estimating the Regression Model (Y = alpha + beta1 * neer_export + Ui)
# Replace 'Y' and 'neer_export' with your actual variable names if they differ
model <- lm(receipts ~ neer_export, data = data)

# 4. Output of the Regression Model
summary(model)

# 5. Plotting the Regression Line
# Scatter plot of the data with regression line
plot(data$neer_export, data$Y, main = "Scatter plot with Regression Line", 
     xlab = "Neer Export (neer_export)", ylab = "RECEIPTS", pch = 16, col = "blue")
abline(model, col = "red")

# 6. Plotting the Residuals
residuals <- residuals(model)
plot(data$neer_export, residuals, main = "Residuals Plot", 
     xlab = "Neer Export (neer_export)", ylab = "Residuals", pch = 16, col = "green")
abline(h = 0, col = "black")

# 7. Diagnostic Tests for Heteroskedasticity
# Breusch-Pagan test for heteroskedasticity
bptest(model)

# 8. Diagnostic Test for Autocorrelation
# Durbin-Watson test for autocorrelation
durbinWatsonTest(model)

# 9. Perform Ramsey RESET Test for Specification (Including Cubic Functional Form)
resettest(model, power = 3)  # 'power = 3' to test the cubic form

# 10. Additional diagnostics: Check residual plots for any remaining patterns
# Residuals vs Fitted values plot
plot(fitted(model), residuals, main = "Residuals vs Fitted", 
     xlab = "Fitted Values", ylab = "Residuals", pch = 16, col = "purple")
abline(h = 0, col = "black")