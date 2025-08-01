# Load necessary libraries
library(readxl)
library(lmtest)
library(car)
library(sandwich)

# Define the file path to your dataset
file_path <- "C:\Users\risha\OneDrive\Desktop\codes\R\Project3\ECOTRIX.xlsx"

# Load the data
data <- read_excel(file_path, sheet = "Sheet3")

# Rename columns for simplicity
colnames(data) <- c("Real_Effective_Exchange_Rate", "FDI", "Forex_Reserves")

# Convert variables to numeric if needed
data$Real_Effective_Exchange_Rate <- as.numeric(data$Real_Effective_Exchange_Rate)
data$FDI <- as.numeric(data$FDI)
data$Forex_Reserves <- as.numeric(data$Forex_Reserves)

# Fit the regression model
model <- lm(Forex_Reserves ~ Real_Effective_Exchange_Rate + FDI, data = data)

# Display summary of the model
summary(model)

# Plot the regression line for Real Effective Exchange Rate (one predictor at a time for visualization)
plot(data$Real_Effective_Exchange_Rate, data$Forex_Reserves, main = "Regression of Forex Reserves on Real Effective Exchange Rate",
     xlab = "Real Effective Exchange Rate", ylab = "Forex Reserves")
abline(lm(Forex_Reserves ~ Real_Effective_Exchange_Rate, data = data), col = "blue")

# Plot residuals
plot(model$residuals, main = "Residuals", ylab = "Residuals", xlab = "Index")
abline(h = 0, col = "red", lty = 2)

# Test for heteroskedasticity using Breusch-Pagan test
bptest(model)

# Test for autocorrelation using Durbin-Watson test
dwtest(model)

# Perform Ramsey RESET test for model specification
resettest(model, power = 3)