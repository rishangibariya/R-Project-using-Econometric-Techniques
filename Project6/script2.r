# Load necessary libraries
library(readxl)
library(lmtest)
library(car)
library(sandwich)

# Define the file path to your dataset
file_path <- "C:\Users\risha\OneDrive\Desktop\codes\R\Project6\ECOTRIX.xlsx"

# Load the data
data <- read_excel(file_path, sheet = "Sheet1")

# Rename columns for simplicity
colnames(data) <- c("NEER_trade", "FDI", "Forex_Reserves")

# Convert variables to numeric if needed
data$NEER_trade <- as.numeric(data$NEER_trade)
data$FDI <- as.numeric(data$FDI)
data$Forex_Reserves <- as.numeric(data$Forex_Reserves)

# Fit the regression model
model <- lm(Forex_Reserves ~ NEER_trade + FDI, data = data)

# Display summary of the model
summary(model)

# Plot the regression line for NEER trade (one predictor at a time for visualization)
plot(data$NEER_trade, data$Forex_Reserves, main = "Regression of Forex Reserves on NEER trade",
     xlab = "NEER Import", ylab = "Forex Reserves")
abline(lm(Forex_Reserves ~ NEER_trade, data = data), col = "blue")

# Plot residuals
plot(model$residuals, main = "Residuals", ylab = "Residuals", xlab = "Index")
abline(h = 0, col = "red", lty = 2)

# Test for heteroskedasticity using Breusch-Pagan test
bptest(model)

# Test for autocorrelation using Durbin-Watson test
dwtest(model)

# Perform Ramsey RESET test for model specification
resettest(model, power = 3)