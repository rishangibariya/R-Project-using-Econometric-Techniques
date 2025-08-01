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
colnames(data) <- c("NEER_Import", "FDI", "Forex_Reserves")

# Convert variables to numeric if needed
data$NEER_Import <- as.numeric(data$NEER_Import)
data$FDI <- as.numeric(data$FDI)
data$Forex_Reserves <- as.numeric(data$Forex_Reserves)

# Print first few rows to check data
print(head(data))

# Identify problematic values
print(sum(data$NEER_Import <= 0, na.rm = TRUE))
print(sum(data$FDI <= 0, na.rm = TRUE))  # Problematic column
print(sum(data$Forex_Reserves <= 0, na.rm = TRUE))

# Apply log transformation (Handling negative FDI)
data$Log_NEER_Import <- log(data$NEER_Import + 1)
data$Log_FDI <- log(abs(data$FDI) + 1)  # Fix for negative FDI values
data$Log_Forex_Reserves <- log(data$Forex_Reserves + 1)

# Fit the regression model with log-transformed variables
model <- lm(Log_Forex_Reserves ~ Log_NEER_Import + Log_FDI, data = data)

# Display summary of the model
summary(model)

# Plot the regression line for log-transformed NEER Import
plot(data$Log_NEER_Import, data$Log_Forex_Reserves, main = "Regression of Log Forex Reserves on Log NEER Import",
     xlab = "Log NEER Import", ylab = "Log Forex Reserves")
abline(lm(Log_Forex_Reserves ~ Log_NEER_Import, data = data), col = "blue")

# Plot residuals
plot(model$residuals, main = "Residuals", ylab = "Residuals", xlab = "Index")
abline(h = 0, col = "red", lty = 2)

# Test for heteroskedasticity using Breusch-Pagan test
bptest(model)

# Test for autocorrelation using Durbin-Watson test
dwtest(model)

# Perform Ramsey RESET test for model specification
resettest(model, power = 3)
