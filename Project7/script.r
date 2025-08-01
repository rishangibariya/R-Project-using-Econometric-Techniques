# Load necessary libraries
library(readxl)
library(lmtest)
library(car)
library(sandwich)

# Define the file path to your dataset
file_path <- "C:\Users\risha\OneDrive\Desktop\codes\R\Project7\ecotrix.xlsx"

# Load the data
data <- read_excel(file_path, sheet = "Sheet3")

# Rename columns for simplicity
colnames(data) <- c("Crude_Oil_Prices", "FDI", "Forex_Reserves")

# Convert variables to numeric if needed
data$Crude_Oil_Prices <- as.numeric(data$Crude_Oil_Prices)
data$FDI <- as.numeric(data$FDI)
data$Forex_Reserves <- as.numeric(data$Forex_Reserves)

# Apply log transformation to variables
data$Log_Crude_Oil_Prices <- log(data$Crude_Oil_Prices)
data$Log_FDI <- log(data$FDI)
data$Log_Forex_Reserves <- log(data$Forex_Reserves)

# Fit the regression model with log-transformed variables
model <- lm(Log_Forex_Reserves ~ Log_Crude_Oil_Prices + Log_FDI, data = data)

# Display summary of the model
summary(model)

# Plot the regression line for log-transformed Crude Oil Prices (one predictor at a time for visualization)
plot(data$Log_Crude_Oil_Prices, data$Log_Forex_Reserves, main = "Regression of Log Forex Reserves on Log Crude Oil Prices",
     xlab = "Log Crude Oil Prices", ylab = "Log Forex Reserves")
abline(lm(Log_Forex_Reserves ~ Log_Crude_Oil_Prices, data = data), col = "blue")

# Plot residuals
plot(model$residuals, main = "Residuals", ylab = "Residuals", xlab = "Index")
abline(h = 0, col = "red", lty = 2)

# Test for heteroskedasticity using Breusch-Pagan test
bptest(model)

# Test for autocorrelation using Durbin-Watson test
dwtest(model)

# Perform Ramsey RESET test for model specification
resettest(model, power = 3)