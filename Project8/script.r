# Load necessary libraries
library(readxl)
library(lmtest)
library(car)
library(sandwich)

# Define the file path to your dataset
file_path <- "C:\Users\risha\OneDrive\Desktop\codes\R\Project8\ECOTRIX.xlsx"
# Load the data
data <- read_excel(file_path, sheet = "Sheet1")

# Rename columns for simplicity
colnames(data) <- c("Real_Effective_Exchange_Rate", "FDI", "Forex_Reserves")

# Convert variables to numeric if needed
data$Real_Effective_Exchange_Rate <- as.numeric(data$Real_Effective_Exchange_Rate)
data$FDI <- as.numeric(data$FDI)
data$Forex_Reserves <- as.numeric(data$Forex_Reserves)

# Apply log transformation to variables
data$log_Real_Effective_Exchange_Rate <- log(data$Real_Effective_Exchange_Rate)
data$log_FDI <- log(data$FDI)
data$log_Forex_Reserves <- log(data$Forex_Reserves)

# Fit the regression model with log-transformed variables
model <- lm(log_Forex_Reserves ~ log_Real_Effective_Exchange_Rate + log_FDI, data = data)

# Display summary of the model
summary(model)

# Plot the regression line for log-transformed Real Effective Exchange Rate
plot(data$log_Real_Effective_Exchange_Rate, data$log_Forex_Reserves, 
     main = "Regression of Log Forex Reserves on Log Real Effective Exchange Rate",
     xlab = "Log Real Effective Exchange Rate", ylab = "Log Forex Reserves")
abline(lm(log_Forex_Reserves ~ log_Real_Effective_Exchange_Rate, data = data), col = "blue")

# Plot residuals
plot(model$residuals, main = "Residuals", ylab = "Residuals", xlab = "Index")
abline(h = 0, col = "red", lty = 2)

# Test for heteroskedasticity using Breusch-Pagan test
bptest(model)

# Test for autocorrelation using Durbin-Watson test
dwtest(model)

# Perform Ramsey RESET test for model specification
resettest(model, power = 3)