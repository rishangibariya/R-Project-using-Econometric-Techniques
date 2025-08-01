# Load necessary libraries
library(readxl)
library(lmtest)
library(car)
library(sandwich)

# Define the file path to your dataset
file_path <- "C:\Users\risha\OneDrive\Desktop\codes\R\Project4\ECOTRIX.xlsx"

# Load the data
data <- read_excel(file_path, sheet = "Sheet1")

# Rename columns for simplicity
colnames(data) <- c("Import", "FDI", "Forex_Reserves")

# Convert variables to numeric if needed
data$Import <- as.numeric(data$Import)
data$FDI <- as.numeric(data$FDI)
data$Forex_Reserves <- as.numeric(data$Forex_Reserves)

# Fit the regression model
model <- lm(Forex_Reserves ~ Import + FDI, data = data)

# Display summary of the model
summary(model)

# Plot the regression line for Import (one predictor at a time for visualization)
plot(data$Import, data$Forex_Reserves, main = "Regression of Forex Reserves on Import",
     xlab = "Import", ylab = "Forex Reserves")
abline(lm(Forex_Reserves ~ Import, data = data), col = "blue")

# Plot residuals
plot(model$residuals, main = "Residuals", ylab = "Residuals", xlab = "Index")
abline(h = 0, col = "red", lty = 2)

# Test for heteroskedasticity using Breusch-Pagan test
bptest(model)

# Test for autocorrelation using Durbin-Watson test
dwtest(model)

# Perform Ramsey RESET test for model specification
resettest(model, power = 3)