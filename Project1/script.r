# Load necessary libraries
library(readxl)
library(lmtest)
library(car)
library(sandwich)


# Define the file path to your dataset
file_path <- "C:\Users\risha\OneDrive\Desktop\codes\R\Project1\ecotrix.xlsx"

# Load the data
data <- read_excel(file_path, sheet = "Sheet3")

# Rename columns for simplicity
colnames(data) <- c("Receipts", "Political_Stability", "Forex_Reserves")

# Convert variables to numeric if needed
data$Receipts <- as.numeric(data$Receipts)
data$Political_Stability <- as.numeric(data$Political_Stability)
data$Forex_Reserves <- as.numeric(data$Forex_Reserves)

# Fit the regression model
model <- lm(Forex_Reserves ~ Receipts + Political_Stability, data = data)

# Display summary of the model
summary(model)

# Plot the regression line (only makes sense for one predictor at a time, so choose one, e.g., Receipts)
plot(data$Receipts, data$Forex_Reserves, main = "Regression of Forex Reserves on Receipts",
     xlab = "Receipts", ylab = "Forex Reserves")
abline(lm(Forex_Reserves ~ Receipts, data = data), col = "blue")

# Plot residuals
plot(model$residuals, main = "Residuals", ylab = "Residuals", xlab = "Index")
abline(h = 0, col = "red", lty = 2)

# Test for heteroskedasticity using Breusch-Pagan test
bptest(model)

# Test for autocorrelation using Durbin-Watson test
dwtest(model)

# Perform Ramsey RESET test for model specification
resettest(model, power = 3)
