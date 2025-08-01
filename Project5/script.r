# Load necessary libraries
library(readxl)
library(lmtest)
library(car)
library(sandwich)

# Define file path
file_path <- "C:\Users\risha\OneDrive\Desktop\codes\R\Project5\ecotrix (a).xlsx"

# Load the data (Ensure correct sheet name)
data <- read_excel(file_path, sheet = "Sheet1")

# Check column names
print(colnames(data))

# Rename columns based on actual names
colnames(data) <- c("FLOW_RBI_FOREX", "Unnamed", "FDI", "Government_Borrowing")

# Remove unnamed or unnecessary columns
data <- data[, c("FLOW_RBI_FOREX", "FDI", "Government_Borrowing")]

# Convert variables to numeric, handling non-numeric issues
data$FLOW_RBI_FOREX <- as.numeric(gsub(",", "", data$FLOW_RBI_FOREX))
data$FDI <- as.numeric(gsub(",", "", data$FDI))
data$Government_Borrowing <- as.numeric(gsub(",", "", data$Government_Borrowing))

# Remove rows with NA values
data <- na.omit(data)

# Fit the regression model
model <- lm(FLOW_RBI_FOREX ~ Government_Borrowing + FDI, data = data)

# Display summary of the model
summary(model)
