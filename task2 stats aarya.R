# Install the required packages
install.packages("tidyverse")  # For data manipulation and visualization
install.packages("ggplot2")    # For data visualization (if not included in tidyverse)
install.packages("corrplot")   # For correlation matrix visualization
install.packages("lmtest")     # For hypothesis testing and model diagnostics

# Load the required libraries
library(tidyverse)  # Contains ggplot2, dplyr, and other packages for data analysis
library(ggplot2)    # For creating visualizations
library(corrplot)   # For correlation matrix visualization
library(lmtest)     # For statistical tests and model diagnostics
library(readxl)


# Read the Excel file
concrete_data <- read_excel("/Users/vishnug/Desktop/desktop 1/desktop1/other people assignment/sherrif friend/stats/task 2/concrete compressive strength.xlsx")

head(concrete_data)

# Check the structure of the dataset
str(concrete_data)

# Summary statistics
summary(concrete_data)

colnames(concrete_data)

# Clean up column names by removing special characters and spaces
colnames(concrete_data) <- gsub("[[:space:][:punct:]]+", "_", colnames(concrete_data))

# Check the cleaned column names
colnames(concrete_data)

# Rename columns manually for clarity
colnames(concrete_data) <- c("cement", "blast_furnace_slag", "fly_ash", "water", "superplasticizer", 
                             "coarse_aggregate", "fine_aggregate", "age", "concrete_category", 
                             "contains_fly_ash", "compressive_strength")

# Check the updated column names
colnames(concrete_data)











# Cement Distribution
ggplot(concrete_data, aes(x = cement)) + 
  geom_histogram(binwidth = 10, fill = 'blue', color = 'black') + 
  theme_minimal() + 
  labs(title = "Cement Distribution")

# Water Distribution
ggplot(concrete_data, aes(x = water)) + 
  geom_histogram(binwidth = 10, fill = 'green', color = 'black') + 
  theme_minimal() + 
  labs(title = "Water Distribution")

# Fine Aggregate Distribution
ggplot(concrete_data, aes(x = fine_aggregate)) + 
  geom_histogram(binwidth = 10, fill = 'red', color = 'black') + 
  theme_minimal() + 
  labs(title = "Fine Aggregate Distribution")

# Compressive Strength Distribution
ggplot(concrete_data, aes(x = compressive_strength)) + 
  geom_histogram(binwidth = 5, fill = 'purple', color = 'black') + 
  theme_minimal() + 
  labs(title = "Compressive Strength Distribution")



# Scatterplot matrix to visualize relationships between variables
pairs(concrete_data[, c("cement", "water", "fine_aggregate", "compressive_strength")], 
      main = "Scatterplot Matrix of Concrete Mixes")



# Compute the correlation matrix
cor_matrix <- cor(concrete_data[, c("cement", "blast_furnace_slag", "fly_ash", "water", 
                                    "superplasticizer", "coarse_aggregate", "fine_aggregate", "age")])

# Visualize the correlation matrix
corrplot(cor_matrix, method = "circle", type = "upper", tl.cex = 0.7, tl.col = "black", addCoef.col = "black")

# Check correlation between compressive strength and other variables
cor(concrete_data$compressive_strength, concrete_data$cement)
cor(concrete_data$compressive_strength, concrete_data$water)
cor(concrete_data$compressive_strength, concrete_data$fine_aggregate)




# Build a linear regression model (simple model with cement, water, and fine aggregate)
linear_model <- lm(compressive_strength ~ cement + water + fine_aggregate, data = concrete_data)

# Summary of the linear model
summary(linear_model)

# Plot diagnostics for the linear model
par(mfrow = c(2, 2))  # Display 4 diagnostic plots
plot(linear_model)



# Build a multiple regression model with all available factors
multi_model <- lm(compressive_strength ~ cement + water + fine_aggregate + blast_furnace_slag + 
                    fly_ash + superplasticizer + coarse_aggregate + age, data = concrete_data)

# Summary of the multiple regression model
summary(multi_model)

# Plot diagnostics for the multiple regression model
par(mfrow = c(2, 2))  # Display 4 diagnostic plots
plot(multi_model)





# Create a new variable for cement content: High vs Low
median_cement <- median(concrete_data$cement)
concrete_data$cement_level <- ifelse(concrete_data$cement > median_cement, "High", "Low")

# Perform a t-test comparing compressive strength between high and low cement content
t_test_result <- t.test(compressive_strength ~ cement_level, data = concrete_data)
print(t_test_result)


# Perform a correlation test between fine aggregate (sand) and compressive strength
cor_test_sand <- cor.test(concrete_data$fine_aggregate, concrete_data$compressive_strength)
print(cor_test_sand)



# Example report generation
cat("Exploratory Data Analysis (EDA) Results:\n")
cat("Descriptive statistics for variables show:\n")
cat(" - Cement ranges from X to Y\n")
cat(" - Compressive strength ranges from A to B\n")

cat("\nCorrelation Analysis:\n")
cat(" - The strongest correlation with compressive strength is observed between cement and compressive strength with a correlation of X\n")

cat("\nRegression Analysis:\n")
cat(" - The linear regression model suggests that cement and fine aggregate are significant predictors of compressive strength.\n")

cat("\nHypothesis Testing:\n")
cat(" - Hypothesis 1: A t-test showed a significant difference between high and low cement content (p-value < 0.05)\n")
cat(" - Hypothesis 2: The correlation test between fine aggregate and compressive strength is significant (p-value < 0.05)\n")

