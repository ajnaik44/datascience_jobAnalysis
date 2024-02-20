library(readr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(wordcloud)
library(tm)
install.packages("labeling")
library(labeling)
install.packages("farver")
library(farver)
# Function to check for null values
check_null_values <- function(data) {
  null_values <- sum(is.na(data))
  cat("Number of null values:", null_values, "\n")
}

# Function to identify and remove duplicate records
remove_duplicates <- function(data) {
  num_duplicates <- sum(duplicated(data))
  cat("Number of duplicate records:", num_duplicates, "\n")
  clean_data <- unique(data)
  cat("Dimensions of cleaned data:", dim(clean_data), "\n")
  return(clean_data)
}

# Function to check if salary values are numeric
check_numeric_salaries <- function(data) {
  invalid_salaries <- sum(!is.numeric(data$salary))
  cat("Number of records with invalid salary values:", invalid_salaries, "\n")
  return(data)
}

# Function to create and save pie charts for categorical variables
create_and_save_pie_charts <- function(data) {
  # Create and save pie chart for 'job_category'
  category_counts <- table(data$job_category)
  pie(category_counts, main = "Pie Chart of job_category")
  filename <- "job_category_pie_chart.png"
  png(filename)
  dev.off()
  
  # Create and save pie chart for 'salary_currency'
  category_counts <- table(data$salary_currency)
  pie(category_counts, main = "Pie Chart of salary currency")
  filename <- "salary_currency_pie_chart.png"
  png(filename)
  dev.off()
  
  
  # Create and save pie chart for 'experience_level'
  category_counts <- table(data$experience_level)
  pie(category_counts, main = "Pie Chart of Experience level")
  filename <- "experience_level_pie_chart.png"
  png(filename)
  dev.off()
  
  # Create and save pie chart for 'employment_type'
  category_counts <- table(data$employment_type)
  pie(category_counts, main = "Pie Chart of Employment type")
  filename <- "employment_type_pie_chart.png"
  png(filename)
  dev.off()
  
  # Create and save pie chart for 'work_setting'
  category_counts <- table(data$work_setting)
  pie(category_counts, main = "Pie Chart of work setting")
  filename <- "work_setting_pie_chart.png"
  png(filename)
  dev.off()
  
  
  # Create and save pie chart for 'company_size'
  category_counts <- table(data$company_size)
  pie(category_counts, main = "Pie Chart of company_size")
  filename <- "company_size_pie_chart.png"
  png(filename)
  dev.off()
}

# Main method
main <- function() {
  # Load required libraries
  library(readr)
  
  # Read the CSV file into R
  data <- read.csv("jobs_in_data.csv", stringsAsFactors = FALSE)
  
  # Step 1: Check for null values
  check_null_values(data)
  
  # Step 2: Remove duplicate records
  clean_data <- remove_duplicates(data)
  
  # Step 3: Check if salary values are numeric
  clean_data <- check_numeric_salaries(clean_data)
  
  # Step 4: Create and save pie charts for categorical variables
  create_and_save_pie_charts(clean_data)
  
  
  summary_stats <- summary(clean_data$salary)
  cat("Summary Statistics for Salary:\n")
  print(summary_stats)
  
  
  # Frequency counts for categorical variables
  job_title_counts <- table(clean_data$job_title)
  cat("Frequency Counts for Job Titles:\n")
  print(job_title_counts)
  
  
  # Step 4: Correlation Analysis
  # Compute correlation matrix
  
  
  
  # Define the level mapping
  level_mapping <- c("Entry-level" = 0, "Mid-level" = 1, "Senior" = 2, "Executive" = 3)
  
  # Convert 'experience_level' using the mapping
  clean_data$experience_level_numeric <- level_mapping[clean_data$experience_level]
  
  # Define the mapping for company size categories
  level_mapping_company <- c("L" = 0, "M" = 1, "S" = 2)
  
  # Convert 'company_size' using the mapping
  clean_data$company_size_numeric <- level_mapping_company[clean_data$company_size]
  
  
  
  
  # Check the converted values
  head(clean_data$company_size_numeric)
  
  # Now calculate the correlation
  correlation_matrix <- cor(clean_data[, c("salary", "experience_level_numeric","company_size_numeric")], use = "complete.obs")
  
  # Plot correlation matrix
 print(correlation_matrix)
 
 corrplot(
   correlation_matrix,
   method = "color",
   type = "upper",
   order = "hclust",
   tl.col = "black"
 )
 
 
  
  # Step 5: Salary Distribution
  # Histogram of salary distribution
  ggplot(clean_data, aes(x = salary)) +
    geom_histogram(binwidth = 1000, fill = "skyblue", color = "black") +
    labs(title = "Salary Distribution", x = "Salary", y = "Frequency")
  
  # Boxplot of salary distribution by job category
  ggplot(clean_data, aes(x = job_category, y = salary)) +
    geom_boxplot(fill = "skyblue", color = "black") +
    labs(title = "Salary Distribution by Job Category", x = "Job Category", y = "Salary")
  
  
}

# Call the main method
main()
