---
title: "E-Business Loan Status Prediction"
output: 
  html_document:
    keep_md: true
---

# Load libraries


``` r
library(tidyverse)
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.1.4     ✔ readr     2.1.5
## ✔ forcats   1.0.0     ✔ stringr   1.5.1
## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
## ✔ purrr     1.0.2     
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

``` r
library(caret)
```

```
## Lade nötiges Paket: lattice
## 
## Attache Paket: 'caret'
## 
## Das folgende Objekt ist maskiert 'package:purrr':
## 
##     lift
```

``` r
library(randomForest)
```

```
## randomForest 4.7-1.1
## Type rfNews() to see new features/changes/bug fixes.
## 
## Attache Paket: 'randomForest'
## 
## Das folgende Objekt ist maskiert 'package:dplyr':
## 
##     combine
## 
## Das folgende Objekt ist maskiert 'package:ggplot2':
## 
##     margin
```

``` r
library(e1071)
library(pROC)
```

```
## Type 'citation("pROC")' for a citation.
## 
## Attache Paket: 'pROC'
## 
## Die folgenden Objekte sind maskiert von 'package:stats':
## 
##     cov, smooth, var
```

``` r
library(rpart)
library(ggplot2)
```

# Load & Transform the data


``` r
# Load the data
rawData <- read.csv("loan_data.csv")

# Transform the data
rawData <- rawData %>%
  select(-Loan_ID) %>%
  mutate(Married = Married == "Yes",
         Education = Education == "Graduate",
         Self_Employed = Self_Employed == "Yes",
         Gender = as.factor(Gender),
         Dependents = as.factor(Dependents),
         Property_Area = as.factor(Property_Area),
         Loan_Status = factor(ifelse(Loan_Status == "Y", "Yes", "No")))
```

# Feature Engineering


``` r
rawData <- rawData %>%
  mutate(Total_Income = ApplicantIncome + CoapplicantIncome,
         Income_to_Loan = Total_Income / LoanAmount)

# Drop all missing values just for testing TODO
rawData <- drop_na(rawData)

# Check for null values and handle them (if any)
null_counts <- colSums(is.na(rawData))
print(null_counts)
```

```
##            Gender           Married        Dependents         Education 
##                 0                 0                 0                 0 
##     Self_Employed   ApplicantIncome CoapplicantIncome        LoanAmount 
##                 0                 0                 0                 0 
##  Loan_Amount_Term    Credit_History     Property_Area       Loan_Status 
##                 0                 0                 0                 0 
##      Total_Income    Income_to_Loan 
##                 0                 0
```

# Initial Data sighting


``` r
# Plot 1: Distribution of Loan Status with customized colors
ggplot(rawData, aes(x = Loan_Status, fill = Loan_Status)) +
  geom_bar() +
  scale_fill_manual(values = c("Yes" = "#90EE90", "No" = "#FFB6C1")) +
  ggtitle("Distribution of Loan Status") +
  theme_minimal()
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

``` r
# Plot 2: Distribution of Gender with customized RGB colors
ggplot(rawData, aes(x = Gender, fill = Gender)) +
  geom_bar() +
  scale_fill_manual(values = c("Male" = "#89CFF0", "Female" = "#FFB6C1")) +
  ggtitle("Distribution of Gender") +
  theme_minimal()
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-4-2.png)<!-- -->

``` r
# Plot 3: Average Loan Amount by Property Area
avg_loan_amount <- rawData %>%
  group_by(Property_Area) %>%
  summarize(LoanAmount = mean(LoanAmount))

ggplot(avg_loan_amount, aes(x = Property_Area, y = LoanAmount)) +
  geom_bar(stat = "identity") +
  ggtitle("Average Loan Amount by Property Area") +
  ylab("Average Loan Amount") +
  theme_minimal()
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-4-3.png)<!-- -->

``` r
# Plot 4: Distribution of Loan Status by Education
ggplot(rawData, aes(x = Loan_Status, fill = Education)) +
  geom_bar(position = "dodge") +
  ggtitle("Distribution of Loan Status by Education") +
  theme_minimal()
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-4-4.png)<!-- -->

``` r
# Plot 5: Distribution of Loan Status by Self Employed status
ggplot(rawData, aes(x = Loan_Status, fill = Self_Employed)) +
  geom_bar(position = "dodge") +
  ggtitle("Distribution of Loan Status by Self Employed Status") +
  theme_minimal()
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-4-5.png)<!-- -->


# EDA

``` r
#Average Loan Amounts Requested and Granted

# Plot 1: Average Loan Amount requested by Gender
# Calculate the average loan amount by gender and remove missing values
avg_loan_amount_gender <- rawData %>%
  filter(!is.na(Gender) & Gender != "") %>%
  group_by(Gender) %>%
  summarize(LoanAmount = mean(LoanAmount, na.rm = TRUE))

# Create the bar plot
ggplot(avg_loan_amount_gender, aes(x = Gender, y = LoanAmount)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(LoanAmount, 2)), vjust = -0.5) +  # Add labels above bars
  ggtitle("Average Loan Amount requested by Gender") +
  ylab("Average Loan Amount") +
  theme_minimal()
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

``` r
# Plot 2: Average Loan Amount  granted granted by Gender
# Calculate the average loan amount by gender and remove missing values
avg_loan_amount_gender <- rawData %>%
  filter(Loan_Status == "Yes" & !is.na(Gender) & Gender != "") %>%
  group_by(Gender) %>%
  summarize(LoanAmount = mean(LoanAmount, na.rm = TRUE))

# Create the bar plot
ggplot(avg_loan_amount_gender, aes(x = Gender, y = LoanAmount)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_text(aes(label = round(LoanAmount, 2)), vjust = -0.5) +  # Add labels above bars
  ggtitle("Average Loan Amount granted by Gender") +
  ylab("Average Loan Amount") +
  theme_minimal()
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-5-2.png)<!-- -->

``` r
# Plot 3: Average Loan Amount requested by Property Area
avg_loan_amount <- rawData %>%
  group_by(Property_Area) %>%
  summarize(LoanAmount = mean(LoanAmount))

ggplot(avg_loan_amount, aes(x = Property_Area, y = LoanAmount)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(LoanAmount, 2)), vjust = -0.5) +  # Add labels above bars
  ggtitle("Average Loan Amount requested by Property Area") +
  ylab("Average Loan Amount") +
  theme_minimal()
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-5-3.png)<!-- -->

``` r
# Plot 4: Average Loan Amount granted by Property Area
avg_loan_amount <- rawData %>%
  filter(Loan_Status == "Yes") %>%
  group_by(Property_Area) %>%
  summarize(LoanAmount = mean(LoanAmount))

ggplot(avg_loan_amount, aes(x = Property_Area, y = LoanAmount)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_text(aes(label = round(LoanAmount, 2)), vjust = -0.5) +  # Add labels above bars
  geom_text(aes(label = round(LoanAmount, 2)), vjust = -0.5) + #Add labels above bars
  ggtitle("Average Loan Amount granted by Property Area") +
  ylab("Average Loan Amount") +
  theme_minimal()
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-5-4.png)<!-- -->

``` r
# Plot 5: Average Loan Amount requested by Marriage Status
avg_loan_amount <- rawData %>%
  group_by(Married) %>%
  summarize(LoanAmount = mean(LoanAmount))

ggplot(avg_loan_amount, aes(x = Married, y = LoanAmount)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(LoanAmount, 2)), vjust = -0.5) +  # Add labels above bars
  ggtitle("Average Loan Amount requested by Marriage Status") +
  ylab("Average Loan Amount") +
  theme_minimal()
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-5-5.png)<!-- -->

``` r
# Plot 6: Average Loan Amount granted by Marriage Status
avg_loan_amount <- rawData %>%
  filter(Loan_Status == "Yes") %>%
  group_by(Married) %>%
  summarize(LoanAmount = mean(LoanAmount))

ggplot(avg_loan_amount, aes(x = Married, y = LoanAmount)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_text(aes(label = round(LoanAmount, 2)), vjust = -0.5) +  # Add labels above bars
  geom_text(aes(label = round(LoanAmount, 2)), vjust = -0.5) + #Add labels above bars
  ggtitle("Average Loan Amount granted by Marriage Status") +
  ylab("Average Loan Amount") +
  theme_minimal()
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-5-6.png)<!-- -->

``` r
# Plot 7: Average Loan Amount requested by Education Status
avg_loan_amount <- rawData %>%
  group_by(Education) %>%
  summarize(LoanAmount = mean(LoanAmount))

ggplot(avg_loan_amount, aes(x = Education, y = LoanAmount)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(LoanAmount, 2)), vjust = -0.5) +  # Add labels above bars
  ggtitle("Average Loan Amount requested by Education Status") +
  ylab("Average Loan Amount") +
  theme_minimal()
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-5-7.png)<!-- -->

``` r
# Plot 8: Average Loan Amount granted by Education Status
avg_loan_amount <- rawData %>%
  filter(Loan_Status == "Yes") %>%
  group_by(Education) %>%
  summarize(LoanAmount = mean(LoanAmount))

ggplot(avg_loan_amount, aes(x = Education, y = LoanAmount)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_text(aes(label = round(LoanAmount, 2)), vjust = -0.5) +  # Add labels above bars
  geom_text(aes(label = round(LoanAmount, 2)), vjust = -0.5) + #Add labels above bars
  ggtitle("Average Loan Amount granted by Education Status") +
  ylab("Average Loan Amount") +
  theme_minimal()
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-5-8.png)<!-- -->

``` r
# Plot 9: Average Loan Amount requested by Self_Employed Status
avg_loan_amount <- rawData %>%
  group_by(Self_Employed) %>%
  summarize(LoanAmount = mean(LoanAmount))

ggplot(avg_loan_amount, aes(x = Self_Employed, y = LoanAmount)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(LoanAmount, 2)), vjust = -0.5) +  # Add labels above bars
  ggtitle("Average Loan Amount requested by Self_Employed Status") +
  ylab("Average Loan Amount") +
  theme_minimal()
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-5-9.png)<!-- -->

``` r
# Plot 10: Average Loan Amount granted by Self_Employed Status
avg_loan_amount <- rawData %>%
  filter(Loan_Status == "Yes") %>%
  group_by(Self_Employed) %>%
  summarize(LoanAmount = mean(LoanAmount))

ggplot(avg_loan_amount, aes(x = Self_Employed, y = LoanAmount)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_text(aes(label = round(LoanAmount, 2)), vjust = -0.5) +  # Add labels above bars
  geom_text(aes(label = round(LoanAmount, 2)), vjust = -0.5) + #Add labels above bars
  ggtitle("Average Loan Amount granted by Self_Employed Status") +
  ylab("Average Loan Amount") +
  theme_minimal()
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-5-10.png)<!-- -->

``` r
# Plot 11: Average Loan Amount requested by Number of Dependents
avg_loan_amount <- rawData %>%
  group_by(Dependents) %>%
  summarize(LoanAmount = mean(LoanAmount))

ggplot(avg_loan_amount, aes(x = Dependents, y = LoanAmount)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(LoanAmount, 2)), vjust = -0.5) +  # Add labels above bars
  ggtitle("Average Loan Amount requested by Number of Dependents") +
  ylab("Average Loan Amount") +
  theme_minimal()
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-5-11.png)<!-- -->

``` r
# Plot 12: Average Loan Amount granted by Number of Dependents
avg_loan_amount <- rawData %>%
  filter(Loan_Status == "Yes") %>%
  group_by(Dependents) %>%
  summarize(LoanAmount = mean(LoanAmount))

ggplot(avg_loan_amount, aes(x = Dependents, y = LoanAmount)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_text(aes(label = round(LoanAmount, 2)), vjust = -0.5) +  # Add labels above bars
  geom_text(aes(label = round(LoanAmount, 2)), vjust = -0.5) + #Add labels above bars
  ggtitle("Average Loan Amount granted by Number of Dependents") +
  ylab("Average Loan Amount") +
  theme_minimal()
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-5-12.png)<!-- -->

``` r
# Plot 13: Average loan amount requested by Applicant Income
avg_loan_requested <- rawData %>%
  group_by(ApplicantIncome) %>%
  summarize(LoanAmount = mean(LoanAmount, na.rm = TRUE))

# Plotting average loan amount requested with a trend line
ggplot(avg_loan_requested, aes(x = ApplicantIncome, y = LoanAmount)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  ggtitle("Average Loan Amount Requested by Applicant Income") +
  xlab("Applicant Income") +
  ylab("Average Loan Amount") +
  theme_minimal()
```

```
## `geom_smooth()` using formula = 'y ~ x'
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-5-13.png)<!-- -->

``` r
# Plot 14: Average loan amount granted by Applicant Income (Loan_Status == TRUE)
avg_loan_granted <- rawData %>%
  filter(Loan_Status == "Yes") %>%
  group_by(ApplicantIncome) %>%
  summarize(LoanAmount = mean(LoanAmount, na.rm = TRUE))

# Plotting average loan amount granted with a trend line
ggplot(avg_loan_granted, aes(x = ApplicantIncome, y = LoanAmount)) +
  geom_line(color = "green") +
  geom_point(color = "green") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  ggtitle("Average Loan Amount Granted by Applicant Income") +
  xlab("Applicant Income") +
  ylab("Average Loan Amount") +
  theme_minimal()
```

```
## `geom_smooth()` using formula = 'y ~ x'
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-5-14.png)<!-- -->

``` r
# Plot 15: Average loan amount requested by Coapplicant Income
avg_loan_requested <- rawData %>%
  group_by(CoapplicantIncome) %>%
  summarize(LoanAmount = mean(LoanAmount, na.rm = TRUE))

# Plotting average loan amount requested with a trend line
ggplot(avg_loan_requested, aes(x = CoapplicantIncome, y = LoanAmount)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  ggtitle("Average Loan Amount Requested by Coapplicant Income") +
  xlab("Coapplicant Income") +
  ylab("Average Loan Amount") +
  theme_minimal()
```

```
## `geom_smooth()` using formula = 'y ~ x'
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-5-15.png)<!-- -->

``` r
# Plot 16: Average loan amount granted by Coapplicant Income (Loan_Status == TRUE)
avg_loan_granted <- rawData %>%
  filter(Loan_Status == "Yes") %>%
  group_by(CoapplicantIncome) %>%
  summarize(LoanAmount = mean(LoanAmount, na.rm = TRUE))

# Plotting average loan amount granted with a trend line
ggplot(avg_loan_granted, aes(x = CoapplicantIncome, y = LoanAmount)) +
  geom_line(color = "green") +
  geom_point(color = "green") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  ggtitle("Average Loan Amount Granted by Coapplicant Income") +
  xlab("Coapplicant Income") +
  ylab("Average Loan Amount") +
  theme_minimal()
```

```
## `geom_smooth()` using formula = 'y ~ x'
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-5-16.png)<!-- -->

``` r
# Plot 17: Average loan amount requested by Total_Income
avg_loan_requested <- rawData %>%
  group_by(Total_Income) %>%
  summarize(LoanAmount = mean(LoanAmount, na.rm = TRUE))

# Plotting average loan amount requested with a trend line
ggplot(avg_loan_requested, aes(x = Total_Income, y = LoanAmount)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  ggtitle("Average Loan Amount Requested by Total_Income") +
  xlab("TotalIncome") +
  ylab("Average Loan Amount") +
  theme_minimal()
```

```
## `geom_smooth()` using formula = 'y ~ x'
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-5-17.png)<!-- -->

``` r
# Plot 18: Average loan amount granted by Total_Income (Loan_Status == TRUE)
avg_loan_granted <- rawData %>%
  filter(Loan_Status == "Yes") %>%
  group_by(Total_Income) %>%
  summarize(LoanAmount = mean(LoanAmount, na.rm = TRUE))

# Plotting average loan amount granted with a trend line
ggplot(avg_loan_granted, aes(x = Total_Income, y = LoanAmount)) +
  geom_line(color = "green") +
  geom_point(color = "green") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  ggtitle("Average Loan Amount Granted by Toal_Income") +
  xlab("Total_Income") +
  ylab("Average Loan Amount") +
  theme_minimal()
```

```
## `geom_smooth()` using formula = 'y ~ x'
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-5-18.png)<!-- -->

``` r
# Plot 19: Average loan amount requested by Income_to_Loan
avg_loan_requested <- rawData %>%
  group_by(Income_to_Loan) %>%
  summarize(LoanAmount = mean(LoanAmount, na.rm = TRUE))

# Plotting average loan amount requested with a trend line
ggplot(avg_loan_requested, aes(x = Income_to_Loan, y = LoanAmount)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  ggtitle("Average Loan Amount Requested by Total_Income") +
  xlab("Income_to_Loan") +
  ylab("Average Loan Amount") +
  theme_minimal()
```

```
## `geom_smooth()` using formula = 'y ~ x'
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-5-19.png)<!-- -->

``` r
# Plot 20: Average loan amount granted by Income_to_Loan (Loan_Status == TRUE)
avg_loan_granted <- rawData %>%
  filter(Loan_Status == "Yes") %>%
  group_by(Income_to_Loan) %>%
  summarize(LoanAmount = mean(LoanAmount, na.rm = TRUE))

# Plotting average loan amount granted with a trend line
ggplot(avg_loan_granted, aes(x = Income_to_Loan, y = LoanAmount)) +
  geom_line(color = "green") +
  geom_point(color = "green") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  ggtitle("Average Loan Amount Granted by Income_to_Loan") +
  xlab("Income_to_Loan") +
  ylab("Average Loan Amount") +
  theme_minimal()
```

```
## `geom_smooth()` using formula = 'y ~ x'
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-5-20.png)<!-- -->

``` r
# Plot 21: Average Loan Amount requested by Credit_History
avg_loan_amount <- rawData %>%
  group_by(Credit_History) %>%
  summarize(LoanAmount = mean(LoanAmount))

ggplot(avg_loan_amount, aes(x = Credit_History, y = LoanAmount)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(LoanAmount, 2)), vjust = -0.5) +  # Add labels above bars
  ggtitle("Average Loan Amount requested by Credit_History") +
  ylab("Average Loan Amount") +
  theme_minimal()
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-5-21.png)<!-- -->

``` r
# Plot 22: Average Loan Amount granted by Credit_History
avg_loan_amount <- rawData %>%
  filter(Loan_Status == "Yes") %>%
  group_by(Credit_History) %>%
  summarize(LoanAmount = mean(LoanAmount))

ggplot(avg_loan_amount, aes(x = Credit_History, y = LoanAmount)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_text(aes(label = round(LoanAmount, 2)), vjust = -0.5) +  # Add labels above bars
  geom_text(aes(label = round(LoanAmount, 2)), vjust = -0.5) + #Add labels above bars
  ggtitle("Average Loan Amount granted by Credit_History") +
  ylab("Average Loan Amount") +
  theme_minimal()
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-5-22.png)<!-- -->

``` r
# Plot 23: Average Loan Amount requested by Loan_Amount_Term
avg_loan_amount <- rawData %>%
  group_by(Loan_Amount_Term) %>%
  summarize(LoanAmount = mean(LoanAmount))

ggplot(avg_loan_amount, aes(x = Loan_Amount_Term, y = LoanAmount)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(LoanAmount, 2)), vjust = -0.5) +  # Add labels above bars
  ggtitle("Average Loan Amount requested by Loan_Amount_Term") +
  ylab("Average Loan Amount") +
  theme_minimal()
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-5-23.png)<!-- -->

``` r
# Plot 24: Average Loan Amount granted by Loan_Amount_Term
avg_loan_amount <- rawData %>%
  filter(Loan_Status == "Yes") %>%
  group_by(Loan_Amount_Term) %>%
  summarize(LoanAmount = mean(LoanAmount))

ggplot(avg_loan_amount, aes(x = Loan_Amount_Term, y = LoanAmount)) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  geom_text(aes(label = round(LoanAmount, 2)), vjust = -0.5) +  # Add labels above bars
  geom_text(aes(label = round(LoanAmount, 2)), vjust = -0.5) + #Add labels above bars
  ggtitle("Average Loan Amount granted by Loan_Amount_Term") +
  ylab("Average Loan Amount") +
  theme_minimal()
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-5-24.png)<!-- -->

``` r
# Plot 25: Average Loan Amount requested by Loan_Status
avg_loan_amount <- rawData %>%
  group_by(Loan_Status) %>%
  summarize(LoanAmount = mean(LoanAmount))

ggplot(avg_loan_amount, aes(x = Loan_Status, y = LoanAmount)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(LoanAmount, 2)), vjust = -0.5) +  # Add labels above bars
  ggtitle("Average Loan Amount requested by Loan_Status") +
  ylab("Average Loan Amount") +
  theme_minimal()
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-5-25.png)<!-- -->


``` r
# Function to create percentage plots for categorical variables
create_percentage_plot <- function(data, x_var, plot_title) {
  # Calculate the percentage of loans granted
  percentage_data <- data %>%
    group_by(.data[[x_var]]) %>%
    summarize(Requested = n(),
              Granted = sum(Loan_Status == "Yes")) %>%
    mutate(Percentage = (Granted / Requested) * 100)
  
  ggplot(percentage_data, aes_string(x = x_var, y = "Percentage")) +
    geom_bar(stat = "identity", fill = "blue") +
    geom_text(aes(label = paste0(round(Percentage, 2), "%")), vjust = -0.5) +
    ggtitle(plot_title) +
    ylab("Percentage of Loans Granted") +
    theme_minimal()
}

# Function to group continuous variables into sensible ranges
group_continuous_variable <- function(data, var, breaks, labels) {
  data %>%
    mutate(!!var := cut(.data[[var]], breaks = breaks, labels = labels, include.lowest = TRUE))
}

rawDataForPlot <- data.table::copy(rawData)

# Group ApplicantIncome
rawDataForPlot <- group_continuous_variable(rawDataForPlot, "ApplicantIncome", 
                                     breaks = c(0, 2500, 5000, 7500, 10000, Inf), 
                                     labels = c("0-2500", "2500-5000", "5000-7500", "7500-10000", ">10000"))

# Group CoapplicantIncome
rawDataForPlot <- group_continuous_variable(rawDataForPlot, "CoapplicantIncome", 
                                     breaks = c(0, 1000, 2000, 3000, 4000, Inf), 
                                     labels = c("0-1000", "1000-2000", "2000-3000", "3000-4000", ">4000"))

# Group Total_Income
rawDataForPlot <- group_continuous_variable(rawDataForPlot, "Total_Income", 
                                     breaks = c(0, 3000, 6000, 9000, 12000, Inf), 
                                     labels = c("0-3000", "3000-6000", "6000-9000", "9000-12000", ">12000"))

# Group Income_to_Loan
rawDataForPlot <- group_continuous_variable(rawDataForPlot, "Income_to_Loan", 
                                     breaks = c(0, 20, 40, 60, 80, 100, Inf), 
                                     labels = c("0-20", "20-40", "40-60", "60-80", "80-100", ">100"))


# Plot percentage of loans granted by Gender
rawDataForPlotGender <- rawDataForPlot %>%
  filter(!is.na(Gender) & Gender != "")

#Percentage of Loans Granted by Gender
plot1 <- create_percentage_plot(rawDataForPlotGender, "Gender", "Percentage of Loans Granted by Gender")
```

```
## Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
## ℹ Please use tidy evaluation idioms with `aes()`.
## ℹ See also `vignette("ggplot2-in-packages")` for more information.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

``` r
print(plot1)
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

``` r
# Plot percentage of loans granted by Married
plot2 <- create_percentage_plot(rawDataForPlot, "Married", "Percentage of Loans Granted by Marriage Status")
print(plot2)
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-6-2.png)<!-- -->

``` r
# Plot percentage of loans granted by Education
plot3 <- create_percentage_plot(rawDataForPlot, "Education", "Percentage of Loans Granted by Education Status")
print(plot3)
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-6-3.png)<!-- -->

``` r
# Plot percentage of loans granted by Self_Employed
plot4 <- create_percentage_plot(rawDataForPlot, "Self_Employed", "Percentage of Loans Granted by Self_Employed Status")
print(plot4)
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-6-4.png)<!-- -->

``` r
# Plot percentage of loans granted by Property Area
plot5 <- create_percentage_plot(rawDataForPlot, "Property_Area", "Percentage of Loans Granted by Property Area")
print(plot5)
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-6-5.png)<!-- -->

``` r
# Plot percentage of loans granted by Dependents
plot6 <- create_percentage_plot(rawDataForPlot, "Dependents", "Percentage of Loans Granted by Number of Dependents")
print(plot6)
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-6-6.png)<!-- -->

``` r
# Plot percentage of loans granted by Credit History
plot7 <- create_percentage_plot(rawDataForPlot, "Credit_History", "Percentage of Loans Granted by Credit History")
print(plot7)
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-6-7.png)<!-- -->

``` r
# Plot percentage of loans granted by Loan Amount Term
plot8 <- create_percentage_plot(rawDataForPlot, "Loan_Amount_Term", "Percentage of Loans Granted by Loan Amount Term")
print(plot8)
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-6-8.png)<!-- -->

``` r
# Plot percentage of loans granted by Applicant Income (grouped)
plot9 <- create_percentage_plot(rawDataForPlot, "ApplicantIncome", "Percentage of Loans Granted by Applicant Income")
print(plot9)
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-6-9.png)<!-- -->

``` r
# Plot percentage of loans granted by Coapplicant Income (grouped)
plot10 <- create_percentage_plot(rawDataForPlot, "CoapplicantIncome", "Percentage of Loans Granted by Coapplicant Income")
print(plot10)
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-6-10.png)<!-- -->

``` r
# Plot percentage of loans granted by Total Income (grouped)
plot11 <- create_percentage_plot(rawDataForPlot, "Total_Income", "Percentage of Loans Granted by Total Income")
print(plot11)
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-6-11.png)<!-- -->

``` r
# Plot percentage of loans granted by Income to Loan (grouped)
plot12 <- create_percentage_plot(rawDataForPlot, "Income_to_Loan", "Percentage of Loans Granted by Income to Loan")
print(plot12)
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-6-12.png)<!-- -->

# Check for Corillation


``` r
library(tidyverse)
library(caret)
library(corrplot)
```

```
## corrplot 0.92 loaded
```

``` r
# Select only the numerical columns
numerical_vars <- rawData %>% select(ApplicantIncome, CoapplicantIncome, LoanAmount, Income_to_Loan)

# Compute the correlation matrix
cor_matrix <- cor(numerical_vars, use = "complete.obs")

# Print the correlation matrix
print(cor_matrix)
```

```
##                   ApplicantIncome CoapplicantIncome LoanAmount Income_to_Loan
## ApplicantIncome         1.0000000        -0.2552392  0.2567091      0.1203217
## CoapplicantIncome      -0.2552392         1.0000000  0.1248243      0.5849900
## LoanAmount              0.2567091         0.1248243  1.0000000     -0.4287219
## Income_to_Loan          0.1203217         0.5849900 -0.4287219      1.0000000
```

``` r
# Visualize the correlation matrix using a heatmap
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8, number.cex = 0.7, addCoef.col = "black")
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


``` r
# Select only the categorical columns
categorical_vars <- rawData %>% select(Gender, Married, Dependents, Education, Self_Employed, Property_Area, Loan_Status)

# Perform chi-square test for each pair of categorical variables
cat_var_names <- names(categorical_vars)
chi_square_results <- matrix(nrow = length(cat_var_names), ncol = length(cat_var_names), dimnames = list(cat_var_names, cat_var_names))

for (i in 1:length(cat_var_names)) {
  for (j in 1:length(cat_var_names)) {
    if (i != j) {
      chi_square_results[i, j] <- chisq.test(categorical_vars[[i]], categorical_vars[[j]])$p.value
    } else {
      chi_square_results[i, j] <- NA
    }
  }
}
```

```
## Warning in chisq.test(categorical_vars[[i]], categorical_vars[[j]]):
## Chi-Quadrat-Approximation kann inkorrekt sein
## Warning in chisq.test(categorical_vars[[i]], categorical_vars[[j]]):
## Chi-Quadrat-Approximation kann inkorrekt sein
## Warning in chisq.test(categorical_vars[[i]], categorical_vars[[j]]):
## Chi-Quadrat-Approximation kann inkorrekt sein
## Warning in chisq.test(categorical_vars[[i]], categorical_vars[[j]]):
## Chi-Quadrat-Approximation kann inkorrekt sein
## Warning in chisq.test(categorical_vars[[i]], categorical_vars[[j]]):
## Chi-Quadrat-Approximation kann inkorrekt sein
## Warning in chisq.test(categorical_vars[[i]], categorical_vars[[j]]):
## Chi-Quadrat-Approximation kann inkorrekt sein
## Warning in chisq.test(categorical_vars[[i]], categorical_vars[[j]]):
## Chi-Quadrat-Approximation kann inkorrekt sein
## Warning in chisq.test(categorical_vars[[i]], categorical_vars[[j]]):
## Chi-Quadrat-Approximation kann inkorrekt sein
## Warning in chisq.test(categorical_vars[[i]], categorical_vars[[j]]):
## Chi-Quadrat-Approximation kann inkorrekt sein
## Warning in chisq.test(categorical_vars[[i]], categorical_vars[[j]]):
## Chi-Quadrat-Approximation kann inkorrekt sein
## Warning in chisq.test(categorical_vars[[i]], categorical_vars[[j]]):
## Chi-Quadrat-Approximation kann inkorrekt sein
## Warning in chisq.test(categorical_vars[[i]], categorical_vars[[j]]):
## Chi-Quadrat-Approximation kann inkorrekt sein
## Warning in chisq.test(categorical_vars[[i]], categorical_vars[[j]]):
## Chi-Quadrat-Approximation kann inkorrekt sein
## Warning in chisq.test(categorical_vars[[i]], categorical_vars[[j]]):
## Chi-Quadrat-Approximation kann inkorrekt sein
## Warning in chisq.test(categorical_vars[[i]], categorical_vars[[j]]):
## Chi-Quadrat-Approximation kann inkorrekt sein
## Warning in chisq.test(categorical_vars[[i]], categorical_vars[[j]]):
## Chi-Quadrat-Approximation kann inkorrekt sein
## Warning in chisq.test(categorical_vars[[i]], categorical_vars[[j]]):
## Chi-Quadrat-Approximation kann inkorrekt sein
## Warning in chisq.test(categorical_vars[[i]], categorical_vars[[j]]):
## Chi-Quadrat-Approximation kann inkorrekt sein
## Warning in chisq.test(categorical_vars[[i]], categorical_vars[[j]]):
## Chi-Quadrat-Approximation kann inkorrekt sein
## Warning in chisq.test(categorical_vars[[i]], categorical_vars[[j]]):
## Chi-Quadrat-Approximation kann inkorrekt sein
## Warning in chisq.test(categorical_vars[[i]], categorical_vars[[j]]):
## Chi-Quadrat-Approximation kann inkorrekt sein
## Warning in chisq.test(categorical_vars[[i]], categorical_vars[[j]]):
## Chi-Quadrat-Approximation kann inkorrekt sein
```

``` r
# Print the chi-square test results
print(chi_square_results)
```

```
##                     Gender      Married   Dependents  Education Self_Employed
## Gender                  NA 1.514891e-09 1.222131e-02 0.06212498     0.7844717
## Married       1.514891e-09           NA 2.512381e-11 0.53279748     1.0000000
## Dependents    1.222131e-02 2.512381e-11           NA 0.34973385     0.3706841
## Education     6.212498e-02 5.327975e-01 3.497338e-01         NA     0.6594358
## Self_Employed 7.844717e-01 1.000000e+00 3.706841e-01 0.65943585            NA
## Property_Area 7.446191e-02 7.560193e-01 4.204311e-01 0.19789072     0.5840935
## Loan_Status   2.228267e-02 8.181087e-02 5.541186e-01 0.36275297     0.4231037
##               Property_Area Loan_Status
## Gender          0.074461908 0.022282673
## Married         0.756019284 0.081810867
## Dependents      0.420431081 0.554118583
## Education       0.197890725 0.362752973
## Self_Employed   0.584093502 0.423103734
## Property_Area            NA 0.006092352
## Loan_Status     0.006092352          NA
```

``` r
# Convert to a dataframe for easier viewing
chi_square_df <- as.data.frame(as.table(chi_square_results))
names(chi_square_df) <- c("Variable1", "Variable2", "P_Value")
chi_square_df <- chi_square_df %>% filter(!is.na(P_Value))

# Print the chi-square test dataframe
print(chi_square_df)
```

```
##        Variable1     Variable2      P_Value
## 1        Married        Gender 1.514891e-09
## 2     Dependents        Gender 1.222131e-02
## 3      Education        Gender 6.212498e-02
## 4  Self_Employed        Gender 7.844717e-01
## 5  Property_Area        Gender 7.446191e-02
## 6    Loan_Status        Gender 2.228267e-02
## 7         Gender       Married 1.514891e-09
## 8     Dependents       Married 2.512381e-11
## 9      Education       Married 5.327975e-01
## 10 Self_Employed       Married 1.000000e+00
## 11 Property_Area       Married 7.560193e-01
## 12   Loan_Status       Married 8.181087e-02
## 13        Gender    Dependents 1.222131e-02
## 14       Married    Dependents 2.512381e-11
## 15     Education    Dependents 3.497338e-01
## 16 Self_Employed    Dependents 3.706841e-01
## 17 Property_Area    Dependents 4.204311e-01
## 18   Loan_Status    Dependents 5.541186e-01
## 19        Gender     Education 6.212498e-02
## 20       Married     Education 5.327975e-01
## 21    Dependents     Education 3.497338e-01
## 22 Self_Employed     Education 6.594358e-01
## 23 Property_Area     Education 1.978907e-01
## 24   Loan_Status     Education 3.627530e-01
## 25        Gender Self_Employed 7.844717e-01
## 26       Married Self_Employed 1.000000e+00
## 27    Dependents Self_Employed 3.706841e-01
## 28     Education Self_Employed 6.594358e-01
## 29 Property_Area Self_Employed 5.840935e-01
## 30   Loan_Status Self_Employed 4.231037e-01
## 31        Gender Property_Area 7.446191e-02
## 32       Married Property_Area 7.560193e-01
## 33    Dependents Property_Area 4.204311e-01
## 34     Education Property_Area 1.978907e-01
## 35 Self_Employed Property_Area 5.840935e-01
## 36   Loan_Status Property_Area 6.092352e-03
## 37        Gender   Loan_Status 2.228267e-02
## 38       Married   Loan_Status 8.181087e-02
## 39    Dependents   Loan_Status 5.541186e-01
## 40     Education   Loan_Status 3.627530e-01
## 41 Self_Employed   Loan_Status 4.231037e-01
## 42 Property_Area   Loan_Status 6.092352e-03
```


``` r
# Drop a single column by name
rawData <- rawData[, !(names(rawData) %in% c("Total_Income"))]
rawData <- rawData[, !(names(rawData) %in% c("Gender"))]
```


# Model Training


``` r
# Split Data into Training and Testing Sets
set.seed(789)
trainIndex <- createDataPartition(rawData$Loan_Status, p = .8, 
                                  list = FALSE, 
                                  times = 1)
X_train <- rawData[trainIndex,]
X_test <- rawData[-trainIndex,]
```


``` r
# Ensure Loan_Status is a factor
X_train$Loan_Status <- as.factor(X_train$Loan_Status)
X_test$Loan_Status <- as.factor(X_test$Loan_Status)
```


``` r
# Preprocessing
preprocess <- preProcess(X_train, method = c("center", "scale"))
X_train <- predict(preprocess, X_train)
X_test <- predict(preprocess, X_test)
```

### Train and Evaluate the Random Forest Model


``` r
plot_roc_curve <- function(roc_obj, model_name) {
  ggroc(roc_obj, legacy.axes = TRUE) +
    geom_abline(linetype = "dashed", color = "gray") +
    ggtitle(paste("ROC Curve for", model_name, "Model")) +
    theme_minimal() +
    xlab("False Positive Rate") +
    ylab("True Positive Rate")
}
```


``` r
# Load necessary libraries
library(caret)
library(pROC)

# Define possible values for hyperparameters
mtry_values <- c(2, 4, 6, 8, 16)
ntree_values <- c(100, 200, 300, 500)
nodesize_values <- c(1, 5, 10, 20)

# Initialize variables to store the best results
best_precision <- 0
best_model <- NULL
best_mtry <- NA
best_ntree <- NA
best_nodesize <- NA

# Loop through all combinations of hyperparameters
for (mtry in mtry_values) {
  for (ntree in ntree_values) {
    for (nodesize in nodesize_values) {
      
      # Train the Random Forest model with the current set of hyperparameters
      rf_model <- train(
        Loan_Status ~ ., 
        data = X_train, 
        method = "rf", 
        trControl = trainControl(method = "cv", number = 10),
        tuneGrid = expand.grid(mtry = mtry),
        ntree = ntree,
        nodesize = nodesize
      )
      
      # Predict on the test set
      rf_predictions <- predict(rf_model, X_test)
      conf_matrix <- confusionMatrix(rf_predictions, X_test$Loan_Status)
      
      # Extract precision from confusion matrix
      precision <- conf_matrix$byClass['Precision']
      
      # Check if the current model has the best precision so far
      if (precision > best_precision) {
        best_precision <- precision
        best_model <- rf_model
        best_mtry <- mtry
        best_ntree <- ntree
        best_nodesize <- nodesize
      }
    }
  }
}
```

```
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
## Warning in randomForest.default(x, y, mtry = param$mtry, ...): invalid mtry:
## reset to within valid range
```

``` r
# Predict on the test set with the best model
rf_predictions <- predict(best_model, X_test)
conf_matrix <- confusionMatrix(rf_predictions, X_test$Loan_Status)

print("Confusion Matrix:")
```

```
## [1] "Confusion Matrix:"
```

``` r
print(conf_matrix)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction No Yes
##        No  10   1
##        Yes 10  46
##                                           
##                Accuracy : 0.8358          
##                  95% CI : (0.7252, 0.9151)
##     No Information Rate : 0.7015          
##     P-Value [Acc > NIR] : 0.008892        
##                                           
##                   Kappa : 0.5498          
##                                           
##  Mcnemar's Test P-Value : 0.015861        
##                                           
##             Sensitivity : 0.5000          
##             Specificity : 0.9787          
##          Pos Pred Value : 0.9091          
##          Neg Pred Value : 0.8214          
##              Prevalence : 0.2985          
##          Detection Rate : 0.1493          
##    Detection Prevalence : 0.1642          
##       Balanced Accuracy : 0.7394          
##                                           
##        'Positive' Class : No              
## 
```

``` r
# Extract metrics from confusion matrix
recall <- conf_matrix$byClass['Recall']
precision <- conf_matrix$byClass['Precision']
f1 <- 2 * (precision * recall) / (precision + recall)

# Get probabilities for ROC curve
rf_probs <- predict(best_model, X_test, type = "prob")[, 2]

# Calculate ROC curve
rf_roc <- roc(X_test$Loan_Status, rf_probs)
```

```
## Setting levels: control = No, case = Yes
```

```
## Setting direction: controls < cases
```

``` r
# Plot ROC curve
print(plot_roc_curve(rf_roc, "Random Forest"))
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

``` r
# Calculate and display accuracy, AUC, recall, precision, and F1 score
rf_metrics <- data.frame(
  Accuracy = max(best_model$results$Accuracy), 
  AUC = auc(rf_roc),
  Recall = recall,
  Precision = precision,
  F1_Score = f1,
  best_mtry = best_mtry,
  best_ntree = best_ntree,
  best_nodesize = best_nodesize
)
print(rf_metrics)
```

```
##         Accuracy       AUC Recall Precision  F1_Score best_mtry best_ntree
## Recall 0.8351852 0.8367021    0.5 0.9090909 0.6451613         2        100
##        best_nodesize
## Recall             5
```

### Train and Evaluate the KNN Model


``` r
# Define control for KNN with cross-validation
knn_control <- trainControl(method = "cv", number = 10)

# Define grid for KNN hyperparameter tuning
knn_grid <- expand.grid(k = c(1, 3, 5, 7, 9, 11))

# Train the KNN model with hyperparameter tuning
knn_model <- train(
  Loan_Status ~ ., 
  data = X_train, 
  method = "knn", 
  tuneGrid = knn_grid, 
  trControl = knn_control
)

# Predict on the test set
knn_predictions <- predict(knn_model, X_test)
knn_conf_matrix <- confusionMatrix(knn_predictions, X_test$Loan_Status)

print("Confusion Matrix:")
```

```
## [1] "Confusion Matrix:"
```

``` r
print(knn_conf_matrix)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction No Yes
##        No   9   1
##        Yes 11  46
##                                          
##                Accuracy : 0.8209         
##                  95% CI : (0.708, 0.9039)
##     No Information Rate : 0.7015         
##     P-Value [Acc > NIR] : 0.019093       
##                                          
##                   Kappa : 0.5006         
##                                          
##  Mcnemar's Test P-Value : 0.009375       
##                                          
##             Sensitivity : 0.4500         
##             Specificity : 0.9787         
##          Pos Pred Value : 0.9000         
##          Neg Pred Value : 0.8070         
##              Prevalence : 0.2985         
##          Detection Rate : 0.1343         
##    Detection Prevalence : 0.1493         
##       Balanced Accuracy : 0.7144         
##                                          
##        'Positive' Class : No             
## 
```

``` r
# Extract metrics from confusion matrix
knn_recall <- knn_conf_matrix$byClass['Recall']
knn_precision <- knn_conf_matrix$byClass['Precision']
knn_f1 <- 2 * (knn_precision * knn_recall) / (knn_precision + knn_recall)

# Get probabilities for ROC curve
knn_probs <- predict(knn_model, X_test, type = "prob")[, 2]

# Calculate ROC curve
knn_roc <- roc(X_test$Loan_Status, knn_probs)
```

```
## Setting levels: control = No, case = Yes
```

```
## Setting direction: controls < cases
```

``` r
# Plot ROC curve
print(plot_roc_curve(knn_roc, "KNN"))
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

``` r
# Calculate and display accuracy, AUC, recall, precision, and F1 score
knn_metrics <- data.frame(
  Accuracy = max(knn_model$results$Accuracy), 
  AUC = auc(knn_roc),
  Recall = knn_recall,
  Precision = knn_precision,
  F1_Score = knn_f1
)
print(knn_metrics)
```

```
##         Accuracy       AUC Recall Precision F1_Score
## Recall 0.8428571 0.8207447   0.45       0.9      0.6
```

### Train and Evaluate the Decision Tree Model


``` r
# Define control for Decision Tree with cross-validation
dt_control <- trainControl(method = "cv", number = 10)

# Define grid for Decision Tree hyperparameter tuning
dt_grid <- expand.grid(cp = seq(0.01, 0.1, by = 0.01))

# Train the Decision Tree model with hyperparameter tuning
dt_model <- train(
  Loan_Status ~ ., 
  data = X_train, 
  method = "rpart", 
  tuneGrid = dt_grid, 
  trControl = dt_control
)

# Predict on the test set
dt_predictions <- predict(dt_model, X_test)
dt_conf_matrix <- confusionMatrix(dt_predictions, X_test$Loan_Status)

print("Decision Tree Confusion Matrix:")
```

```
## [1] "Decision Tree Confusion Matrix:"
```

``` r
print(dt_conf_matrix)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction No Yes
##        No   9   1
##        Yes 11  46
##                                          
##                Accuracy : 0.8209         
##                  95% CI : (0.708, 0.9039)
##     No Information Rate : 0.7015         
##     P-Value [Acc > NIR] : 0.019093       
##                                          
##                   Kappa : 0.5006         
##                                          
##  Mcnemar's Test P-Value : 0.009375       
##                                          
##             Sensitivity : 0.4500         
##             Specificity : 0.9787         
##          Pos Pred Value : 0.9000         
##          Neg Pred Value : 0.8070         
##              Prevalence : 0.2985         
##          Detection Rate : 0.1343         
##    Detection Prevalence : 0.1493         
##       Balanced Accuracy : 0.7144         
##                                          
##        'Positive' Class : No             
## 
```

``` r
# Extract metrics from confusion matrix
dt_recall <- dt_conf_matrix$byClass['Recall']
dt_precision <- dt_conf_matrix$byClass['Precision']
dt_f1 <- 2 * (dt_precision * dt_recall) / (dt_precision + dt_recall)

# Get probabilities for ROC curve
dt_probs <- predict(dt_model, X_test, type = "prob")[, 2]

# Calculate ROC curve
dt_roc <- roc(X_test$Loan_Status, dt_probs)
```

```
## Setting levels: control = No, case = Yes
```

```
## Setting direction: controls < cases
```

``` r
# Plot ROC curve
print(plot_roc_curve(dt_roc, "Decision Tree"))
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-16-1.png)<!-- -->

``` r
# Calculate and display accuracy, AUC, recall, precision, and F1 score
dt_metrics <- data.frame(
  Accuracy = max(dt_model$results$Accuracy), 
  AUC = auc(dt_roc),
  Recall = dt_recall,
  Precision = dt_precision,
  F1_Score = dt_f1
)
print("Decision Tree Metrics:")
```

```
## [1] "Decision Tree Metrics:"
```

``` r
print(dt_metrics)
```

```
##         Accuracy       AUC Recall Precision F1_Score
## Recall 0.8420361 0.7143617   0.45       0.9      0.6
```

# Compare Models


``` r
# Create a dataframe to compare the models
model_comparison <- data.frame(
  Model = c("Random Forest", "KNN", "Decision Tree"),
  Accuracy = c(rf_metrics$Accuracy, knn_metrics$Accuracy, dt_metrics$Accuracy),
  AUC = c(rf_metrics$AUC, knn_metrics$AUC, dt_metrics$AUC),
  Precision = c(rf_metrics$Precision, knn_metrics$Precision, dt_metrics$Precision)
)

model_comparison
```

```
##           Model  Accuracy       AUC Precision
## 1 Random Forest 0.8351852 0.8367021 0.9090909
## 2           KNN 0.8428571 0.8207447 0.9000000
## 3 Decision Tree 0.8420361 0.7143617 0.9000000
```


``` r
# Reshape the dataframe for plotting
model_comparison_melted <- reshape2::melt(model_comparison, id.vars = 'Model')

# Plot the comparison
ggplot(model_comparison_melted, aes(x = Model, y = value, color = variable, group = variable)) +
  geom_line(aes(linetype = variable), size = 1) +
  geom_point(size = 3) +
  labs(title = "Model Comparison",
       x = "Model",
       y = "Score") +
  scale_color_manual(values = c("Accuracy" = "blue", "AUC" = "green", "Precision" = "red")) +
  theme_minimal() +
  theme(legend.title = element_blank())
```

```
## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
## ℹ Please use `linewidth` instead.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-18-1.png)<!-- -->


``` r
# Reshape the dataframe for plotting
model_comparison_melted <- reshape2::melt(model_comparison, id.vars = 'Model')
```


``` r
# Get unique model names for color mapping
unique_models <- unique(model_comparison_melted$Model)

# Create a color palette
color_palette <- setNames(c("blue", "green", "red"), unique_models)

# Plot the comparison with KPIs on the x-axis
ggplot(model_comparison_melted, aes(x = variable, y = value, color = Model, group = Model)) +
  geom_line(aes(linetype = Model), size = 1) +
  geom_point(size = 3) +
  labs(title = "Model Comparison",
       x = "KPI",
       y = "Score") +
  scale_color_manual(values = color_palette) +
  theme_minimal() +
  theme(legend.title = element_blank())
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

# Identify the best model


``` r
best_model_name <- model_comparison[which.max(model_comparison$AUC), "Model"]
print(paste("Best model based on AUC is:", best_model_name))
```

```
## [1] "Best model based on AUC is: Random Forest"
```

``` r
if (best_model_name == "Random Forest") {
  best_model <- rf_model$finalModel
  varImpPlot(best_model)
} else if (best_model_name == "Decision Tree") {
  best_model <- dt_model$finalModel
  rpart.plot(best_model)
}
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-21-1.png)<!-- -->
