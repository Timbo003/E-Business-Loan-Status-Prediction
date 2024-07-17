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


``` r
# Save the data as a csv #TODO
write.csv(rawData, "C:/Users/timst/Documents/GitHub/E-Business-Loan-Status-Prediction/rawData.csv")
```

# Initial Data sighting


``` r
# Plot 1: Distribution of Loan Status with customized colors
ggplot(rawData, aes(x = Loan_Status, fill = Loan_Status)) +
  geom_bar() +
  scale_fill_manual(values = c("TRUE" = "#90EE90", "FALSE" = "#FFB6C1")) +
  ggtitle("Distribution of Loan Status") +
  theme_minimal()
```

```
## Warning: No shared levels found between `names(values)` of the manual scale and the
## data's fill values.
## No shared levels found between `names(values)` of the manual scale and the
## data's fill values.
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

``` r
# Plot 2: Distribution of Gender with customized RGB colors
ggplot(rawData, aes(x = Gender, fill = Gender)) +
  geom_bar() +
  scale_fill_manual(values = c("Male" = "#89CFF0", "Female" = "#FFB6C1")) +
  ggtitle("Distribution of Gender") +
  theme_minimal()
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-5-2.png)<!-- -->

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

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-5-3.png)<!-- -->

``` r
# Plot 4: Distribution of Loan Status by Education
ggplot(rawData, aes(x = Loan_Status, fill = Education)) +
  geom_bar(position = "dodge") +
  ggtitle("Distribution of Loan Status by Education") +
  theme_minimal()
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-5-4.png)<!-- -->

``` r
# Plot 5: Distribution of Loan Status by Self Employed status
ggplot(rawData, aes(x = Loan_Status, fill = Self_Employed)) +
  geom_bar(position = "dodge") +
  ggtitle("Distribution of Loan Status by Self Employed Status") +
  theme_minimal()
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-5-5.png)<!-- -->


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

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

``` r
# Plot 2: Average Loan Amount  granted granted by Gender
# Calculate the average loan amount by gender and remove missing values
avg_loan_amount_gender <- rawData %>%
  filter(Loan_Status == "TRUE" & !is.na(Gender) & Gender != "") %>%
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

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-6-2.png)<!-- -->

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

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-6-3.png)<!-- -->

``` r
# Plot 4: Average Loan Amount granted by Property Area
avg_loan_amount <- rawData %>%
  filter(Loan_Status == "TRUE") %>%
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

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-6-4.png)<!-- -->

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

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-6-5.png)<!-- -->

``` r
# Plot 6: Average Loan Amount granted by Marriage Status
avg_loan_amount <- rawData %>%
  filter(Loan_Status == "TRUE") %>%
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

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-6-6.png)<!-- -->

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

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-6-7.png)<!-- -->

``` r
# Plot 8: Average Loan Amount granted by Education Status
avg_loan_amount <- rawData %>%
  filter(Loan_Status == "TRUE") %>%
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

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-6-8.png)<!-- -->

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

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-6-9.png)<!-- -->

``` r
# Plot 10: Average Loan Amount granted by Self_Employed Status
avg_loan_amount <- rawData %>%
  filter(Loan_Status == "TRUE") %>%
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

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-6-10.png)<!-- -->

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

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-6-11.png)<!-- -->

``` r
# Plot 12: Average Loan Amount granted by Number of Dependents
avg_loan_amount <- rawData %>%
  filter(Loan_Status == "TRUE") %>%
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

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-6-12.png)<!-- -->

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

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-6-13.png)<!-- -->

``` r
# Plot 14: Average loan amount granted by Applicant Income (Loan_Status == TRUE)
avg_loan_granted <- rawData %>%
  filter(Loan_Status == TRUE) %>%
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

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-6-14.png)<!-- -->

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

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-6-15.png)<!-- -->

``` r
# Plot 16: Average loan amount granted by Coapplicant Income (Loan_Status == TRUE)
avg_loan_granted <- rawData %>%
  filter(Loan_Status == TRUE) %>%
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

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-6-16.png)<!-- -->

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

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-6-17.png)<!-- -->

``` r
# Plot 18: Average loan amount granted by Total_Income (Loan_Status == TRUE)
avg_loan_granted <- rawData %>%
  filter(Loan_Status == TRUE) %>%
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

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-6-18.png)<!-- -->

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

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-6-19.png)<!-- -->

``` r
# Plot 20: Average loan amount granted by Income_to_Loan (Loan_Status == TRUE)
avg_loan_granted <- rawData %>%
  filter(Loan_Status == TRUE) %>%
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

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-6-20.png)<!-- -->

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

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-6-21.png)<!-- -->

``` r
# Plot 22: Average Loan Amount granted by Credit_History
avg_loan_amount <- rawData %>%
  filter(Loan_Status == "TRUE") %>%
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

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-6-22.png)<!-- -->

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

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-6-23.png)<!-- -->

``` r
# Plot 24: Average Loan Amount granted by Loan_Amount_Term
avg_loan_amount <- rawData %>%
  filter(Loan_Status == "TRUE") %>%
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

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-6-24.png)<!-- -->

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

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-6-25.png)<!-- -->

# Model Training


``` r
# Split Data into Training and Testing Sets
set.seed(123)
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
##        No   8   1
##        Yes 12  46
##                                           
##                Accuracy : 0.806           
##                  95% CI : (0.6911, 0.8924)
##     No Information Rate : 0.7015          
##     P-Value [Acc > NIR] : 0.037459        
##                                           
##                   Kappa : 0.4498          
##                                           
##  Mcnemar's Test P-Value : 0.005546        
##                                           
##             Sensitivity : 0.4000          
##             Specificity : 0.9787          
##          Pos Pred Value : 0.8889          
##          Neg Pred Value : 0.7931          
##              Prevalence : 0.2985          
##          Detection Rate : 0.1194          
##    Detection Prevalence : 0.1343          
##       Balanced Accuracy : 0.6894          
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

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

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
## Recall 0.8347884 0.6941489    0.4 0.8888889 0.5517241         2        100
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
##        No   7   1
##        Yes 13  46
##                                           
##                Accuracy : 0.791           
##                  95% CI : (0.6743, 0.8808)
##     No Information Rate : 0.7015          
##     P-Value [Acc > NIR] : 0.067604        
##                                           
##                   Kappa : 0.3972          
##                                           
##  Mcnemar's Test P-Value : 0.003283        
##                                           
##             Sensitivity : 0.3500          
##             Specificity : 0.9787          
##          Pos Pred Value : 0.8750          
##          Neg Pred Value : 0.7797          
##              Prevalence : 0.2985          
##          Detection Rate : 0.1045          
##    Detection Prevalence : 0.1194          
##       Balanced Accuracy : 0.6644          
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

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

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
## Recall 0.8530423 0.6803191   0.35     0.875      0.5
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
##        No   7   1
##        Yes 13  46
##                                           
##                Accuracy : 0.791           
##                  95% CI : (0.6743, 0.8808)
##     No Information Rate : 0.7015          
##     P-Value [Acc > NIR] : 0.067604        
##                                           
##                   Kappa : 0.3972          
##                                           
##  Mcnemar's Test P-Value : 0.003283        
##                                           
##             Sensitivity : 0.3500          
##             Specificity : 0.9787          
##          Pos Pred Value : 0.8750          
##          Neg Pred Value : 0.7797          
##              Prevalence : 0.2985          
##          Detection Rate : 0.1045          
##    Detection Prevalence : 0.1194          
##       Balanced Accuracy : 0.6644          
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

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

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
##        Accuracy       AUC Recall Precision F1_Score
## Recall     0.85 0.6643617   0.35     0.875      0.5
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
## 1 Random Forest 0.8347884 0.6941489 0.8888889
## 2           KNN 0.8530423 0.6803191 0.8750000
## 3 Decision Tree 0.8500000 0.6643617 0.8750000
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

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-15-1.png)<!-- -->


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

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

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

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-18-1.png)<!-- -->
