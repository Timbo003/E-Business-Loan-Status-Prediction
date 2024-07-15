---
title: "E-Business Loan Status Prediction"
output: 
  html_document:
    keep_md: true
---

# Load libraries


``` r
library(readr)
library(dplyr)
```

```
## 
## Attache Paket: 'dplyr'
```

```
## Die folgenden Objekte sind maskiert von 'package:stats':
## 
##     filter, lag
```

```
## Die folgenden Objekte sind maskiert von 'package:base':
## 
##     intersect, setdiff, setequal, union
```

``` r
library(ggplot2)
```

------------------------------------------------------------------------

# Load the data


``` r
rawData <- read_csv("loan_data.csv")
```

```
## Rows: 381 Columns: 13
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (8): Loan_ID, Gender, Married, Dependents, Education, Self_Employed, Pro...
## dbl (5): ApplicantIncome, CoapplicantIncome, LoanAmount, Loan_Amount_Term, C...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
# print col names
names(rawData)
```

```
##  [1] "Loan_ID"           "Gender"            "Married"          
##  [4] "Dependents"        "Education"         "Self_Employed"    
##  [7] "ApplicantIncome"   "CoapplicantIncome" "LoanAmount"       
## [10] "Loan_Amount_Term"  "Credit_History"    "Property_Area"    
## [13] "Loan_Status"
```

# Transform the data


``` r
# Drop the Loan_ID column
rawData <- rawData %>% select(-Loan_ID)

# Change Married from Y/N to True/False
rawData <- rawData %>% mutate(Married = ifelse(Married == "Yes", TRUE, FALSE))

# Change Education from Y/N to True/False
rawData <- rawData %>% mutate(Education = ifelse(Education == "Graduate", TRUE, FALSE))

# Change Education from Y/N to True/False
rawData <- rawData %>% mutate(Self_Employed = ifelse(Self_Employed == "Yes", TRUE, FALSE))

# print data head
head(rawData)
```

```
## # A tibble: 6 × 12
##   Gender Married Dependents Education Self_Employed ApplicantIncome
##   <chr>  <lgl>   <chr>      <lgl>     <lgl>                   <dbl>
## 1 Male   TRUE    1          TRUE      FALSE                    4583
## 2 Male   TRUE    0          TRUE      TRUE                     3000
## 3 Male   TRUE    0          FALSE     FALSE                    2583
## 4 Male   FALSE   0          TRUE      FALSE                    6000
## 5 Male   TRUE    0          FALSE     FALSE                    2333
## 6 Male   TRUE    2          TRUE      FALSE                    3200
## # ℹ 6 more variables: CoapplicantIncome <dbl>, LoanAmount <dbl>,
## #   Loan_Amount_Term <dbl>, Credit_History <dbl>, Property_Area <chr>,
## #   Loan_Status <chr>
```

------------------------------------------------------------------------

# Initial Data sighting


``` r
# Plot 1: Distribution of Loan Status with customized colors
ggplot(rawData, aes(x = Loan_Status, fill = Loan_Status)) +
  geom_bar(color = "black") +
  scale_fill_manual(values = c("Y" = "#90EE90", "N" = "#FFB6C1")) +
  theme_minimal() +
  ggtitle("Distribution of Loan Status")
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


``` r
# Plot 2: Distribution of Gender with customized RGB colors
ggplot(rawData, aes(x = Gender, fill = Gender)) +
  geom_bar(color = "black") +
  scale_fill_manual(values = c("Male" = "#89CFF0", "Female" = "#FFB6C1")) +
  theme_minimal() +
  ggtitle("Distribution of Gender")
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


``` r
# Plot 3: Average Loan Amount by Property Area
avg_loan_amount <- rawData %>%
  group_by(Property_Area) %>%
  summarise(Average_LoanAmount = mean(LoanAmount, na.rm = TRUE))

ggplot(avg_loan_amount, aes(x = Property_Area, y = Average_LoanAmount, fill = Property_Area)) +
  geom_bar(stat = "identity", color = "black") +
  theme_minimal() +
  ggtitle("Average Loan Amount by Property Area") +
  labs(y = "Average Loan Amount", fill = "Property Area")
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


``` r
# Plot 4: Distribution of Loan Status by Education
ggplot(rawData, aes(x = Loan_Status, fill = Education)) +
  geom_bar(position = "dodge", color = "black") +
  theme_minimal() +
  ggtitle("Distribution of Loan Status by Education") +
  labs(fill = "Education")
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


``` r
# Plot 5: Distribution of Loan Status by Self Employed status
ggplot(rawData, aes(x = Loan_Status, fill = Self_Employed)) +
  geom_bar(position = "dodge", color = "black") +
  theme_minimal() +
  ggtitle("Distribution of Loan Status by Self Employed Status") +
  labs(fill = "Self Employed")
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-8-1.png)<!-- -->



# teststet
