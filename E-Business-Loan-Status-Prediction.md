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
         Loan_Status = as.factor(Loan_Status == "Y"))
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
rf_grid <- expand.grid(mtry = c(2, 4, 6))
rf_control <- trainControl(method = "cv", number = 10)
rf_model <- train(Loan_Status ~ ., data = X_train, method = "rf", 
                  trControl = rf_control, tuneGrid = rf_grid)

rf_predictions <- predict(rf_model, X_test)
confusionMatrix(rf_predictions, X_test$Loan_Status)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction FALSE TRUE
##      FALSE     8    2
##      TRUE     12   45
##                                           
##                Accuracy : 0.791           
##                  95% CI : (0.6743, 0.8808)
##     No Information Rate : 0.7015          
##     P-Value [Acc > NIR] : 0.06760         
##                                           
##                   Kappa : 0.4174          
##                                           
##  Mcnemar's Test P-Value : 0.01616         
##                                           
##             Sensitivity : 0.4000          
##             Specificity : 0.9574          
##          Pos Pred Value : 0.8000          
##          Neg Pred Value : 0.7895          
##              Prevalence : 0.2985          
##          Detection Rate : 0.1194          
##    Detection Prevalence : 0.1493          
##       Balanced Accuracy : 0.6787          
##                                           
##        'Positive' Class : FALSE           
## 
```

``` r
rf_probs <- predict(rf_model, X_test, type = "prob")[, 2]
rf_roc <- roc(X_test$Loan_Status, rf_probs)
```

```
## Setting levels: control = FALSE, case = TRUE
```

```
## Setting direction: controls < cases
```

``` r
plot(rf_roc, main = "ROC Curve for Random Forest Model")
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

``` r
rf_metrics <- data.frame(Accuracy = max(rf_model$results$Accuracy), 
                         AUC = auc(rf_roc))
```

### Train and Evaluate the KNN Model

``` r
knn_model <- train(Loan_Status ~ ., data = X_train, method = "knn", tuneLength = 5)
knn_predictions <- predict(knn_model, X_test)
confusionMatrix(knn_predictions, X_test$Loan_Status)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction FALSE TRUE
##      FALSE     7    1
##      TRUE     13   46
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
##        'Positive' Class : FALSE           
## 
```

``` r
knn_probs <- predict(knn_model, X_test, type = "prob")[, 2]
knn_roc <- roc(X_test$Loan_Status, knn_probs)
```

```
## Setting levels: control = FALSE, case = TRUE
```

```
## Setting direction: controls < cases
```

``` r
plot(knn_roc, main = "ROC Curve for KNN Model")
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

``` r
knn_metrics <- data.frame(Accuracy = max(knn_model$results$Accuracy), 
                          AUC = auc(knn_roc))
```

### Train and Evaluate the Decision Tree Model

``` r
dt_model <- train(Loan_Status ~ ., data = X_train, method = "rpart")
dt_predictions <- predict(dt_model, X_test)
confusionMatrix(dt_predictions, X_test$Loan_Status)
```

```
## Confusion Matrix and Statistics
## 
##           Reference
## Prediction FALSE TRUE
##      FALSE     7    1
##      TRUE     13   46
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
##        'Positive' Class : FALSE           
## 
```

``` r
dt_probs <- predict(dt_model, X_test, type = "prob")[, 2]
dt_roc <- roc(X_test$Loan_Status, dt_probs)
```

```
## Setting levels: control = FALSE, case = TRUE
```

```
## Setting direction: controls < cases
```

``` r
plot(dt_roc, main = "ROC Curve for Decision Tree Model")
```

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

``` r
dt_metrics <- data.frame(Accuracy = max(dt_model$results$Accuracy), 
                         AUC = auc(dt_roc))
```

# Compare Models

``` r
model_comparison <- data.frame(
  Model = c("Random Forest", "KNN", "Decision Tree"),
  Accuracy = c(rf_metrics$Accuracy, knn_metrics$Accuracy, dt_metrics$Accuracy),
  AUC = c(rf_metrics$AUC, knn_metrics$AUC, dt_metrics$AUC)
)

print(model_comparison)
```

```
##           Model  Accuracy       AUC
## 1 Random Forest 0.8473545 0.7882979
## 2           KNN 0.8402217 0.6803191
## 3 Decision Tree 0.8370872 0.6643617
```

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

![](E-Business-Loan-Status-Prediction_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
