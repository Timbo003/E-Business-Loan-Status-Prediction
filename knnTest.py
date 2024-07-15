# Load libraries
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.model_selection import train_test_split, GridSearchCV
from sklearn.preprocessing import StandardScaler, OneHotEncoder
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import confusion_matrix, roc_curve, roc_auc_score, accuracy_score
from sklearn.tree import DecisionTreeClassifier, plot_tree
from sklearn.neighbors import KNeighborsClassifier
from sklearn.compose import ColumnTransformer
from sklearn.pipeline import Pipeline

# Load the data
rawData = pd.read_csv("loan_data.csv")

# Transform the data
rawData = rawData.drop(columns=['Loan_ID'])

# Transform categorical variables to factors
rawData['Married'] = rawData['Married'].apply(lambda x: True if x == "Yes" else False)
rawData['Education'] = rawData['Education'].apply(lambda x: True if x == "Graduate" else False)
rawData['Self_Employed'] = rawData['Self_Employed'].apply(lambda x: True if x == "Yes" else False)
rawData['Gender'] = rawData['Gender'].astype('category')
rawData['Dependents'] = rawData['Dependents'].astype('category')
rawData['Property_Area'] = rawData['Property_Area'].astype('category')
rawData['Loan_Status'] = rawData['Loan_Status'].apply(lambda x: True if x == "Y" else False)

# Feature Engineering
rawData['Total_Income'] = rawData['ApplicantIncome'] + rawData['CoapplicantIncome']
rawData['Income_to_Loan'] = rawData['Total_Income'] / rawData['LoanAmount']

# Drop all missing values just for testing TODO
rawData = rawData.dropna()

# Check for null values and handle them (if any)
null_counts = rawData.isnull().sum()
print(null_counts)

# Save the data as a csv #TODO
rawData.to_csv("C:/Users/timst/Documents/GitHub/E-Business-Loan-Status-Prediction/rawData.csv", index=False)

# Initial Data sighting
# Plot 1: Distribution of Loan Status with customized colors
sns.countplot(x='Loan_Status', hue='Loan_Status', data=rawData, palette={True: "#90EE90", False: "#FFB6C1"}, legend=False)
plt.title("Distribution of Loan Status")
plt.show()

# Plot 2: Distribution of Gender with customized RGB colors
sns.countplot(x='Gender', hue='Gender', data=rawData, palette={"Male": "#89CFF0", "Female": "#FFB6C1"}, legend=False)
plt.title("Distribution of Gender")
plt.show()

# Plot 3: Average Loan Amount by Property Area
avg_loan_amount = rawData.groupby('Property_Area')['LoanAmount'].mean().reset_index()
sns.barplot(x='Property_Area', y='LoanAmount', data=avg_loan_amount)
plt.title("Average Loan Amount by Property Area")
plt.ylabel("Average Loan Amount")
plt.show()

# Plot 4: Distribution of Loan Status by Education
sns.countplot(x='Loan_Status', hue='Education', data=rawData)
plt.title("Distribution of Loan Status by Education")
plt.show()

# Plot 5: Distribution of Loan Status by Self Employed status
sns.countplot(x='Loan_Status', hue='Self_Employed', data=rawData)
plt.title("Distribution of Loan Status by Self Employed Status")
plt.show()

# Split Data into Training and Testing Sets
X = rawData.drop(columns=['Loan_Status'])
y = rawData['Loan_Status']

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=123)

# Scale the Data
numeric_features = X.select_dtypes(include=[np.number]).columns.tolist()
categorical_features = X.select_dtypes(include=['category']).columns.tolist()

numeric_transformer = Pipeline(steps=[
    ('scaler', StandardScaler())
])

categorical_transformer = Pipeline(steps=[
    ('onehot', OneHotEncoder(drop='if_binary'))
])

preprocessor = ColumnTransformer(
    transformers=[
        ('num', numeric_transformer, numeric_features),
        ('cat', categorical_transformer, categorical_features)
    ])

# Train and Evaluate the Random Forest Model
rf_pipeline = Pipeline(steps=[
    ('preprocessor', preprocessor),
    ('classifier', RandomForestClassifier(n_estimators=500, random_state=123))
])

rf_param_grid = {'classifier__max_features': [2, 4, 6]}
rf_grid_search = GridSearchCV(rf_pipeline, rf_param_grid, cv=10)
rf_grid_search.fit(X_train, y_train)

rf_best_model = rf_grid_search.best_estimator_
rf_predictions = rf_best_model.predict(X_test)

# Confusion matrix
rf_conf_matrix = confusion_matrix(y_test, rf_predictions)
print(rf_conf_matrix)

# Plot ROC curve and calculate AUC
rf_probs = rf_best_model.predict_proba(X_test)[:, 1]
rf_roc_auc = roc_auc_score(y_test, rf_probs)
fpr, tpr, _ = roc_curve(y_test, rf_probs)
plt.plot(fpr, tpr, marker='.')
plt.title('ROC Curve for Random Forest Model')
plt.show()

rf_metrics = {'Accuracy': accuracy_score(y_test, rf_predictions), 'AUC': rf_roc_auc}
print(rf_metrics)

# Train and Evaluate the KNN Model
knn_pipeline = Pipeline(steps=[
    ('preprocessor', preprocessor),
    ('classifier', KNeighborsClassifier(n_neighbors=5))
])

knn_pipeline.fit(X_train, y_train)
knn_predictions = knn_pipeline.predict(X_test)

# Confusion matrix
knn_conf_matrix = confusion_matrix(y_test, knn_predictions)
print(knn_conf_matrix)

# Plot ROC curve and calculate AUC
knn_probs = knn_pipeline.predict_proba(X_test)[:, 1]
knn_roc_auc = roc_auc_score(y_test, knn_probs)
fpr, tpr, _ = roc_curve(y_test, knn_probs)
plt.plot(fpr, tpr, marker='.')
plt.title('ROC Curve for KNN Model')
plt.show()

knn_metrics = {'Accuracy': accuracy_score(y_test, knn_predictions), 'AUC': knn_roc_auc}
print(knn_metrics)

# Train and Evaluate the Decision Tree Model
dt_pipeline = Pipeline(steps=[
    ('preprocessor', preprocessor),
    ('classifier', DecisionTreeClassifier(random_state=123))
])

dt_pipeline.fit(X_train, y_train)
dt_predictions = dt_pipeline.predict(X_test)

# Confusion matrix
dt_conf_matrix = confusion_matrix(y_test, dt_predictions)
print(dt_conf_matrix)

# Plot ROC curve and calculate AUC
dt_probs = dt_pipeline.predict_proba(X_test)[:, 1]
dt_roc_auc = roc_auc_score(y_test, dt_probs)
fpr, tpr, _ = roc_curve(y_test, dt_probs)
plt.plot(fpr, tpr, marker='.')
plt.title('ROC Curve for Decision Tree Model')
plt.show()

dt_metrics = {'Accuracy': accuracy_score(y_test, dt_predictions), 'AUC': dt_roc_auc}
print(dt_metrics)

# Compare Models
model_comparison = pd.DataFrame({
    'Model': ['Random Forest', 'KNN', 'Decision Tree'],
    'Accuracy': [rf_metrics['Accuracy'], knn_metrics['Accuracy'], dt_metrics['Accuracy']],
    'AUC': [rf_metrics['AUC'], knn_metrics['AUC'], dt_metrics['AUC']]
})

print(model_comparison)

# Identify the best model
best_model_name = model_comparison.loc[model_comparison['AUC'].idxmax(), 'Model']
print(f"Best model based on AUC is: {best_model_name}")

if best_model_name == "Random Forest":
    best_model = rf_best_model
    # Plot the importance of variables
    importances = best_model.named_steps['classifier'].feature_importances_
    indices = np.argsort(importances)[::-1]
    plt.figure()
    plt.title("Feature importances")
    plt.bar(range(X.shape[1]), importances[indices], align="center")
    plt.xticks(range(X.shape[1]), X.columns[indices], rotation=90)
    plt.xlim([-1, X.shape[1]])
    plt.show()
elif best_model_name == "Decision Tree":
    best_model = dt_pipeline.named_steps['classifier']
    plt.figure(figsize=(20,10))
    plot_tree(best_model, feature_names=X.columns, class_names=['False', 'True'], filled=True)
    plt.show()

