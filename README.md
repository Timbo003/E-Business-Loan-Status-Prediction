# E-Business-Loan-Status-Prediction
Welcome to the E-Business Machine Learning Project repository! This project aims to predict the loan status, that applicants for a real-estate credit have at their preferred bank using machine learning techniques. The dataset used for this project is sourced from Kaggle: [Loan Status Prediction](https://www.kaggle.com/datasets/bhavikjikadara/loan-status-prediction).

##Project Overview and Business Case
The objective of this project is to build a machine learning model that can help banks to predict on the applicants creditability and to take the decision, if the application should be approaved or rejected. To achieve this our model is trained on a dataset of already closed cases of loan applications, which includes various features and customer characteristics such as information about the applicants financial situation (e.g. by looking at his regular income), his credit history and the conditions of the loan (e.g. information about the area, where the real-estate is located; information about payment amounts). Using the already known classification approaches of KNN, Decision Tree and Random Forest, as well as a hyperparameter tuning techniques, we are targeting to optimize the accuracy and ability of our fictous bank in predicting, if the application should be approved or rejected. This is necessary to ensure the repution and financial performance of the bank. 

###Dataset
The dataset used in this project is provided by Bhavik Jikadara on Kaggle. It contains the following columns:
Target Variable, on which we predict:
- Loan_Status: categorical -> Identifies the final decision on the loan application (whether is is approved or rejected)

Independent Variables:
- Loan_ID: categorical -> Unique identifier for each loan application
- Gender: categorical (male and female) -> Identifies the gender of the applicant 
- Married: categorical (Y/N) -> Indicates whether the applicant is married (Y for yes; N for no)
- Dependents: ordinal (0-2; all values above 3 are stored as 3+) -> The number of dependents the applicant has
- Education: categorical (graduated or not graduated) -> Information about educational qualification level of the applicant
- Self_Employed: categorical (Y/N) -> Indicates whether the applicant is running a own business or not (Y for yes, N for not)
- ApplicantIncome: numerical -> The montly regular income of the applicant 
- CoapplicantIncome: numerical -> Monthly regular income of a potential second coapplicant
- LoanAmount: numerical (in thousand $) -> Full amount of money, that is loaned from the bank and needs to be repayed
- Loan_Amount_Term: numerical (in months) -> Agreed term of the loan before the total amount must be repaid 
- Credit_History: categorical (1.0/0.0) -> Information about the credit history of the related applicant (1.0 if the applicant has a good credit history (has reliably repayed all his former loans), 0.0 if                                             there any conspicuous features in his credit history)
- Property_Area: categorical (urban, semiurban and rural) -> Information about the location of the related real-estate object
  
