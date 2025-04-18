# Customer Churn Prediction in R

Predicting customer churn for a telecom company using machine learning models in R.

## Table of Contents
- [Project Overview](#project-overview)
- [Dataset](#dataset)
- [Technologies Used](#technologies-used)
- [Project Structure](#project-structure)
- [How to Run](#how-to-run)
- [Results](#results)
- [Future Work](#future-work)

## Project Overview
The goal of this project was to identify customers likely to churn (leave the service) based on their usage patterns and service history. Early detection of churn can help businesses proactively retain customers.  
Techniques used include:
- Exploratory Data Analysis (EDA)
- Data preprocessing
- Machine learning models (Random Forest, Logistic Regression)
- Model evaluation (ROC curve, AUC score)

## Dataset
- **Source:** [Kaggle - Telco Customer Churn Dataset](https://www.kaggle.com/blastchar/telco-customer-churn)
- **Observations:** 7,043 customers
- **Features:** Demographic info, account information, usage patterns
- **Preprocessing:** Handling missing values, encoding categorical variables

## Technologies Used
- **R version:** 4.3.0
- **Libraries:**
  - tidyverse
  - caret
  - randomForest
  - ROCR
  - ggplot2

## Project Structure
├── data/ │ ├── telco_churn.csv ├── R/ │ ├── analysis.R │ ├── model_randomforest.R │ ├── model_logistic.R ├── output/ │ ├── churn_rf_confusionmatrix.png │ ├── churn_logistic_roc.png ├── README.md


## How to Run
1. Clone the repository:
git clone https://github.com/yourusername/CustomerChurnPrediction-R.git
2. Open the R project and install required libraries:
```r
install.packages(c("tidyverse", "caret", "randomForest", "ROCR", "ggplot2"))
