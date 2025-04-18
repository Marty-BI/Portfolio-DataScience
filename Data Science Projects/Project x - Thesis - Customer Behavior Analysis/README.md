# Customer Behavior Analysis using DID, ADID, and SDID Models

This project focuses on analyzing customer purchase behaviors over time, evaluating the impact of promotions using Difference-in-Differences (DID), Augmented DID (ADID), and Synthetic DID (SDID) methodologies.  
The analysis is performed across multiple datasets from different retailers.

## Table of Contents
- [Project Overview](#project-overview)
- [Techniques Used](#techniques-used)
- [Libraries](#libraries)
- [Workflow](#workflow)
- [Datasets](#datasets)
- [Key Outputs](#key-outputs)
- [How to Run](#how-to-run)

## Project Overview

The project cleans, transforms, and analyzes transaction datasets.  
It:
- Handles outliers and missing values
- Segments customers into light, medium, and heavy buyers
- Prepares train and validation datasets
- Applies DID, ADID, and SDID models to measure the promotional impact
- Visualizes revenue, quantity sold, price, frequency, and recency metrics

## Techniques Used

- **Data Cleaning**: Outlier removal, null value handling, and data standardization
- **Customer Segmentation**: Based on historical total revenue
- **Causal Inference**:
  - DID (Difference-in-Differences)
  - ADID (Augmented DID) with additional control variables
  - SDID (Synthetic DID) for robust panel data analysis
- **Model Validation**: Using AIC, BIC, RMSE, MAE, standard error
- **Data Visualization**: Revenue trends, quantity sold, frequency, price changes, and recency

## Libraries

- dplyr
- ggplot2
- caret
- arrow
- synthdid
- mclust
- writexl
- car

## Workflow

1. Cleaning and Partitioning:
   - Detect and remove extreme outliers in revenue and quantity sold
   - Correct negative or unrealistic values
   - Create partitions to parallelize cleaning across customers
   
2. Customer Feature Engineering:
   - Initial Expenditure Level (Light, Medium, Heavy buyer)
   - Churn identification
   - Recency calculation
   
3. Modeling and Analysis:
   - DID, ADID, and SDID applied per buyer segment and full sample
   - Validation on hold-out samples (train/validation split)

4. Visualization:
   - Bar charts summarizing revenue, average quantity sold, price, frequency, and recency
   - Separate plots for redeemers vs. non-redeemers

5. Saving Outputs:
   - Model validation results exported to Excel
   - Descriptive summaries for each dataset

## Datasets

- **Dataset 1**: Retailer 1, multiple partitions
- **Dataset 2**: Retailer 2, Program 1 (larger and subset samples)
- **Dataset 3**: Retailer 2, Program 2 (larger and subset samples)

Each dataset was cleaned separately before modeling and evaluation.

## Key Outputs

- Cleaned customer datasets
- DID, ADID, and SDID model results (AIC, BIC, RMSE, MAE, Standard Errors)
- Descriptive statistics per segment (Revenue, Quantity, Price, Frequency, Recency)
- Visualizations comparing redeemers and non-redeemers
- Excel reports summarizing descriptive and model performance metrics

## How to Run

1. Install required libraries:
```r
install.packages(c("dplyr", "ggplot2", "caret", "arrow", "synthdid", "mclust", "writexl", "car"))
```
2. Set your local paths to load the parquet datasets.

3. Source the R functions:
```r
source("path_to_cleaning_functions.R")
source("path_to_modeling_functions.R")
```
4. Load datasets, clean partitions, and split into training and validation sets.
5. Run DID, ADID, and SDID Models:
```r
TotalResults <- DiD_function(train_set, validation_set, vector_of_outcomes, "DiD", "dataset_name")
TotalResults <- ADiD_function(train_set, validation_set, vector_of_outcomes, "ADiD", "dataset_name", c("medium_buyer","heavy_buyer"))
TotalResults <- SDiD_function(train_set, validation_set, vector_of_outcomes, "SDiD", "dataset_name")
```
6. Export results:
```r
writexl::write_xlsx(TotalResults, "output_file_path.xlsx")
```
7. Create visualisations:
```r
Revenue_graph(clean_dataset, "Title Name")
Avg_quantity_sold_graph(clean_dataset, "Title Name")
```
