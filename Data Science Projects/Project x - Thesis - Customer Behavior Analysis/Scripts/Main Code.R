## Cleaning the dataset
rm(list=ls())

set.seed(1234)
## Basic settings
options(scipen = 999)

## Loading the Libraries
library(synthdid) 
library(arrow)
library(dplyr)
library(mclust)
library(ggplot2)
library(caret)
library(writexl)
library(car)
#library(mlogit)

# Create vector with all dependent variables
vector_of_outcomes <- c("total_revenue","average_quantity_sold","frequency","average_price","recency")

# Create dataframe to store validation results
TotalResults = data.frame("Dataset" = character() 
                                , "Analysis_name" = character()
                                , "Group"=character()
                                , "Dependent_variable" = character()
                                , "Dataset"= character()
                                , "AIC"=double(),"BIC"=double()
                                , "IN_RMSE"=double(),"IN_MAE"=double()
                                , "OUT_RMSE"=double(),"OUT_MAE"=double()
                                , "SE"=double()
                                , "Estimation"=double())


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
### BEGIN  CLEANING-FUNCTIONS ### BEGIN  CLEANING-FUNCTIONS ### BEGIN  CLEANING-FUNCTIONS ## ----
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
replace_null_with_zero <- function(x) { 
  ifelse(is.na(x), 0, x) 
}

# Clean the dataset divided into n partitions
clean_partitions <- function(df,n=20){
  start.time <- Sys.time()
  
  print("step 1/8")
  
  # Get min-max dates
  # Print them to console
  cat(paste(" period 1: " , min(df$date_code[df$period_code == "1"]) , " = ", max(df$date_code[df$period_code == "1"])),"\n" ,
      paste("period 2: " , min(df$date_code[df$period_code == "2"]) , " = ", max(df$date_code[df$period_code == "2"])),"\n" ,
      paste("period 3: " , min(df$date_code[df$period_code == "3"]) , " = ", max(df$date_code[df$period_code == "3"])),"\n" ,
      paste("period 4: " , min(df$date_code[df$period_code == "4"]) , " = ", max(df$date_code[df$period_code == "4"])),"\n") 
  # Save the dates into variables
  period1mindate = as.numeric(as.Date(toString(min(df$date_code[df$period_code == "1"])), format = '%Y%m%d'))
  period1maxdate = as.numeric(as.Date(toString(max(df$date_code[df$period_code == "1"])), format = '%Y%m%d'))
  period2mindate = as.numeric(as.Date(toString(min(df$date_code[df$period_code == "2"])), format = '%Y%m%d'))
  period2maxdate = as.numeric(as.Date(toString(max(df$date_code[df$period_code == "2"])), format = '%Y%m%d'))
  period3mindate = as.numeric(as.Date(toString(min(df$date_code[df$period_code == "3"])), format = '%Y%m%d'))
  period3maxdate = as.numeric(as.Date(toString(max(df$date_code[df$period_code == "3"])), format = '%Y%m%d'))
  period4mindate = as.numeric(as.Date(toString(min(df$date_code[df$period_code == "4"])), format = '%Y%m%d'))
  period4maxdate = as.numeric(as.Date(toString(max(df$date_code[df$period_code == "4"])), format = '%Y%m%d'))
  
  
  print("step 2/8")
  
  
  hist(df$revenue_after_discount_incl_vat)
  # Outliers in Revenue
  quantile(df$revenue_after_discount_incl_vat, prob= 0.999, type =1)
  df <- df %>% 
    filter(revenue_after_discount_incl_vat < quantile(df$revenue_after_discount_incl_vat, prob= 0.999, type =1))
  # negative revenue
  df <- df %>%
    filter(revenue_after_discount_incl_vat > 0)
  # Outliers in Quantity Sold 
  hist(df$quantity_sold)
  # Outliers in Quantity_sold
  quantile(df$quantity_sold, prob= 0.999, type =1)
  df <- df %>% 
    filter(quantity_sold < quantile(df$quantity_sold, prob= 0.999, type =1))
  # Small quantities sold
  summary(df[df$quantity_sold < 1,])
  df <- df %>% 
    mutate(quantity_sold = ifelse(quantity_sold < 1 , 1 , quantity_sold))
  
  
  print("step 3/8")
  
  
  # Set dummies for each period
  df_check_periods <- df %>%
    mutate(perioda = ifelse(period_code == 1, 1, 0),
           periodb = ifelse(period_code == 2, 1, 0),
           periodc = ifelse(period_code == 3, 1, 0),
           periodd = ifelse(period_code == 4, 1, 0),
    ) %>%
    group_by(customer_code) %>%
    summarise(period1 = sum(perioda),
              period2 = sum(periodb),
              period3 = sum(periodc),
              period4 = sum(periodd)) 
  
  # Create a list for which either period 4 is empty, or period 1 2 and 3 are empty
  df_omit_customers_no_purchases <- df_check_periods %>%
    filter( (period1 == 0 & period2 == 0 & period3 == 0)) %>%
    select(customer_code)
  # Delete the customer that have no purchase history whatsoever
  df <- df%>%
    filter(!customer_code %in% df_omit_customers_no_purchases$customer_code) 
  
  
  print("step 4/8 ")
  
  # Sanitary check
  print(sum(df$revenue_after_discount_incl_vat))
  
  
  

  # Create list for each customer with redeem status
  customer_info <- df %>% distinct(customer_code, redeemer_latest_ind)
  # Get all unique customers and periods
  customers <- unique(df$customer_code)
  periods <- unique(df$period_code)
  # Join all customers with all periods, left joining the redeeming status
  reference_Table = expand.grid(customer_code = customers, period_code = periods) %>% left_join(customer_info, by= "customer_code")  
  # Join the new table with a period for each customer with the whole data set
  results <- reference_Table %>%
    left_join(df, by = c("customer_code", "period_code")) %>%
    select(date_code,period_code,redeemer_latest_ind.x,customer_code,revenue_after_discount_incl_vat,quantity_sold) %>%
    rename(redeemer_latest_ind = "redeemer_latest_ind.x")
  # Due to missing periods, we have missing values. We replace them with zero
  df <- results %>% mutate_all(replace_null_with_zero)
  
  
  print("step 5/8")
  
  
  # Group the dataset on revenue per customer
  df_grouped_past_revenue <- df %>%
    filter(period_code != 4) %>%
    group_by(customer_code) %>%
    summarize(total_revenue = sum(revenue_after_discount_incl_vat))
  # See the revenue per customer distribution
  hist(df_grouped_past_revenue$total_revenue, breaks=60)
  # Group the customers based on the quantiles
  # Check the groups
  df_grouped_past_revenue %>%
    mutate(group = ifelse(total_revenue < quantile(df_grouped_past_revenue$total_revenue,.80), 1,
                          ifelse(total_revenue > quantile(df_grouped_past_revenue$total_revenue,.95),3,2))) %>%
    group_by(group) %>%
    summarise(sum(total_revenue))
  # Create the groups
  df <- df_grouped_past_revenue %>%
    mutate(group = ifelse(total_revenue < quantile(df_grouped_past_revenue$total_revenue,.80), 1,
                          ifelse(total_revenue > quantile(df_grouped_past_revenue$total_revenue,.95),3,2))) %>%
    select(customer_code,group) %>%
    full_join(df, by = "customer_code")

  
  print("step 6/8")
  
  # Create partitions of the dataframe
  list_partitions <- create_partition_list(df,n)
  
  print("Step 7/8")
  
  # Create empty dataframe
  df_transformed <- data.frame()
  
  # Do the cleaning per partition
  for (i in list_partitions){
    df_r2_groups_1 <- as.data.frame(i) %>%
      group_by( customer_code, 
                period_code) %>%
      summarize(initial_expenditure_level     = mean(group),
                redeemer_latest_ind           = mean(redeemer_latest_ind),
                total_revenue                 = sum(revenue_after_discount_incl_vat),
                average_quantity_sold         = mean(quantity_sold),
                quantity_sold                 = sum(quantity_sold),
                frequency                     = n(),
                recency_last_date             = as.numeric(as.Date(toString(max(date_code)), format="%Y%m%d")) )
    
    #Churning
    #customers_to_churn <- df_r2_groups_1[df_r2_groups_1$period_code == 4 & df_r2_groups_1$total_revenue == 0, "customer_code"]
    
    df_r2_groups_2 <- df_r2_groups_1 %>%
      mutate(   average_price                 = ifelse(total_revenue == 0 & quantity_sold ==0, 0, total_revenue / quantity_sold),
                light_buyer                   = +(initial_expenditure_level == 1),
                medium_buyer                  = +(initial_expenditure_level == 2),
                heavy_buyer                   = +(initial_expenditure_level == 3),
                frequency                     = ifelse(quantity_sold == 0 , 0 , frequency),
                #churner                       = +(customer_code %in% customers_to_churn$customer_code), #churning
                churner                       = +(total_revenue == 0),
                recency                       = ifelse(is.na(recency_last_date), 140, 
                                                       ifelse(period_code == 4, period4maxdate - recency_last_date,
                                                              ifelse(period_code == 3, period3maxdate - recency_last_date, 
                                                                     ifelse(period_code == 2, period2maxdate - recency_last_date, 
                                                                            period1maxdate - recency_last_date)))),
                sdid_treatment                = +(period_code == 4 & redeemer_latest_ind == 1),
                period1                       = +(period_code == 1),
                period2                       = +(period_code == 2),
                period3                       = +(period_code == 3),
                period4                       = +(period_code == 4))
    
    df_transformed <- rbind(as.data.frame(df_transformed),as.data.frame(df_r2_groups_2))
    print("Succesful run")
    print(length(df_transformed$customer_code))
    
  }
  print("Step 8/8")
  end.time <- Sys.time()
  time.taken <- round(end.time - start.time,2)
  print(time.taken)
  return (df_transformed)
}


# A function to return a list with partitions
# Partition will be done based on customer codes
# This way, a customer will be only present in one partition
# and not spread among different partitions
# A function to return a list with partitions
create_partition_list <- function(df, n=20){
  
  unique_customers <-as.data.frame(unique(df$customer_code))
  sanitary_check = 0
  list_of_partitions <- list()
  
  for ( i in 1:n){
    if ( i == 1 ) {
      partition = df[df$customer_code <= quantile(unique_customers$`unique(df$customer_code)`,i/n),]
    } else {
      partition = df[df$customer_code > quantile(unique_customers$`unique(df$customer_code)`,(i-1)/n) 
                     & df$customer_code <= quantile(unique_customers$`unique(df$customer_code)`,i/n),]
    }
    list_of_partitions[[length(list_of_partitions)+1]] <- list(partition)
    sanitary_check = sanitary_check + sum(list_of_partitions[[i]][[1]][[6]])
    print(sanitary_check)
  }
  
  
  return(list_of_partitions)
  
  
}


## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## #
###   DID-ANALYSIS ### BEGIN DID-ANALYSIS ### BEGIN DID-ANALYSIS ### ----
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## #
DiD_function <- function(train_dataset, validation_dataset, vector_of_outcomes,results_name, name_dataset){
  
  
  list_of_results = list()
  # Create dataframe to store validation results
  validation_results = data.frame("Dataset" = character() 
                            , "Analysis_name" = character()
                            , "Group"=character()
                            , "Dependent_variable" = character()
                            , "Dataset"= character()
                            , "AIC"=double(),"BIC"=double()
                            , "IN_RMSE"=double(),"IN_MAE"=double()
                            , "OUT_RMSE"=double(),"OUT_MAE"=double()
                            , "SE"=double()
                            , "Estimation"=double())
  
  
  # Create loop for all datasets to do DiD analysis based on all outcomes and each initial expenditure levels
  for( df in list(train_dataset)){
    
    for ( i in vector_of_outcomes ) {
      
      # Light buyer
      model_revenue_light <- lm(df[df$light_buyer == 1,i]   ~ period2 + period3 + period4 + redeemer_latest_ind + period4*redeemer_latest_ind, data =  df[df$light_buyer  == 1,])
      list_of_results[paste0("light_",i,results_name)] = list(model_revenue_light)
      
      # Medium Buyer
      model_revenue_medium <- lm(df[df$medium_buyer == 1,i] ~ period2 + period3 + period4 + redeemer_latest_ind + period4*redeemer_latest_ind, data =  df[df$medium_buyer == 1,])
      list_of_results[paste0("medium_",i,results_name)] = list(model_revenue_medium)
      
      # Heavy Buyer
      model_revenue_heavy <- lm(df[df$heavy_buyer == 1,i]   ~ period2 + period3 + period4 + redeemer_latest_ind + period4*redeemer_latest_ind, data =  df[df$heavy_buyer  == 1,])
      list_of_results[paste0("heavy_",i,results_name)] = list(model_revenue_heavy)
      
      
      message("\n Beginning of the results of " , i)
      
      
      # Do an interation on the outcomes for each initial expenditure level
      for ( j in c("light_","medium_","heavy_"))  {
        
        # Create a new row to store results
        validation_results <- rbind(validation_results, data.frame("Dataset" = name_dataset
                                                                   , "Analysis_name"= results_name
                                                                   , "Group" = j
                                                                   , "Dependent_variable" = i
                                                                   , "AIC"=0.
                                                                   , "BIC"=0.
                                                                   , "IN_RMSE"=0.
                                                                   , "IN_MAE"=0.
                                                                   , "OUT_RMSE"=0.
                                                                   , "OUT_MAE"=0.
                                                                   , "SE"=0.
                                                                   , "Estimation"=0))
        
        
        
        # Create models for validation
        in_sample_model <- predict(list_of_results[[paste0(j,i,results_name)]], train_dataset)
        validation_model <- predict(list_of_results[[paste0(j,i,results_name)]], validation_dataset)
        print(vif(list_of_results[[paste0(j,i,results_name)]], type = "predictor"))
        # Store the results
        validation_results$AIC        [validation_results$Group == j & validation_results$Dependent_variable == i] <- AIC  (list_of_results[[paste0(j,i,results_name)]])
        validation_results$BIC        [validation_results$Group == j & validation_results$Dependent_variable == i] <- BIC  (list_of_results[[paste0(j,i,results_name)]])
        validation_results$IN_RMSE    [validation_results$Group == j & validation_results$Dependent_variable == i] <- RMSE (in_sample_model, train_dataset[, i])
        validation_results$IN_MAE     [validation_results$Group == j & validation_results$Dependent_variable == i] <- MAE  (in_sample_model, train_dataset[, i])
        validation_results$OUT_RMSE   [validation_results$Group == j & validation_results$Dependent_variable == i] <- RMSE (validation_model, validation_dataset[, i])
        validation_results$OUT_MAE    [validation_results$Group == j & validation_results$Dependent_variable == i] <- MAE  (validation_model, validation_dataset[, i])
        validation_results$Estimation [validation_results$Group == j & validation_results$Dependent_variable == i] <- coef (summary(list_of_results[[paste0(j,i,results_name)]]))["period4:redeemer_latest_ind", "Estimate"]
        validation_results$SE         [validation_results$Group == j & validation_results$Dependent_variable == i] <- coef (summary(list_of_results[[paste0(j,i,results_name)]]))["period4:redeemer_latest_ind", "Std. Error"]
        
        # Print out summary
        print(summary(list_of_results[[paste0(j,i,results_name)]]))
        
        # Print Out validation results
        cat("\n AIC: ",paste(paste0(j,i,results_name) , validation_results$AIC     [validation_results$Group == j & validation_results$Dependent_variable == i]), " \n")
        cat(" BIC: " , paste(paste0(j,i,results_name) , validation_results$BIC     [validation_results$Group == j & validation_results$Dependent_variable == i]), " \n")
        in_sample_model <- predict(list_of_results[[paste0(j,i,results_name)]], train_dataset)
        cat("\n RMSE in  sample: ", validation_results$IN_RMSE [validation_results$Group == j & validation_results$Dependent_variable == i])
        cat("\n MAE  in  sample: ", validation_results$IN_MAE  [validation_results$Group == j & validation_results$Dependent_variable == i])
        cat("\n RMSE out sample: ", validation_results$OUT_RMSE[validation_results$Group == j & validation_results$Dependent_variable == i])
        cat("\n MAE  out sample: ", validation_results$OUT_MAE [validation_results$Group == j & validation_results$Dependent_variable == i])
        cat(" \n\n------------------------------\n\n\n")
        
        
      }
      
      message("\n End of the results of " , i)
      
    }
    
  }
  
  return(validation_results)
}



## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## #
###   ADID-ANALYSIS ### BEGIN ADID-ANALYSIS ### BEGIN ADID-ANALYSIS ### ---- 
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## #
ADiD_function <- function(train_dataset, validation_dataset, vector_of_outcomes,results_name, name_dataset,control_variables){
  
  
  list_of_results = list()
  
  # Create dataframe to store validation results
  validation_results = data.frame("Dataset" = character() 
                                  , "Analysis_name" = character()
                                  , "Group"=character()
                                  , "Dependent_variable" = character()
                                  , "Dataset"= character()
                                  , "AIC"=double(),"BIC"=double()
                                  , "IN_RMSE"=double(),"IN_MAE"=double()
                                  , "OUT_RMSE"=double(),"OUT_MAE"=double()
                                  , "SE"=double()
                                  , "Estimation"=double())
  

  
  
  # Create loop for all datasets to do DiD analysis based on all outcomes and each initial expenditure levels
  for( df in list(train_dataset)){
    
    
    for ( i in vector_of_outcomes ) {
      
      # Create all control Variables where leaving out the current variable
      control_variables = data.frame(control_variables = control_variables)
      control_variables = control_variables %>% filter(!control_variables == i)
      
      # Light buyer
      formula_light_buyer <- as.formula(paste("df[df$light_buyer == 1,i] ~ period2 + period3 + period4 + redeemer_latest_ind + period4*redeemer_latest_ind + ", paste(control_variables$control_variables, collapse= "+")))
      model_light <- lm(formula = formula_light_buyer, data =  df[df$light_buyer == 1,])
      list_of_results[paste0("light_",i,results_name)] = list(model_light)
      
      # Medium Buyer
      formula_medium_buyer <- as.formula(paste("df[df$medium_buyer == 1,i] ~ period2 + period3 + period4 + redeemer_latest_ind + period4*redeemer_latest_ind + ", paste(control_variables$control_variables, collapse= "+")))
      model_medium <- lm(formula = formula_medium_buyer, data =  df[df$medium_buyer == 1,])
      list_of_results[paste0("medium_",i,results_name)] = list(model_medium)
      
      # Heavy Buyer
      formula_heavy_buyer <- as.formula(paste("df[df$heavy_buyer == 1,i] ~ period2 + period3 + period4 + redeemer_latest_ind + period4*redeemer_latest_ind + ", paste(control_variables$control_variables, collapse= "+")))
      model_heavy <- lm(formula = formula_heavy_buyer, data =  df[df$heavy_buyer == 1,])
      list_of_results[paste0("heavy_",i,results_name)] = list(model_heavy)
      
      # Heavy Buyer
      formula_total <- as.formula(paste("df[df$heavy_buyer == 1 || df$heavy_buyer == 0,i] ~ period2 + period3 + period4 + redeemer_latest_ind + period4*redeemer_latest_ind + ", paste(control_variables$control_variables, collapse= "+")))
      print(formula_total)
      model_total <- lm(formula = formula_total, data =  df[df$heavy_buyer == 1 || df$heavy_buyer == 0,])
      list_of_results[paste0("total_",i,results_name)] = list(model_total)
      
    }
    
  }
  
  
  # Show validation of all saved outcomes
  for ( i in vector_of_outcomes ) {
    
    message("\n Beginning of the results of " , i)
    
    # Do an interation on the outcomes for each initial expenditure level
    for ( j in c(#"light_","medium_","heavy_",
                 "total_"))  {
      
      # Create a new row to store results
      validation_results <- rbind(validation_results, data.frame("Dataset" = name_dataset
                                                                 , "Analysis_name"= results_name
                                                                 , "Group" = j
                                                                 , "Dependent_variable" = i
                                                                 , "AIC"=0.
                                                                 , "BIC"=0.
                                                                 , "IN_RMSE"=0.
                                                                 , "IN_MAE"=0.
                                                                 , "OUT_RMSE"=0.
                                                                 , "OUT_MAE"=0.
                                                                 , "SE"=0.
                                                                 , "Estimation"=0))
      # Print summary
      print(summary(list_of_results[[paste0(j,i,results_name)]]))
      print(vif(list_of_results[[paste0(j,i,results_name)]]))
      
      # Create models for validation
      in_sample_model <- predict(list_of_results[[paste0(j,i,results_name)]], train_dataset)
      validation_model <- predict(list_of_results[[paste0(j,i,results_name)]], validation_dataset)
      
      # Store the results
      validation_results$AIC     [validation_results$Group == j & validation_results$Dependent_variable == i] <- AIC  (list_of_results[[paste0(j,i,results_name)]])
      validation_results$BIC     [validation_results$Group == j & validation_results$Dependent_variable == i] <- BIC  (list_of_results[[paste0(j,i,results_name)]])
      validation_results$IN_RMSE [validation_results$Group == j & validation_results$Dependent_variable == i] <- RMSE (in_sample_model, train_dataset[, i])
      validation_results$IN_MAE  [validation_results$Group == j & validation_results$Dependent_variable == i] <- MAE  (in_sample_model, train_dataset[, i])
      validation_results$OUT_RMSE[validation_results$Group == j & validation_results$Dependent_variable == i] <- RMSE (validation_model, validation_dataset[, i])
      validation_results$OUT_MAE [validation_results$Group == j & validation_results$Dependent_variable == i] <- MAE  (validation_model, validation_dataset[, i])
      validation_results$Estimation [validation_results$Group == j & validation_results$Dependent_variable == i] <- coef (summary(list_of_results[[paste0(j,i,results_name)]]))["period4:redeemer_latest_ind", "Estimate"]
      validation_results$SE         [validation_results$Group == j & validation_results$Dependent_variable == i] <- coef (summary(list_of_results[[paste0(j,i,results_name)]]))["period4:redeemer_latest_ind", "Std. Error"]
      
      # Print AIC & BIC
      cat("\n AIC: ",paste(paste0(j,i,results_name) , validation_results$AIC     [validation_results$Group == j & validation_results$Dependent_variable == i]), " \n")
      cat(" BIC: " , paste(paste0(j,i,results_name) , validation_results$BIC     [validation_results$Group == j & validation_results$Dependent_variable == i]), " \n")
      
      # print in-sample RMSE & MAE
      in_sample_model <- predict(list_of_results[[paste0(j,i,results_name)]], train_dataset)
      cat("\n RMSE in  sample: ", validation_results$IN_RMSE [validation_results$Group == j & validation_results$Dependent_variable == i])
      cat("\n MAE  in  sample: ", validation_results$IN_MAE  [validation_results$Group == j & validation_results$Dependent_variable == i])
      
      # Print out-sample RMSE & MAE
      cat("\n RMSE out sample: ", validation_results$OUT_RMSE[validation_results$Group == j & validation_results$Dependent_variable == i])
      cat("\n MAE  out sample: ", validation_results$OUT_MAE [validation_results$Group == j & validation_results$Dependent_variable == i])
      cat(" \n\n------------------------------\n\n\n")
      
      
    }
    
    message("\n End of the results of " , i)
    
  }
  validation_results$Dataset <- name_dataset
  return(validation_results)
}






## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## #
###   SDID-ANALYSIS ### BEGIN SDID-ANALYSIS ### BEGIN SDID-ANALYSIS ### ---- 
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## #
SDiD_function <- function(train_dataset, validation_dataset, vector_of_outcomes,results_name, name_dataset, iterations=5000){
  start.time2 <- Sys.time()
  list_of_results = list()
  
  # Create dataframe to store validation results
  validation_results = data.frame("Dataset" = character() 
                                  , "Analysis_name" = character()
                                  , "Group"=character()
                                  , "Dependent_variable" = character()
                                  , "Dataset"= character()
                                  , "AIC"=double(),"BIC"=double()
                                  , "IN_RMSE"=double(),"IN_MAE"=double()
                                  , "OUT_RMSE"=double(),"OUT_MAE"=double()
                                  , "SE"=double()
                                  , "Estimation"=double())
  
  for( df in list(train_dataset)){
    
    for ( i in vector_of_outcomes ) {
      SDID.start.time <- Sys.time()
      # Light buyer
      setup <- panel.matrices(df[df$light_buyer == 1,], unit = 'customer_code', time = 'period_code', outcome = i, treatment = 'sdid_treatment')
      model_sdid_light = synthdid_estimate(setup$Y, setup$N0, setup$T0, max.iter = iterations)
      list_of_results[paste0("light_",i,results_name)] = list(model_sdid_light)
      
      # Medium Buyer
      setup <- panel.matrices(df[df$medium_buyer == 1,], unit = 'customer_code', time = 'period_code', outcome = i, treatment = 'sdid_treatment')
      model_sdid_light = synthdid_estimate(setup$Y, setup$N0, setup$T0, max.iter = iterations)
      list_of_results[paste0("medium_",i,results_name)] = list(model_sdid_light)
      
      # Heavy buyer
      setup <- panel.matrices(df[df$heavy_buyer == 1,], unit = 'customer_code', time = 'period_code', outcome = i, treatment = 'sdid_treatment')
      model_sdid_light = synthdid_estimate(setup$Y, setup$N0, setup$T0, max.iter = iterations)
      list_of_results[paste0("heavy_",i,results_name)] = list(model_sdid_light)
      print(round(Sys.time() - SDID.start.time,2))
    }
    
  }
  
  # Show validation of all saved outcomes
  for ( i in vector_of_outcomes ) {
    
    message("\n Beginning of the results of " , i)
    SDID.start.time <- Sys.time()
    for ( j in c("light_","medium_","heavy_"))  {
      
      
      # Create a new row to store results
      validation_results <- rbind(validation_results, data.frame("Dataset" = name_dataset
                                                                 , "Analysis_name"= results_name
                                                                 , "Group" = j
                                                                 , "Dependent_variable" = i
                                                                 , "AIC"=0.
                                                                 , "BIC"=0.
                                                                 , "IN_RMSE"=0.
                                                                 , "IN_MAE"=0.
                                                                 , "OUT_RMSE"=0.
                                                                 , "OUT_MAE"=0.
                                                                 , "SE"=0.
                                                                 , "Estimation"=0))     
        
      
      print(paste0(j,i))
      
      print(list_of_results[[paste0(j,i,results_name)]])
      validation_results$IN_RMSE     [validation_results$Group == j & validation_results$Dependent_variable == i] <- min(synthdid_rmse_plot(list_of_results[[paste0(j,i,results_name)]])$data$rmse)
      validation_results$SE          [validation_results$Group == j & validation_results$Dependent_variable == i] <- sqrt(vcov(list_of_results[[paste0(j,i,results_name)]], method = 'jackknife'))
      
      print(validation_results$IN_RMSE     [validation_results$Group == j & validation_results$Dependent_variable == i])
      print(validation_results$SE          [validation_results$Group == j & validation_results$Dependent_variable == i])
      
    }
    print(round(Sys.time() - SDID.start.time,2))
    message("\n End of the results of " , i)
    
  }
  end.time2 <- Sys.time()
  time.taken2 <- round(end.time2 - start.time2,2)
  print(time.taken2)
  return(validation_results)
}

### Visuals ----
# Revenue Graph
Revenue_graph <- function(dataset, titlename) {
  revenue_dataset3 <- dataset %>%
    group_by(period_code,redeemer_latest_ind) %>%
    summarize(revenuesum =sum(total_revenue))
  
  revenue_dataset3b <- dataset %>%
    group_by(period_code,redeemer_latest_ind) %>%
    summarize(revenuesum =sum(total_revenue)) %>%
    mutate(redeemer_latest_ind = -1)
  
  revenue_dataset3 <- rbind(revenue_dataset3b,revenue_dataset3)
  
  
  ggplot(revenue_dataset3, aes(x = factor(redeemer_latest_ind, levels = c(-1,0,1), labels=c("total","non-redeemer","redeemer")), y= revenuesum, 
                               fill=factor(redeemer_latest_ind, levels = c(-1,0,1), labels=c("total","non-redeemer","redeemer")))) +
    geom_bar( stat = "identity") +
    facet_grid(~ factor(period_code)) +
    theme(legend.position="bottom",axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
    xlab("Periods") +
    ylab("Revenue in units") +
    scale_fill_discrete(name="") +
    labs(title=titlename)
  
}

# Avg_quantity_sold Graph
Avg_quantity_sold_graph <- function(dataset, titlename) {
  revenue_dataset3 <- dataset %>%
    group_by(period_code,redeemer_latest_ind) %>%
    summarize(revenuesum = mean(average_quantity_sold))
  
  revenue_dataset3b <- dataset %>%
    group_by(period_code) %>%
    summarize(revenuesum = mean(average_quantity_sold)) %>%
    mutate(redeemer_latest_ind = -1)
  
  revenue_dataset3 <- rbind(revenue_dataset3b,revenue_dataset3)
  
  
  ggplot(revenue_dataset3, aes(x = factor(redeemer_latest_ind, levels = c(-1,0,1), labels=c("total","non-redeemer","redeemer")), y= revenuesum, 
                               fill=factor(redeemer_latest_ind, levels = c(-1,0,1), labels=c("total","non-redeemer","redeemer")))) +
    geom_bar( stat = "identity") +
    facet_grid(~ factor(period_code)) +
    theme(legend.position="bottom",axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
    xlab("Periods") +
    ylab("Avg Quantity in units") +
    scale_fill_discrete(name="") +
    labs(title=titlename)
  
}

# Frequency Graph
frequency_graph <- function(dataset, titlename) {
  revenue_dataset3 <- dataset %>%
    group_by(period_code,redeemer_latest_ind) %>%
    summarize(revenuesum =mean(frequency))
  
  revenue_dataset3b <- dataset %>%
    group_by(period_code) %>%
    summarize(revenuesum =mean(frequency)) %>%
    mutate(redeemer_latest_ind = -1)
  
  revenue_dataset3 <- rbind(revenue_dataset3b,revenue_dataset3)
  
  
  ggplot(revenue_dataset3, aes(x = factor(redeemer_latest_ind, levels = c(-1,0,1), labels=c("total","non-redeemer","redeemer")), y= revenuesum, 
                               fill=factor(redeemer_latest_ind, levels = c(-1,0,1), labels=c("total","non-redeemer","redeemer")))) +
    geom_bar( stat = "identity") +
    facet_grid(~ factor(period_code)) +
    theme(legend.position="bottom",axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
    xlab("Periods") +
    ylab("Frequency in units") +
    scale_fill_discrete(name="") +
    labs(title=titlename)
  
}

# average_price Graph
average_price_graph <- function(dataset, titlename) {
  revenue_dataset3 <- dataset %>%
    group_by(period_code,redeemer_latest_ind) %>%
    summarize(revenuesum =mean(average_price))
  
  revenue_dataset3b <- dataset %>%
    group_by(period_code) %>%
    summarize(revenuesum =mean(average_price)) %>%
    mutate(redeemer_latest_ind = -1)
  
  revenue_dataset3 <- rbind(revenue_dataset3b,revenue_dataset3)
  
  
  ggplot(revenue_dataset3, aes(x = factor(redeemer_latest_ind, levels = c(-1,0,1), labels=c("total","non-redeemer","redeemer")), y= revenuesum, 
                               fill=factor(redeemer_latest_ind, levels = c(-1,0,1), labels=c("total","non-redeemer","redeemer")))) +
    geom_bar( stat = "identity") +
    facet_grid(~ factor(period_code)) +
    theme(legend.position="bottom",axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
    xlab("Periods") +
    ylab("Average Price in units") +
    scale_fill_discrete(name="") +
    labs(title=titlename)
  
}

# recency Graph
recency_graph <- function(dataset, titlename) {
  revenue_dataset3 <- dataset %>%
    group_by(period_code,redeemer_latest_ind) %>%
    summarize(revenuesum =mean(recency))
  
  revenue_dataset3b <- dataset %>%
    group_by(period_code) %>%
    summarize(revenuesum =mean(recency)) %>%
    mutate(redeemer_latest_ind = -1)
  
  revenue_dataset3 <- rbind(revenue_dataset3b,revenue_dataset3)
  
  
  ggplot(revenue_dataset3, aes(x = factor(redeemer_latest_ind, levels = c(-1,0,1), labels=c("total","non-redeemer","redeemer")), y= revenuesum, 
                               fill=factor(redeemer_latest_ind, levels = c(-1,0,1), labels=c("total","non-redeemer","redeemer")))) +
    geom_bar( stat = "identity") +
    facet_grid(~ factor(period_code)) +
    theme(legend.position="bottom",axis.text.x=element_blank(),axis.ticks.x=element_blank()) +
    xlab("Periods") +
    ylab("Recency in units") +
    scale_fill_discrete(name="") +
    labs(title=titlename)
  
}



### Datasets ----
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
#### DATASET 1 ### BEGIN DATASET 1 ### BEGIN DATASET 1 ### BEGIN DATASET 1  ----
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
df_r1_part1 <- read_parquet('D:/Rijksuniversiteit Groningen/MADS/Thesis/Data/Retailer 1/part-00000-ffbea56e-a67e-49e8-b7c5-155ffa2d97de-c000.snappy.parquet')
df_r1_part2 <- read_parquet('D:/Rijksuniversiteit Groningen/MADS/Thesis/Data/Retailer 1/part-00001-ffbea56e-a67e-49e8-b7c5-155ffa2d97de-c000.snappy.parquet')
df_r1_part3 <- read_parquet('D:/Rijksuniversiteit Groningen/MADS/Thesis/Data/Retailer 1/part-00002-ffbea56e-a67e-49e8-b7c5-155ffa2d97de-c000.snappy.parquet')
df_r1_part4 <- read_parquet('D:/Rijksuniversiteit Groningen/MADS/Thesis/Data/Retailer 1/part-00003-ffbea56e-a67e-49e8-b7c5-155ffa2d97de-c000.snappy.parquet')
df_r1_part5 <- read_parquet('D:/Rijksuniversiteit Groningen/MADS/Thesis/Data/Retailer 1/part-00004-ffbea56e-a67e-49e8-b7c5-155ffa2d97de-c000.snappy.parquet')
df_r1 <- rbind(df_r1_part1,df_r1_part2,df_r1_part3,df_r1_part4,df_r1_part5)

 # Clean dataset
clean_df_program1 <- clean_partitions(df_r1,5)
 # Check redeeming customers
clean_df_program1 %>%
  filter(churner == 1, period_code == 4, redeemer_latest_ind == 1)

churners_datset1 <- clean_df_program1 %>%
  filter(churner == 1, period_code == 4, redeemer_latest_ind == 1) %>%
  select(customer_code)

clean_df_program1 <- clean_df_program1 %>%
  filter(! customer_code %in% churners_datset1$customer_code)


 # Create train and validation dataset
sampled_customers1           <- sample(unique(clean_df_program1$customer_code),size = length(unique(clean_df_program1$customer_code))*.8, replace = F)
train_clean_df_program1      <- clean_df_program1 %>% filter(customer_code %in% sampled_customers1)
validation_clean_df_program1 <- clean_df_program1 %>% filter(!customer_code %in% sampled_customers1)

 # Analyses
TotalResults <- rbind(TotalResults,    DiD_function(train_clean_df_program1,validation_clean_df_program1,vector_of_outcomes,"DiD","dataset1"))
TotalResults <- rbind(TotalResults,   ADiD_function(train_clean_df_program1,validation_clean_df_program1,vector_of_outcomes,"ADiD","dataset1",c("medium_buyer","heavy_buyer")))
TotalResults <- rbind(TotalResults,   SDiD_function(train_clean_df_program1,validation_clean_df_program1,vector_of_outcomes,"SDiD","dataset1"))

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##  
####  DATASET 2 ### BEGIN DATASET 2 ### BEGIN DATASET 2 ### BEGIN DATASET 2 ### ----
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
df_r2_p1 <- read_parquet('D:/Rijksuniversiteit Groningen/MADS/Thesis/Data/Retailer 2/Program 1/part-00000-7a8ad9fa-b658-470b-a58d-ce2ec6cdfa22-c000.snappy.parquet')

df_r2_p1 <- df_r2_p1 %>% rename(customer_code = "customer_card_number", redeemer_latest_ind = "Redeemer_ind")
df_r2_p1 <- df_r2_p1 %>% mutate(period_code = ifelse(period_code == 0,3,period_code))
df_r2_p1 <- na.omit(df_r2_p1)

# Clean Dataset
clean_df_program2 <- clean_partitions(df_r2_p1,20)

# Clean Churning Redeemers
clean_df_program2 %>%
  filter(churner == 1, period_code == 4, redeemer_latest_ind == 1)

churners_datset2 <- clean_df_program2 %>%
  filter(churner == 1, period_code == 4, redeemer_latest_ind == 1) %>%
  select(customer_code)

clean_df_program2 <- clean_df_program2 %>%
  filter(! customer_code %in% churners_datset2$customer_code)

  # Sample larger dataset
sampled_customers2a            <- sample(unique(clean_df_program2$customer_code) ,size = length(unique(clean_df_program2$customer_code))*.80, replace = F)
train_clean_df_program2a      <- clean_df_program2 %>% filter( customer_code %in% sampled_customers2a)
validation_clean_df_program2a <- clean_df_program2 %>% filter(!customer_code %in% sampled_customers2a)

TotalResults <- rbind(TotalResults,    DiD_function(train_clean_df_program2a,validation_clean_df_program2a,vector_of_outcomes, "DiD","dataset2_full"))
TotalResults <- rbind(TotalResults,   ADiD_function(train_clean_df_program2a,validation_clean_df_program2a,vector_of_outcomes,"ADiD","dataset2_full",c("medium_buyer","heavy_buyer")))
  # Sample smaller dataset
sampled_customers2b            <- sample(unique(train_clean_df_program2a$customer_code) ,size = length(unique(train_clean_df_program2a$customer_code))*.0625, replace = F)
train_clean_df_program2b      <- clean_df_program2 %>% filter( customer_code %in% sampled_customers2b)

TotalResults <- rbind(TotalResults,    DiD_function(train_clean_df_program2b,validation_clean_df_program2a,vector_of_outcomes, "DiD","dataset2_part"))
TotalResults <- rbind(TotalResults,   ADiD_function(train_clean_df_program2b,validation_clean_df_program2a,vector_of_outcomes,"ADiD","dataset2_part",c("medium_buyer","heavy_buyer")))
TotalResults <- rbind(TotalResults,   SDiD_function(train_clean_df_program2b,validation_clean_df_program2a,vector_of_outcomes,"SDiD","dataset2_part", 3000))

## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
####  DATASET 3 ### BEGIN DATASET 3 ### BEGIN DATASET 3 ### BEGIN DATASET 3 ### ----
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ##
df_r2_p2 <- read_parquet('D:/Rijksuniversiteit Groningen/MADS/Thesis/Data/Retailer 2/Program 2/part-00000-484e5ff3-5729-42f4-846f-3710a2e88e9b-c000.snappy.parquet')

df_r2_p2 <- df_r2_p2 %>% rename(customer_code = "customer_card_number")
df_r2_p2 <- na.omit(df_r2_p2)

# Clean Dataset
clean_df_program3 <- clean_partitions(df_r2_p2,40)

# Clean Churning Redeemers
clean_df_program3 %>%
  filter(churner == 1, period_code == 4, redeemer_latest_ind == 1)

churners_datset3 <- clean_df_program3 %>%
  filter(churner == 1, period_code == 4, redeemer_latest_ind == 1) %>%
  select(customer_code)

clean_df_program3 <- clean_df_program3 %>%
  filter(! customer_code %in% churners_datset3$customer_code)

# Sample larger dataset
sampled_customers3a            <- sample(unique(clean_df_program3$customer_code),size = length(unique(clean_df_program3$customer_code))*.80, replace = F)
train_clean_df_program3a      <- clean_df_program3 %>% filter( customer_code %in% sampled_customers3a)
validation_clean_df_program3a <- clean_df_program3 %>% filter(!customer_code %in% sampled_customers3a)

TotalResults <- rbind(TotalResults,    DiD_function(train_clean_df_program3a,validation_clean_df_program3a,vector_of_outcomes, "DiD","dataset3_full"))
TotalResults <- rbind(TotalResults,   ADiD_function(train_clean_df_program3a,validation_clean_df_program3a,vector_of_outcomes,"ADiD","dataset3_full",c("medium_buyer","heavy_buyer")))

# Sample smaller dataset
sampled_customers3b            <- sample(unique(train_clean_df_program3a$customer_code),size = length(unique(train_clean_df_program3a$customer_code))*.0625, replace = F)
train_clean_df_program3b      <- clean_df_program3 %>% filter( customer_code %in% sampled_customers3b)

TotalResults <- rbind(TotalResults,    DiD_function(train_clean_df_program3b,validation_clean_df_program3a,vector_of_outcomes, "DiD","dataset3_part"))
TotalResults <- rbind(TotalResults,   ADiD_function(train_clean_df_program3b,validation_clean_df_program3a,vector_of_outcomes,"ADiD","dataset3_part",c("medium_buyer","heavy_buyer")))
TotalResults <- rbind(TotalResults,   SDiD_function(train_clean_df_program3b,validation_clean_df_program3a,vector_of_outcomes,"SDiD","dataset3_part",2000))

write_xlsx(TotalResults, 'D:/Rijksuniversiteit Groningen/MADS/Thesis/R code/output/20240613 Dataset1 output.xlsx')















# Descriptives dataset 1 

resulting <- clean_df_program1 %>%
  filter(total_revenue != 0,average_quantity_sold != 0,frequency != 0,average_price != 0) %>%
  group_by(period_code,initial_expenditure_level,redeemer_latest_ind) %>%
  summarize(sum_revenue = mean(total_revenue),
            mean_quantit = mean(average_quantity_sold),
            sum_frequen = mean(frequency),
            mean_price   = mean(average_price),
            mean_recency = mean(recency),
            n_total = n()) %>%
  arrange(redeemer_latest_ind,initial_expenditure_level)

write_xlsx(resulting, 'D:/Rijksuniversiteit Groningen/MADS/Thesis/R code/output/dataset1 descriptives ITL_REDb.xlsx')

resulting <- clean_df_program1 %>%
  filter(total_revenue != 0,average_quantity_sold != 0,frequency != 0,average_price != 0) %>%
  group_by(period_code,initial_expenditure_level) %>%
  summarize(sum_revenue = mean(total_revenue),
            mean_quantit = mean(average_quantity_sold),
            sum_frequen = mean(frequency),
            mean_price   = mean(average_price),
            mean_recency = mean(recency),
            n_total = n()) %>%
  arrange(initial_expenditure_level)

write_xlsx(resulting, 'D:/Rijksuniversiteit Groningen/MADS/Thesis/R code/output/dataset1 descriptives ITL.xlsx')

resulting <- clean_df_program1 %>%
  filter(total_revenue != 0,average_quantity_sold != 0,frequency != 0,average_price != 0) %>%
  group_by(period_code,redeemer_latest_ind) %>%
  summarize(sum_revenue = mean(total_revenue),
            mean_quantit = mean(average_quantity_sold),
            sum_frequen = mean(frequency),
            mean_price   = mean(average_price),
            mean_recency = mean(recency),
            n_total = n()) %>%
  arrange(redeemer_latest_ind)

write_xlsx(resulting, 'D:/Rijksuniversiteit Groningen/MADS/Thesis/R code/output/dataset1 descriptives REDb.xlsx')

resulting <- clean_df_program1 %>%
  filter(total_revenue != 0,average_quantity_sold != 0,frequency != 0,average_price != 0) %>%
  group_by(period_code) %>%
  summarize(sum_revenue = mean(total_revenue),
            mean_quantit = mean(average_quantity_sold),
            sum_frequen = mean(frequency),
            mean_price   = mean(average_price),
            mean_recency = mean(recency),
            n_total = n())

write_xlsx(resulting, 'D:/Rijksuniversiteit Groningen/MADS/Thesis/R code/output/dataset1 descriptives.xlsx')

# Descriptives

resulting <- clean_df_program2 %>%
  filter(total_revenue != 0,average_quantity_sold != 0,frequency != 0,average_price != 0) %>%
  group_by(period_code,initial_expenditure_level,redeemer_latest_ind) %>%
  summarize(sum_revenue = mean(total_revenue),
            mean_quantit = mean(average_quantity_sold),
            sum_frequen = mean(frequency),
            mean_price   = mean(average_price),
            mean_recency = mean(recency),
            n_total = n()) %>%
  arrange(redeemer_latest_ind,initial_expenditure_level)

write_xlsx(resulting, 'D:/Rijksuniversiteit Groningen/MADS/Thesis/R code/output/dataset2 descriptives ITL_RED.xlsx')

resulting <- clean_df_program2 %>%
  filter(total_revenue != 0,average_quantity_sold != 0,frequency != 0,average_price != 0) %>%
  group_by(period_code,initial_expenditure_level) %>%
  summarize(sum_revenue = mean(total_revenue),
            mean_quantit = mean(average_quantity_sold),
            sum_frequen = mean(frequency),
            mean_price   = mean(average_price),
            mean_recency = mean(recency),
            n_total = n()) %>%
  arrange(initial_expenditure_level)

write_xlsx(resulting, 'D:/Rijksuniversiteit Groningen/MADS/Thesis/R code/output/dataset2 descriptives ITL.xlsx')

resulting <- clean_df_program2 %>%
  filter(total_revenue != 0,average_quantity_sold != 0,frequency != 0,average_price != 0) %>%
  group_by(period_code,redeemer_latest_ind) %>%
  summarize(sum_revenue = mean(total_revenue),
            mean_quantit = mean(average_quantity_sold),
            sum_frequen = mean(frequency),
            mean_price   = mean(average_price),
            mean_recency = mean(recency),
            n_total = n()) %>%
  arrange(redeemer_latest_ind)

write_xlsx(resulting, 'D:/Rijksuniversiteit Groningen/MADS/Thesis/R code/output/dataset2 descriptives RED.xlsx')

resulting <- clean_df_program2 %>%
  filter(total_revenue != 0,average_quantity_sold != 0,frequency != 0,average_price != 0) %>%
  group_by(period_code) %>%
  summarize(sum_revenue = mean(total_revenue),
            mean_quantit = mean(average_quantity_sold),
            sum_frequen = mean(frequency),
            mean_price   = mean(average_price),
            mean_recency = mean(recency),
            n_total = n())

write_xlsx(resulting, 'D:/Rijksuniversiteit Groningen/MADS/Thesis/R code/output/dataset2 descriptives.xlsx')

#Descriptives

resulting <- clean_df_program3 %>%
  filter(total_revenue != 0,average_quantity_sold != 0,frequency != 0,average_price != 0) %>%
  group_by(period_code,initial_expenditure_level,redeemer_latest_ind) %>%
  summarize(sum_revenue = mean(total_revenue),
            mean_quantit = mean(average_quantity_sold),
            sum_frequen = mean(frequency),
            mean_price   = mean(average_price),
            mean_recency = mean(recency),
            n_total = n()) %>%
  arrange(redeemer_latest_ind,initial_expenditure_level)

write_xlsx(resulting, 'D:/Rijksuniversiteit Groningen/MADS/Thesis/R code/output/dataset3 descriptives ITL_RED.xlsx')

resulting <- clean_df_program3 %>%
  filter(total_revenue != 0,average_quantity_sold != 0,frequency != 0,average_price != 0) %>%
  group_by(period_code,initial_expenditure_level) %>%
  summarize(sum_revenue = mean(total_revenue),
            mean_quantit = mean(average_quantity_sold),
            sum_frequen = mean(frequency),
            mean_price   = mean(average_price),
            mean_recency = mean(recency),
            n_total = n()) %>%
  arrange(initial_expenditure_level)

write_xlsx(resulting, 'D:/Rijksuniversiteit Groningen/MADS/Thesis/R code/output/dataset3 descriptives ITL.xlsx')

resulting <- clean_df_program3 %>%
  filter(total_revenue != 0,average_quantity_sold != 0,frequency != 0,average_price != 0) %>%
  group_by(period_code,redeemer_latest_ind) %>%
  summarize(sum_revenue = mean(total_revenue),
            mean_quantit = mean(average_quantity_sold),
            sum_frequen = mean(frequency),
            mean_price   = mean(average_price),
            mean_recency = mean(recency),
            n_total = n()) %>%
  arrange(redeemer_latest_ind)

write_xlsx(resulting, 'D:/Rijksuniversiteit Groningen/MADS/Thesis/R code/output/dataset3 descriptives RED.xlsx')

resulting <- clean_df_program3 %>%
  filter(total_revenue != 0,average_quantity_sold != 0,frequency != 0,average_price != 0) %>%
  group_by(period_code) %>%
  summarize(sum_revenue = mean(total_revenue),
            mean_quantit = mean(average_quantity_sold),
            sum_frequen = mean(frequency),
            mean_price   = mean(average_price),
            mean_recency = mean(recency),
            n_total = n())

write_xlsx(resulting, 'D:/Rijksuniversiteit Groningen/MADS/Thesis/R code/output/dataset3 descriptives.xlsx')
df_total <- readxl::read_xlsx('D:/Rijksuniversiteit Groningen/MADS/Thesis/R code/input/Overall.xlsx')


ggplot(df_total, aes(Dataset,y,fill=Dataset)) +
  geom_bar(stat="identity") +
  facet_wrap (~factor(Control_variable, levels = c("Revenue","Quantity","Price","Frequency","Recency")), scales="free_y") +
  xlab("")+
  ylab("")


ggplot(data= clean_df_program1, aes(x=total_revenue, fill=factor(redeemer_latest_ind, levels = c(-1,0,1), labels=c("Total","non-redeemer","redeemer")))) +
  geom_histogram(show.legend = T,position="fill", bins = 1) 



  

# Graphs dataset 1
Revenue_graph(clean_df_program1, "Total Revenue Dataset 1")
Avg_quantity_sold_graph(clean_df_program1, "Average quantity sold Dataset 1")
frequency_graph(clean_df_program1, "Average frequency Dataset 1")
average_price_graph(clean_df_program1, "Average price Dataset 1")
Revenue_graph(clean_df_program1[clean_df_program1$heavy_buyer ==1,], "Total Revenue Dataset 1")
Revenue_graph(clean_df_program1[clean_df_program1$medium_buyer ==1,], "Total Revenue Dataset 1")
Revenue_graph(clean_df_program1[clean_df_program1$light_buyer ==1,], "Total Revenue Dataset 1")
Avg_quantity_sold_graph(clean_df_program1[clean_df_program1$heavy_buyer ==1,], "Average quantity sold Dataset 1")
Avg_quantity_sold_graph(clean_df_program1[clean_df_program1$medium_buyer ==1,], "Average quantity sold Dataset 1")
Avg_quantity_sold_graph(clean_df_program1[clean_df_program1$light_buyer ==1,], "Average quantity sold Dataset 1")
frequency_graph(clean_df_program1[clean_df_program1$heavy_buyer ==1,], "Average frequency Dataset 1")
frequency_graph(clean_df_program1[clean_df_program1$medium_buyer ==1,], "Average frequency Dataset 1")
frequency_graph(clean_df_program1[clean_df_program1$light_buyer ==1,], "Average frequency Dataset 1")
average_price_graph(clean_df_program1[clean_df_program1$heavy_buyer ==1,], "Average price Dataset 1")
average_price_graph(clean_df_program1[clean_df_program1$medium_buyer ==1,], "Average price Dataset 1")
average_price_graph(clean_df_program1[clean_df_program1$light_buyer ==1,], "Average price Dataset 1")
recency_graph(clean_df_program1[clean_df_program1$light_buyer == 1,], "Average recency Dataset 1 Light Users")
recency_graph(clean_df_program1[clean_df_program1$medium_buyer == 1,], "Average recency Dataset 1 Medium Users")
recency_graph(clean_df_program1[clean_df_program1$heavy_buyer == 1,], "Average recency Dataset 1 Heavy Users")
# Graphs dataset 2
Revenue_graph(clean_df_program2, "Total Revenue Dataset 2")
Avg_quantity_sold_graph(clean_df_program2, "Average quantity sold Dataset 2")
frequency_graph(clean_df_program2, "Average frequency Dataset 2")
average_price_graph(clean_df_program2, "Average price Dataset 2")
recency_graph(clean_df_program2, "Average recency Dataset 2")
Revenue_graph(clean_df_program2[clean_df_program2$heavy_buyer ==1,], "Total Revenue Dataset 2")
Revenue_graph(clean_df_program2[clean_df_program2$medium_buyer ==1,], "Total Revenue Dataset 2")
Revenue_graph(clean_df_program2[clean_df_program2$light_buyer ==1,], "Total Revenue Dataset 2")
Avg_quantity_sold_graph(clean_df_program2[clean_df_program2$heavy_buyer ==1,], "Average quantity sold Dataset 2")
Avg_quantity_sold_graph(clean_df_program2[clean_df_program2$medium_buyer ==1,], "Average quantity sold Dataset 2")
Avg_quantity_sold_graph(clean_df_program2[clean_df_program2$light_buyer ==1,], "Average quantity sold Dataset 2")
frequency_graph(clean_df_program2[clean_df_program2$heavy_buyer ==1,], "Average frequency Dataset 2")
frequency_graph(clean_df_program2[clean_df_program2$medium_buyer ==1,], "Average frequency Dataset 2")
frequency_graph(clean_df_program2[clean_df_program2$light_buyer ==1,], "Average frequency Dataset 2")
average_price_graph(clean_df_program2[clean_df_program2$heavy_buyer ==1,], "Average price Dataset 2")
average_price_graph(clean_df_program2[clean_df_program2$medium_buyer ==1,], "Average price Dataset 2")
average_price_graph(clean_df_program2[clean_df_program2$light_buyer ==1,], "Average price Dataset 2")
recency_graph(clean_df_program2[clean_df_program2$light_buyer == 1,], "Average recency Dataset 2 Light Users")
recency_graph(clean_df_program2[clean_df_program2$medium_buyer == 1,], "Average recency Dataset 2 Medium Users")
recency_graph(clean_df_program2[clean_df_program2$heavy_buyer == 1,], "Average recency Dataset 2 Heavy Users")
# Graphs dataset 3
Revenue_graph(clean_df_program3, "Total Revenue Dataset 3")
Avg_quantity_sold_graph(clean_df_program3, "Average quantity sold Dataset 3")
frequency_graph(clean_df_program3, "Average frequency Dataset 3")
average_price_graph(clean_df_program3, "Average price Dataset 3")
recency_graph(clean_df_program3, "Average recency Dataset 3")
Revenue_graph(clean_df_program3[clean_df_program3$heavy_buyer ==1,], "Total Revenue Dataset 3")
Revenue_graph(clean_df_program3[clean_df_program3$medium_buyer ==1,], "Total Revenue Dataset 3")
Revenue_graph(clean_df_program3[clean_df_program3$light_buyer ==1,], "Total Revenue Dataset 3")
Avg_quantity_sold_graph(clean_df_program3[clean_df_program3$heavy_buyer ==1,], "Average quantity sold Dataset 3")
Avg_quantity_sold_graph(clean_df_program3[clean_df_program3$medium_buyer ==1,], "Average quantity sold Dataset 3")
Avg_quantity_sold_graph(clean_df_program3[clean_df_program3$light_buyer ==1,], "Average quantity sold Dataset 3")
frequency_graph(clean_df_program3[clean_df_program3$heavy_buyer ==1,], "Average frequency Dataset 3")
frequency_graph(clean_df_program3[clean_df_program3$medium_buyer ==1,], "Average frequency Dataset 3")
frequency_graph(clean_df_program3[clean_df_program3$light_buyer ==1,], "Average frequency Dataset 3")
average_price_graph(clean_df_program3[clean_df_program3$heavy_buyer ==1,], "Average price Dataset 3")
average_price_graph(clean_df_program3[clean_df_program3$medium_buyer ==1,], "Average price Dataset 3")
average_price_graph(clean_df_program3[clean_df_program3$light_buyer ==1,], "Average price Dataset 3")
recency_graph(clean_df_program3[clean_df_program3$light_buyer == 1,], "Average recency Dataset 3 Light Users")
recency_graph(clean_df_program3[clean_df_program3$medium_buyer == 1,], "Average recency Dataset 3 Medium Users")
recency_graph(clean_df_program3[clean_df_program3$heavy_buyer == 1,], "Average recency Dataset 3 Heavy Users")


