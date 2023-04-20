# Functions to study unit increase in unique product , customer , branch 
#'@date 16-06-2020
#'Version 1


#######################################################################################################
# Placeholder names:
# Input_Date_Variable , Input_Forecast_Variable 
#######################################################################################################
#detach("package:Metrics", unload=TRUE)
 

###########################################################################################
# Loading Library
###########################################################################################

library(readr)
library(forecast)
library(fpp2)
library(TTR)
library(dplyr)
library(lubridate)
library(prophet)
library(jsonlite)

 
############################################################################################
# Input Data   { Input_Forecast_Variable , Input_Date_Variable , No_Of_Period}
############################################################################################

# Dummy Inputs Just for Testing 

Input_Forecast_Variable <- c(23323 , 454545 , 45453453,4545445,4545453,4545455)


Input_Date_Variable <- c("2020-01-01","2020-01-02","2020-01-03","2020-01-04","2020-01-05","2020-01-06")



# Read Csv File Here 
#sales_data_csv_file <- read.csv('Sales_2017_Jan.csv')

# Be Note define the columns of data which needs to be put as date and sales as other column

#Input_Date_Variable <- sales_data_csv_file$sold_on_dt

#Input_Forecast_Variable <- sales_data_csv_file$sales_price

No_of_Periods_To_Forecast <- 5



###############################################################################################
# Input Data Validation
################################################################################################


#----1.Checking for length of Transaction Data Provided-----------------------------------


check_length <- function(Input_Date_Variable ,Input_Forecast_Variable){
  
  Input_Forecast_Variable_length <- length(Input_Forecast_Variable)
  Input_Date_Variable_length <- length(Input_Date_Variable)
  
  if (Input_Forecast_Variable_length == Input_Date_Variable_length ){
    print('Length Forecast Variable and date are same ')
  }
  else
    stop("Input variables length must be of same ")
}

check_length(Input_Date_Variable , Input_Forecast_Variable)




##########################################################################################
# Data Transformation 
##########################################################################################

#----------- Converting Date format------------------------------------------------ 

Input_Date_Variable <- as.Date(Input_Date_Variable ,format = "%Y-%m-%d")



#------------- Check Frequency of Data Provided-------------------------------------

check_frequency <- function(Input_Date_Variable){
  # Subjected to change as per handling missing Data
  Data_Freq <- as.numeric(Input_Date_Variable[2] - Input_Date_Variable[1])
  
  if (Data_Freq == 1){
    Data_Frequency <- "days"
    Data_Frequency_num <- 365.25
  }
  if(Data_Freq == 7){
    Data_Frequency <- "weeks"
    Data_Frequency_num <- 365.25/7
  }  
  if(Data_Freq == 28 | Data_Freq == 29 | Data_Freq == 30 | Data_Freq == 31 ){
    Data_Frequency <- "months"
    Data_Frequency_num <- 12
  }  
  if(Data_Freq == 365 |Data_Freq == 366 ){
    Data_Frequency <- "years"
    Data_Frequency_num <- 1
  }  
  return(list(Data_Frequency,Data_Frequency_num))
}


# Saving the frequency results in Variable 
check_frequency_result <-  check_frequency(Input_Date_Variable)

Data_Frequency_level <- check_frequency_result[[1]]

Data_Frequency_level_numeric <- check_frequency_result[[2]]

sprintf("The Frequency of Data is --> %s , %s",Data_Frequency_level , Data_Frequency_level_numeric)


#------------- Creating Future Dates---------------------------------------------------

# Creating a sequence of date 
Future_Dates <- seq(Input_Date_Variable[length(Input_Date_Variable)],
                    by = Data_Frequency_level,length.out = No_of_Periods_To_Forecast+1)

# by default last date is taken so removing that and getting future data
Future_Dates <- Future_Dates[2:length(Future_Dates)]



#------- Converting Input Date Variable and Input Variable in to Time Series Object-----

TS_Data <- ts(Input_Forecast_Variable, 
            freq= Data_Frequency_level_numeric , 
            start=decimal_date(ymd(Input_Date_Variable[1])))

############################################################################################
#Modeling 
###########################################################################################

#-------- Defining  data frame for forecast result ----------------------------

# Adding the future dates
forecast_results <- as.data.frame(Future_Dates)



#----- 1. Simple Exponential Smoothing Model------------------------------------

se_model <- ses(TS_Data, h = No_of_Periods_To_Forecast)
summary(se_model)

# Saving Results in  Forecast Results 
forecast_results$Simple_Exponential <- se_model$mean
forecast_results$Simple_Exponential_High_Value <-  se_model$upper[,2]
forecast_results$Simple_Exponential_Low_Value <-  se_model$lower[,2]

Simple_Exponential_RMSE <- accuracy(se_model)[2] 

# Saving Error Result 
RMSE_Error <- as.data.frame(Simple_Exponential_RMSE) 



#------ 2. Holt Trend Model ------------------------------------------------------- 

holt_model <- holt(TS_Data, h = No_of_Periods_To_Forecast)
summary(holt_model)

# Saving Results in  Forecast Results 
forecast_results$Holt_Trend <- holt_model$mean
forecast_results$Holt_Trend_High_Value <-  holt_model$upper[,2]
forecast_results$Holt_Trend_Low_Value <-  holt_model$lower[,2]

# Saving Error Result 

RMSE_Error$Holt_Trend_RMSE <- accuracy(holt_model)[2]


#------ 3.ARIMA---------------------------------------------------------------------

arima_model <- auto.arima(TS_Data)
summary(arima_model)
fore_arima <- forecast::forecast(arima_model, h=No_of_Periods_To_Forecast)

# Saving Results in  Forecast Results 
forecast_results$Auto_Arima <- fore_arima$mean
forecast_results$Auto_Arima_High_Value <-  fore_arima$upper[,2]
forecast_results$Auto_Arima_Low_Value <-  fore_arima$lower[,2]

# Saving Error Result 

RMSE_Error$Auto_Arima_RMSE <- accuracy(fore_arima)[2]

#---------- 4. TBATS ----------------------------------------------------------------

model_tbats <- tbats(TS_Data)
summary(model_tbats)

for_tbats <- forecast::forecast(model_tbats, h = No_of_Periods_To_Forecast)

# Saving Results in  Forecast Results 
forecast_results$TBAT <- for_tbats$mean
forecast_results$TBAT_High_Value <-  for_tbats$upper[,2]
forecast_results$TBAT_Low_Value <-  for_tbats$lower[,2]

# Saving Error Result 

RMSE_Error$TBAT_RMSE <- accuracy(for_tbats)[2]



#------------ 5.SVM --------------------------------------------------------------------

library(e1071)

# Length of time series data 
data_length <- 1:length(Input_Date_Variable)

# Building Data frame for SVM 

SVM_DF <- data.frame(data_length,Input_Forecast_Variable)
colnames(SVM_DF)<-c("x","y")

# train an svm model, consider further tuning parameters for lower MSE
svmodel <- svm(y ~ x,data=SVM_DF, type="eps-regression")

#specify timesteps for forecast, eg for all series + No_OF_Periods ahead

nd <- 1:(length(Input_Date_Variable) + No_of_Periods_To_Forecast)

#compute forecast for all the total time series data plus the no_of_period for forecast 
SVM_Model <- predict(svmodel, newdata=data.frame(x=nd))


# Saving Error Result 

RMSE_Error$SVM_RMSE <- accuracy(SVM_DF$y, SVM_Model[1:length(SVM_DF$y)])[2]

SVM_Error <- RMSE_Error$SVM_RMSE


# Saving Results in  Forecast Results 

forecast_results$SVM <- SVM_Model[(length(SVM_DF$y) + 1) :length(SVM_Model)]
forecast_results$SVM_High_Value <-  SVM_Model[(length(SVM_DF$y) + 1) :length(SVM_Model)]+SVM_Error
forecast_results$SVM_Low_Value <-  SVM_Model[(length(SVM_DF$y) + 1) :length(SVM_Model)]-SVM_Error

# --------6. Prophet----------------------------------------------------------------------


prophet_df <- data.frame(Input_Date_Variable ,Input_Forecast_Variable)
colnames(prophet_df) <- c("ds", "y")

prophet_m <- prophet(prophet_df)

prophet_future <- make_future_dataframe(prophet_m, periods = No_of_Periods_To_Forecast)

Prophet_forecast <- predict(prophet_m, prophet_future)

#plot(m, forecast)

# Prophet Error
RMSE_Error$Prophet_RMSE <- accuracy(prophet_df$y , Prophet_forecast$yhat[1:length(prophet_df$y)])[2]
Prophet_Error <- RMSE_Error$Prophet_RMSE

# Saving High and Low 

forecast_results$Prophet <- Prophet_forecast$yhat[(length(Input_Date_Variable)+1) :length(Prophet_forecast$yhat)]
forecast_results$Prophet_High_Value <-  Prophet_forecast$yhat[(length(Input_Date_Variable)+1) :length(Prophet_forecast$yhat)] + Prophet_Error
forecast_results$Prophet_Low_Value <- Prophet_forecast$yhat[(length(Input_Date_Variable)+1) :length(Prophet_forecast$yhat)] - Prophet_Error

# Saving Assement Results

best_model <- names(RMSE_Error)[which.min(apply(RMSE_Error,MARGIN=2,min))]
best_model <- gsub('.{5}$', '', best_model)  #Remove _RMSE String
best_model_RMSE <- apply(RMSE_Error, 1, FUN=min)
best_model_RMSE <-format(round(best_model_RMSE, 4), nsmall = 4) # Limiting the decimal point upto 4 



Assesement_Text <-  sprintf("Based on the data and the period selected for forecasting %s period, the better model is %s since it has a smaller RMSE (Root Mean Square Error) value of %s as compared to the RMSE of other models that were executed on the same dataset", No_of_Periods_To_Forecast , best_model, best_model_RMSE )


########################################################################################
# Saving Result
########################################################################################

# Converting into desire Result Json Structure

Json_Result <- list( Results = lapply(1:nrow(forecast_results), FUN = function(i){
  list("Date" = as.character(forecast_results[i,'Future_Dates']),ARIMA = 
         list("ARIMA Final Forecast"= forecast_results[i,'Auto_Arima'],
              "ARIMA Lower Confidence" = forecast_results[i,'Auto_Arima_Low_Value'],
              "ARIMA Higher Confidence" = forecast_results[i,'Auto_Arima_High_Value']),
       SVM = list("SVM Final Forecast"= forecast_results[i,'SVM'],
                  "SVM Lower Confidence" = forecast_results[i,'SVM_Low_Value'],
                  "SVM Higher Confidence" = forecast_results[i,'SVM_High_Value']),
       TBAT = list("TBAT Final Forecast"= forecast_results[i,'TBAT'],
                   "TBAT Lower Confidence" = forecast_results[i,'TBAT_Low_Value'],
                   "TBAT Higher Confidence" = forecast_results[i,'TBAT_High_Value']),
       Simple_Exponential = list("Simple_Exponential Final Forecast"= forecast_results[i,'Simple_Exponential'],
                                 "Simple_Exponential Lower Confidence" = forecast_results[i,'Simple_Exponential_Low_Value'],
                                 "Simple_Exponential Higher Confidence" = forecast_results[i,'Simple_Exponential_High_Value']),
       Holt_Trend = list("Holt_Trend Final Forecast"= forecast_results[i,'Holt_Trend'],
                         "Holt_Trend Lower Confidence" = forecast_results[i,'Holt_Trend_Low_Value'],
                         "Holt_Trend Higher Confidence" = forecast_results[i,'Holt_Trend_High_Value']),
       Prophet = list("Prophet Final Forecast"= forecast_results[i,'Prophet'],
                      "Prophet Lower Confidence" = forecast_results[i,'Prophet_Low_Value'],
                      "Prophet Higher Confidence" = forecast_results[i,'Prophet_High_Value'])
  )
}
),
RMSE = RMSE_Error,
Assesment_of_Result = list(Assesement_Text)
)




write_json(Json_Result,'TS_Results.json',pretty = TRUE , auto_unbox = TRUE)




 



