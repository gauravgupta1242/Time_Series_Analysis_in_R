
## simple exponential smoothing in R##

## Holt Winter Model  ##

### Input Data is Month Wise Data from Dec 2016 to Jan 2019


library(feather) # data import
library(data.table) # data handle
library(rpart) # decision tree method
library(party) # decision tree method
library(forecast) # forecasting methods
library(randomForest) # ensemble learning method
library(ggplot2) # visualizations
library("TTR")



# reading Data 
DT <- as.data.table(read.csv('Monthly_Store_Data.csv'))
data_train <- DT[Date %in% n_date[1:26]]


## Parameter set for plotting Graph 

theme_ts <- theme(panel.border = element_rect(fill = NA, 
                                              colour = "grey10"),
                  panel.background = element_blank(),
                  panel.grid.minor = element_line(colour = "grey85"),
                  panel.grid.major = element_line(colour = "grey85"),
                  panel.grid.major.x = element_line(colour = "grey85"),
                  axis.text = element_text(size = 13, face = "bold"),
                  axis.title = element_text(size = 15, face = "bold"),
                  plot.title = element_text(size = 16, face = "bold"),
                  strip.text = element_text(size = 16, face = "bold"),
                  strip.background = element_rect(colour = "black"),
                  legend.text = element_text(size = 15),
                  legend.title = element_text(size = 16, face = "bold"),
                  legend.background = element_rect(fill = "white"),
                  legend.key = element_rect(fill = "white"))

# Plot  Original Sales vs Month Wise for overall Data

ggplot(data_train, aes(sold_on_dt, sales_price , , group = 1)) +
  geom_line() +
  labs(x = "Date", y = "Sales") +
  theme_ts


## Creating the time Series Object 

data_ts <- ts(data_train$sales, freq = 12  , start = c(1919,1) , end = c(1921,2))

## View Time Series Object
data_ts

## Plotting the time Series Object
plot(data_ts)


## Calling Model 
## default calling the model will give the forecasted values for same date of data provided

seriesforecasts <- HoltWinters(data_ts, beta=FALSE, gamma=FALSE)
seriesforecasts

## Predicted Data
seriesforecasts$fitted


## Plotting for the actual data vs forecasted data on same dates as provided in Original Data
plot(seriesforecasts)


## Predicting for Future Forecast Values for next 6 months (change parameter of numercial value to 
#                                                              other get other results ) 

predicted_Values <- predict(seriesforecasts, 6)

## Predicted Values

predicted_Values

## Ploting forecast of future values for next 6 month.   

plot(seriesforecasts , predicted_Values)



