# Import libraries
library(tseries)
library(forecast)
library(odbc)
library(corrplot)
library(dplyr)
library(ggplot2)

# Connecting to SQL database
con <- dbConnect(odbc::odbc(),
                 Driver = "SQL Server",
                 Server = "Server Name",
                 Database = "SalesForecasting",
                 Port = 1433)

# Converting to dataframe
SalesForecasting <- tbl(con, 'train_data')
Sales_Forecasting <- collect(SalesForecasting)
View(Sales_Forecasting) # Viewing the dataframe

#histplot of sales
hist(log(Sales_Forecasting$sales))

Sales_Forecasting <- Sales_Forecasting %>% 
  mutate_if(is.numeric, function(x) ifelse(is.na(x), median(x, na.rm = T), x))

#count of  stores_type
dist_x2 = ggplot(data = Sales_Forecasting) +
  geom_bar(mapping = aes(x = stores_type))

#relationship between sales and transactions
ggplot(data = Sales_Forecasting, mapping = aes(x = sales, y = transactions)) + 
  geom_point()

# Changing data from character datatype to date datatype
Sales_Forecasting$date <- as.Date(Sales_Forecasting$date, format = "%Y-%m-%d")
class(Sales_Forecasting$date) # Confirming class of date column

# Extracting required columns into a variable
Sales_Forecasting_Pred <- Sales_Forecasting%>%
  select(date,store_number,family,onpromotion,sales)

# Removing Null values and grouping sales by date
store_sales_ts <- Sales_Forecasting_Pred %>%
  group_by(Sales_Forecasting$date) %>% # group by the day column
  summarise(total_sales=sum(sales)) %>%  # calculate the SUM of all sales that occurred on each day
  na.omit()

# Converting dataframe to time series
store_sales_ts <- ts(store_sales_ts$total_sales, start = c(2013, 1), end = c(2017, 12), frequency = 365)

class(store_sales_ts) # Confirming datatype of store_sales_ts
summary(store_sales_ts)
plot(store_sales_ts) # Plotting store_sales_ts 

start(store_sales_ts)
end(store_sales_ts)

# Plottings
boxplot(store_sales_ts~cycle(store_sales_ts))
plot.ts(store_sales_ts)
cycle(store_sales_ts)

# Using naive forecasting method
naive_ss <- naive(store_sales_ts, h = 12)
naive_ss

plot(aggregate(store_sales_ts, FUN=mean)) # Plotting the mean of store_sales_ts
adf.test(store_sales_ts) # dickey-fuller's stationarity test

# Using the arima
arima_model = auto.arima(store_sales_ts)
arima_model

Dcompstore_sales <- decompose(store_sales_ts, "multiplicative")
autoplot(Dcompstore_sales)

# forecasting using the arima model
forecast <- forecast::forecast(arima_model, h = 30)
forecast

plot(forecast)
plot(snaive(store_sales_ts, h = 30))

residue = residuals(forecast)
plot(residue)
