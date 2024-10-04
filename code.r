library(dplyr)
library(broom)
library(forecast)
library(readr)
library(tidyr)
library(lubridate)
library(glmnet)
library(gbm)
library(xts)
library(ggplot2)
library(tidyverse)

#Importing Data

setwd("D:/NMIMS/Projects/Walmart Sales Forecasting/wallmart sales forecasting")
store_raw <- read_csv("stores.csv")
feature_raw <- read_csv("features.csv")
train <- read_csv("train.csv")
test_final <- read_csv("test.csv")
# glimpse(store_raw)
# glimpse(feature_raw)
# glimpse(train)
# glimpse(test_final)
store_raw$Store <- as.factor(store_raw$Store)
train$Store <- as.factor(train$Store)
train$Dept <- as.factor(train$Dept)
test_final$Store <- as.factor(test_final$Store)
test_final$Dept <- as.factor(test_final$Dept)
feature_raw$Store <- as.factor(feature_raw$Store)

#Exploring Data

train %>% summarize(Total_stores = n_distinct(Store))

train %>% summarize(Total_Dept = n_distinct(Dept))

train %>% summarize(min_date = min(Date), max_date = max(Date), total_weeks = difftime(min_date,max_date, unit = "weeks"))

train %>% group_by(Store, Dept) %>% summarize(count_wk = n_distinct(Date)) %>% ggplot(aes(x = count_wk)) + geom_histogram()

ggplot(store_raw, aes(x = Size)) + geom_histogram(binwidth = 15000) + facet_grid(Type~.)

store_raw %>% group_by(Type) %>% summarize(n()) %>% rename(`Number of Stores` = `n()`)

train %>% left_join(store_raw, by = "Store") %>% ggplot(aes(x = Weekly_Sales)) + geom_histogram() + facet_grid(Type~.) + scale_x_log10()

train_all_factors <- train %>% left_join(store_raw, by = "Store") %>% left_join(feature_raw, by = c("Store", "Date")) %>% mutate(first_wk = ifelse(mday(Date) <= 7, TRUE, FALSE))

train_all_factors %>% group_by(Type, Date) %>% summarize(sales = sum(Weekly_Sales)) %>% ggplot(aes(x = Date, y = sales, col = Type)) + geom_line() + scale_y_log10() + scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "4 weeks") + theme(axis.text.x = element_text(angle = 90, hjust = 1))


train_all_factors %>% group_by(Date,first_wk) %>% summarize(sales = sum(Weekly_Sales)) %>% ggplot(aes(x = first_wk, y = sales)) + geom_boxplot()

train_all_factors %>% ggplot(aes(x = CPI, y = Weekly_Sales)) + geom_point()  + geom_line(aes(y= fitted(lm(Weekly_Sales~CPI, data = train_all_factors))), colour = "red")

train_all_factors %>% ggplot(aes(x = Unemployment, y = Weekly_Sales)) + geom_point()  + geom_line(aes(y= fitted(lm(Weekly_Sales~Unemployment, data = train_all_factors))), colour = "red")


train_all_factors %>% ggplot(aes(x = Temperature, y = Weekly_Sales)) + geom_point()  + geom_line(aes(y= fitted(lm(Weekly_Sales~Temperature, data = train_all_factors))), colour = "red")


#DATA CLEANING

feature_raw %>% filter(Store == c(1,25,33,41)) %>% ggplot(aes(x = Date, y = CPI, col = Store)) + geom_line()

#Creating Exhaustive table with all weeks and store combination and joining feature to get CPI and Unemployement exhaustive data set to implement time series forecast for missing 2013 data
CPI_Unemp_exh <- feature_raw %>% select(Store, Date) %>% complete(Store, Date) %>% left_join(feature_raw, by = c("Store","Date")) %>% select(Store, Date, CPI, Unemployment)
# Store list to put in loop
store_lst <- distinct(CPI_Unemp_exh,Store)
# Dates to be forecasted
date_fore <- CPI_Unemp_exh %>% filter(is.na(CPI))  %>% select(Date) %>% distinct() %>% arrange()
# Function to time series forecast CPI by store
forecast <- NULL
for (i in unique(CPI_Unemp_exh$Store)) {
  CPI_ts <- CPI_Unemp_exh %>% filter(Date<= "2013-04-26", Store == i) %>% select(CPI) %>% ts(start = c(2010,2,5), end = c(2013,4,26), frequency = 52)
  fit <- auto.arima(CPI_ts)
  frcst <- as.data.frame(forecast(fit,13)) %>% select(`Point Forecast`) %>% rename(CPI = `Point Forecast`)
  fore <- merge(i,date_fore, all = T) %>% cbind(frcst)
  forecast <- rbind(forecast, fore)
}

plot(forecast(fit,13))

feature_raw %>% filter(Store == c(1,39,33,44)) %>% ggplot(aes(x = Date, y = Unemployment, col = Store)) + geom_line()


# Function to forecast Unemployment
forecast_Un <- NULL

for (i in unique(CPI_Unemp_exh$Store)) {
  Unemp_ts <- CPI_Unemp_exh %>% filter(Date<= "2013-04-26", Store == i) %>% select(Unemployment) %>% ts(start = c(2010,2,5), end = c(2013,4,26), frequency = 52)
  fit <- auto.arima(Unemp_ts)
  frcst <- as.data.frame(forecast(fit,13)) %>% select(`Point Forecast`) %>% rename(Unemployment = `Point Forecast`)
  fore <- merge(i,date_fore, all = T) %>% cbind(frcst)
  forecast_Un <- rbind(forecast_Un, fore)
}

plot(forecast(fit,13))


# Combining both the forecast
forecast_CPI_Un <- inner_join(forecast, forecast_Un, by = c("x", "Date")) %>% rename(Store = x)

# Joining it with feature_raw and substituting the missing values with forecast
feature_raw1 <- feature_raw %>% left_join(forecast_CPI_Un, by = c("Store","Date")) %>% mutate(CPI = ifelse(is.na(CPI.x),CPI.y,CPI.x),Unemployment = ifelse(is.na(Unemployment.x),Unemployment.y,Unemployment.x)) %>%
  select(-Unemployment.x, -Unemployment.y, -CPI.x, -CPI.y)




#Weekly Sales Outlier Treatment

feature_fi <- feature_raw1 %>% mutate_at(c("MarkDown1","MarkDown2","MarkDown3","MarkDown4","MarkDown5"),funs(ifelse(.<0 |                          is.na(.),0,.))) %>% mutate(mkdn_flag = (MarkDown1 !=0 | MarkDown2 !=0 | MarkDown3 !=0 | MarkDown4 !=0 | MarkDown5 !=0                ), mkdn_hol = IsHoliday | mkdn_flag)
train_all_fac <- train %>% left_join(feature_fi, by = c("Store","Date"))
# Treating outliers outside 1.5*IQR range at store-dept level whenever its not holiday or no markdown given
train_outlier_treated <- train_all_fac %>% group_by(Store, Dept) %>% mutate(perc_25 = quantile(Weekly_Sales,0.25), perc_75 = quantile(Weekly_Sales,0.75), iqr_sales = IQR(Weekly_Sales), Wkly_sales_treated = ifelse(Weekly_Sales<perc_25 - 1.5* iqr_sales & !mkdn_hol, perc_25 - 1.5* iqr_sales, ifelse(Weekly_Sales > perc_75 + 1.5* iqr_sales & !mkdn_hol, perc_75 + 1.5* iqr_sales,Weekly_Sales)))

# Percentage of outliers 
paste(round(mean(train_outlier_treated$Weekly_Sales != train_outlier_treated$Wkly_sales_treated) * 100,2),"%")
