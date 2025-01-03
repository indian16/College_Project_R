library(shiny)
library(ggplot2)
library(forecast)
library(dplyr)
library(lubridate)
library(shinydashboard)
library(lubridate)
library(DT)
# Load your dataset
df <- delhi_aqi  

# Function to calculate AQI for dataset
calculate_aqi <- function(co, no, no2, o3, so2, pm2_5, pm10, nh3) {
  aqi <- (0.25 * co + 0.1 * no + 0.2 * no2 + 0.1 * o3 + 
            0.15 * so2 + 0.1 * pm2_5 + 0.1 * pm10 + 0.1 * nh3) / 8
  return(aqi)
}

# Conversion of date to specific format
df$date <- as.POSIXct(df$date, format = "%Y-%m-%d %H:%M", tz = "UTC")
df <- df %>% arrange(date) #Arrangement according to date

#Inclusion of the column AQI in the existing dataset 
df <- df %>% 
  mutate(AQI = calculate_aqi(co, no, no2, o3, so2, pm2_5, pm10, nh3))


# Fit ARIMA models (Time Series Analysis)
models <- list(
  co = Arima(df$co, order = c(2, 1, 3)),
  no = Arima(df$no, order = c(2, 1, 3)),
  no2 = Arima(df$no2, order = c(3, 1, 3)),
  o3 = Arima(df$o3, order = c(2, 1, 5)),
  so2 = Arima(df$so2, order = c(2, 1, 5)),
  pm2_5 = Arima(df$pm2_5, order = c(2, 1, 2)),
  pm10 = Arima(df$pm10, order = c(2, 1, 2)),
  nh3 = Arima(df$nh3, order = c(2, 1, 6))
)

# Prediction function
forecast_or_return_data <- function(input_date_time, steps = 24) {
  input_dt <- as.POSIXct(input_date_time, format = "%Y-%m-%d %H:%M", tz = "UTC")
  if (input_dt %in% df$date) {
    return(df %>% filter(date == input_dt))
  } else if (input_dt > max(df$date)) {
    forecast_results <- sapply(models, function(model) forecast(model, h = steps)$mean[1])
    forecast_results$AQI <- calculate_aqi(forecast_results["co"], forecast_results["no"], 
                                          forecast_results["no2"], forecast_results["o3"], 
                                          forecast_results["so2"], forecast_results["pm2_5"], 
                                          forecast_results["pm10"], forecast_results["nh3"])
    return(as.list(forecast_results))
  } else {
    return(NULL)
  }
}