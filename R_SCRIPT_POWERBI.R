# Load necessary libraries
library(forecast)
library(lubridate)
library(dplyr)
library(ggplot2)

# Load the dataset and convert it to a data frame (Enter the path of the file)
df <- read.csv('Path to the file')

# Ensure date is in the correct format
df$date <- as.POSIXct(df$date, format = "%Y-%m-%d %H:%M", tz = "UTC")

# Order data by date
df <- df %>% arrange(date)

# Define and fit ARIMA models for each gas with specified orders
models <- list()
models$co <- Arima(df$co, order = c(2, 1, 3))
models$no <- Arima(df$no, order = c(2, 1, 3))
models$no2 <- Arima(df$no2, order = c(3, 1, 3))
models$o3 <- Arima(df$o3, order = c(2, 1, 5))
models$so2 <- Arima(df$so2, order = c(2, 1, 5))
models$pm2_5 <- Arima(df$pm2_5, order = c(2, 1, 2))
models$pm10 <- Arima(df$pm10, order = c(2, 1, 2))
models$nh3 <- Arima(df$nh3, order = c(2, 1, 6))

# Forecast or return data function
forecast_or_return_data <- function(input_date_time, steps = 24) {
  input_dt <- as.POSIXct(input_date_time, format = "%Y-%m-%d %H:%M", tz = "UTC")
  
  # Return actual values if the date is in the dataset
  if (input_dt %in% df$date) {
    gas_values <- df[df$date == input_dt, ]
    return(as.list(gas_values))
  } 
  # Forecast for future dates
  else if (input_dt > max(df$date)) {
    forecast_results <- sapply(models, function(model) forecast(model, h = steps)$mean[1])
    return(as.list(forecast_results))
  } else {
    return(NULL)
  }
}

# Calculate AQI function
calculate_aqi <- function(co, no, no2, o3, so2, pm2_5, pm10, nh3) {
  aqi <- (0.25 * co + 0.1 * no + 0.2 * no2 + 0.1 * o3 + 
            0.15 * so2 + 0.1 * pm2_5 + 0.1 * pm10 + 0.1 * nh3) / 8
  return(aqi)
}

# AQI calculation for each row
df <- df %>%
  rowwise() %>%
  mutate(airQualityIndex = calculate_aqi(co, no, no2, o3, so2, pm2_5, pm10, nh3)) %>%
  ungroup()

# User-selected date (replace '2025-11-25 01:00' with user input via Power BI slicer)
input_date_time <- '2025-11-25 01:00' # Example input; replace with Power BI input

# Get forecast or actual data for selected date
result <- forecast_or_return_data(input_date_time)
if (!is.null(result)) {
  forecast_data <- as.data.frame(t(unlist(result)))
  
  # Convert forecast_data$date to datetime format to match df$date
  forecast_data$date <- as.POSIXct(input_date_time, format = "%Y-%m-%d %H:%M", tz = "UTC")
  forecast_data$type <- "Forecasted"
  
  # Calculate AQI for forecast
  forecast_data$airQualityIndex <- calculate_aqi(forecast_data$co, forecast_data$no, 
                                                 forecast_data$no2, forecast_data$o3,
                                                 forecast_data$so2, forecast_data$pm2_5,
                                                 forecast_data$pm10, forecast_data$nh3)
  
  # Append forecast to existing dataset
  df <- bind_rows(df, forecast_data)
}

# Plot
ggplot(df, aes(x = date)) +
  geom_point(aes(y = co, color = type), size = 1) +
  labs(title = "CO Concentration Over Time", x = "Date", y = "CO (μg/m³)") +
  scale_color_manual(values = c("Existing" = "red", "Forecasted" = "green")) +
  theme_minimal()