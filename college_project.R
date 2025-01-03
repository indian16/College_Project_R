library(shiny)
library(ggplot2)
library(forecast)
library(dplyr)
library(lubridate)
library(shinydashboard)
library(lubridate)
library(DT)
library(httr)

df <-read.csv("D://delhi_aqi.csv")  


calculate_aqi <- function(co, no, no2, o3, so2, pm2_5, pm10, nh3) {
  aqi <- (0.25 * co + 0.1 * no + 0.2 * no2 + 0.1 * o3 + 
            0.15 * so2 + 0.1 * pm2_5 + 0.1 * pm10 + 0.1 * nh3) / 8
  return(aqi)
}


df$date <- as.POSIXct(df$date, format = "%Y-%m-%d %H:%M", tz = "UTC")
df <- df %>% arrange(date) #Arrangement according to date

 
df <- df %>% 
  mutate(AQI = calculate_aqi(co, no, no2, o3, so2, pm2_5, pm10, nh3))



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

google_client_id <- "884012862657-d0pq1amc1dop2paj6cdj05bk4aodvd59.apps.googleusercontent.com"

google_client_secret <- "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"


scopes <- c("https://www.googleapis.com/auth/userinfo.profile",
            "https://www.googleapis.com/auth/userinfo.email")




ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = "Delhi Air Quality Prediction",
    tags$li(class = "dropdown",
            actionButton("logout_btn", "Logout", class = "btn-danger"))
  ),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Introduction", tabName = "intro", icon = icon("info-circle")),
      menuItem("Predict AQI", tabName = "predict", icon = icon("chart-line")),
      menuItem("View Data", tabName = "data", icon = icon("table")),
      menuItem("Analysis", tabName = "analysis", icon = icon("chart-area")),
      menuItem("Login", tabName="login",icon = icon("sign-in-alt"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .main-header .logo {
          font-family: 'Montserrat', sans-serif;
          font-weight: bold;
        }
        .box {
          border-top: 2px solid #1E90FF;
          border-radius: 10px;
          box-shadow: 0px 0px 15px rgba(0,0,0,0.1);
        }
        .content-wrapper {
          background-color: #f4f6f9;
        }
        .selectize-input {
          font-size: 16px;
        }
        .splash {
          text-align: center;
          padding-top: 50px;
        }
      "))
    ),
    
    tabItems(
    
      tabItem(tabName = "intro",
              fluidPage(
                tags$div(class = "splash",
                         style = "background-image: url('https://images.moneycontrol.com/static-mcnews/2024/11/20241102025036_Delhi-AQI.jpg?impolicy=website&width=770&height=431'); 
                              background-size: cover; 
                              background-position: center; 
                              background-repeat: no-repeat; 
                              height: 100vh; 
                              display: flex; 
                              flex-direction: column; 
                              align-items: center; 
                              justify-content: center;",
                         h1("Welcome to the Delhi Air Quality Predictor", style = "font-weight: bold;"),
                         tags$p(tags$strong("The Delhi Air Quality Predictor is an interactive application that empowers users to predict air quality index (AQI) and pollutant concentrations.")),
                         tags$p(tags$strong("It leverages advanced predictive modeling to provide accurate forecasts based on user-selected dates and times.")),
                         tags$p(tags$strong("Dynamic visualizations make it easy to explore historical and forecasted trends, enhancing your understanding of air quality patterns.")),
                         tags$p(tags$strong("This tool is designed for decision-making and awareness in addressing air pollution challenges.")),
                         actionButton("start", "Get Started", class = "btn-primary", colors = "white")
                )
              )
      ),
      
    
      tabItem(tabName = "predict",
              fluidPage(
                box(title = "Select Date and Time for Prediction", width = 4,
                    dateInput("date_input", "Select Date:", format = "yyyy-mm-dd"),
                    selectInput("hour_input", "Select Hour:", choices = sprintf("%02d", 0:23)),
                    selectInput("minute_input", "Select Minute:", choices = sprintf("%02d", seq(0, 55, by = 5))),
                    actionButton("predict_btn", "Predict AQI", class = "btn-success")),
                
                box(title = "Prediction Results", width = 8,
                    verbatimTextOutput("results"),
                    plotOutput("forecast_plot", height = 300))
              )
      ),
      
     
      tabItem(tabName = "data",
              fluidPage(
                box(title = "Air Quality Data", width = 12,
                    dataTableOutput("data_table"))
              )
      ),
      tabItem(tabName = "login",
              fluidRow(
                box(width = 12, title = "Login with Google", status = "primary", solidHeader = TRUE, 
                    actionButton("login_btn", "Login with Google", icon = icon("google"))
                ),
                box(width = 12, title = "User Info", status = "info", solidHeader = TRUE,
                    textOutput("user_info")
                )
              )
      ),
      
      
      tabItem(tabName = "analysis",
              fluidPage(
                box(title = "Select Gas or AQI to View Trends", width = 4,
                    selectInput("gas_selection", "Select Gas/AQI:", choices = c("co", "no", "no2", "o3", "so2", "pm2_5", "pm10", "nh3", "AQI")),
                    actionButton("view_graph", "View Graph", class = "btn-info")),
                
                box(title = "Gas/AQI Trend Analysis", width = 8,
                    plotOutput("trend_plot", height = 400))
              )
      )
    )
  )
)


server <- function(input, output, session) {
  logged_in <- reactiveVal(FALSE)  # Track login status
  
  observeEvent(input$tabs, {
    selected_tab <- input$tabs
    if(selected_tab == "predict"){
      observe({
        if (!logged_in()) {
          updateTabItems(session, "tabs", "login")
        }
      })
    }
    else if(selected_tab == "analysis"){
      observe({
        if (!logged_in()) {
          updateTabItems(session, "tabs", "login")
        }
      })
    }
    else if(selected_tab == "data"){
      observe({
        if (!logged_in()) {
          updateTabItems(session, "tabs", "login")
        }
      })
    }
  })
  observeEvent(input$login_btn, {
    req(input$login_btn)
    oauth_endpoints("google")
    myapp <- oauth_app("google", key = google_client_id, secret = google_client_secret)
    goog_auth <- oauth2.0_token(oauth_endpoints("google"), myapp, scope = scopes, cache = FALSE)
    

    user_info <- GET("https://www.googleapis.com/oauth2/v2/userinfo", config(token = goog_auth))
    user_info_data <- content(user_info)
    
    output$user_info <- renderText({
      paste("Hello,", user_info_data$name)
    })
    logged_in(TRUE)
  })
  
  observeEvent(input$logout_btn, {
    showModal(
      modalDialog(
        title = "Confirm Logout",
        "Are you sure you want to log out?",
        easyClose = FALSE,
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_logout", "Logout", class = "btn-danger")
        )
      )
    )
  })
  
  observeEvent(input$confirm_logout, {
    removeModal() 
   
    session$reload()
  })
  
  observeEvent(input$start, {
    observe({
      if (!logged_in()) {
        updateTabItems(session, "tabs", "login")
      }
    })
    updateTabItems(session, "tabs", "predict")
  })
  
  observeEvent(input$predict_btn, {
    req(input$date_input, input$hour_input, input$minute_input)
    date_time <- paste(input$date_input, sprintf("%02d:%02d", as.numeric(input$hour_input), as.numeric(input$minute_input)))
    
    forecast_data <- forecast_or_return_data(date_time)
    
    if (!is.null(forecast_data)) {
      aqi <- forecast_data$AQI
      
      output$results <- renderPrint({
        list(Predicted_Values = forecast_data, AQI = aqi)
      })
    } else {
      output$results <- renderPrint("Date not found in dataset.")
    }
  })
  
 
  output$data_table <- renderDataTable({
    datatable(df %>% select(date, co, no, no2, o3, so2, pm2_5, pm10, nh3, AQI))
  })
  

  observeEvent(input$view_graph, {
    req(input$gas_selection)
    selected_gas <- input$gas_selection
    
    historical_data <- df %>% select(date, all_of(selected_gas)) %>%
      rename(value = all_of(selected_gas)) %>%
      mutate(type = "Existing Values")
    
   
    date_time <- paste(input$date_input, sprintf("%02d:%02d", as.numeric(input$hour_input), as.numeric(input$minute_input)))
    forecast_data <- forecast_or_return_data(date_time)
    if (!is.null(forecast_data)) {
      predicted_data <- data.frame(
        date = as.POSIXct(date_time, format = "%Y-%m-%d %H:%M", tz = "UTC"),
        value = forecast_data[[selected_gas]],
        type = "Predicted Value"
      )
      
     
      combined_data <- bind_rows(historical_data, predicted_data)
      
      
      output$trend_plot <- renderPlot({
        ggplot(combined_data, aes(x = date, y = value, color = type)) +
          geom_line(data = filter(combined_data, type == "Existing Values"), size = 1) +
          geom_point(data = filter(combined_data, type == "Predicted Value"), size = 3) +
          scale_color_manual(
            values = c("Existing Values" = "#1E90FF", "Predicted Value" = "red"),
            name = "Legend"
          ) +
          labs(
            title = paste("Historical and Forecasted Values for", selected_gas),
            x = "Date",
            y = selected_gas
          ) +
          theme_minimal()
      })
    } else {
      output$trend_plot <- renderPlot({
        ggplot() + 
          labs(title = "No prediction available for the selected date and time.") +
          theme_void()
      })
    }
  })
}

shinyApp(ui = ui, server = server)