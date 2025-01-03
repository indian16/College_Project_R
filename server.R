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
    # Initiate OAuth 2.0 authentication
    oauth_endpoints("google")
    myapp <- oauth_app("google", key = google_client_id, secret = google_client_secret)
    goog_auth <- oauth2.0_token(oauth_endpoints("google"), myapp, scope = scopes, cache = FALSE)
    
    # Get user information
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
    removeModal() # Close the confirmation modal
    # Invalidate the token and clear session variables
    session$reload()
  })
  
  # Link the Get Started button to the Predict AQI tab
  observeEvent(input$start, {
    observe({
      if (!logged_in()) {
        updateTabItems(session, "tabs", "login")
      }
    })
    updateTabItems(session, "tabs", "predict")
  })
  
  # Prediction and AQI Calculation
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
  
  # Data Table
  output$data_table <- renderDataTable({
    datatable(df %>% select(date, co, no, no2, o3, so2, pm2_5, pm10, nh3, AQI))
  })
  
  # Analysis Plot for Selected Gas/AQI 
  observeEvent(input$view_graph, {
    req(input$gas_selection)
    selected_gas <- input$gas_selection
    
    # Prepare historical data
    historical_data <- df %>% select(date, all_of(selected_gas)) %>%
      rename(value = all_of(selected_gas)) %>%
      mutate(type = "Existing Values")
    
    # Prepare predicted data
    date_time <- paste(input$date_input, sprintf("%02d:%02d", as.numeric(input$hour_input), as.numeric(input$minute_input)))
    forecast_data <- forecast_or_return_data(date_time)
    if (!is.null(forecast_data)) {
      predicted_data <- data.frame(
        date = as.POSIXct(date_time, format = "%Y-%m-%d %H:%M", tz = "UTC"),
        value = forecast_data[[selected_gas]],
        type = "Predicted Value"
      )
      
      # Combine datasets for plotting
      combined_data <- bind_rows(historical_data, predicted_data)
      
      # Create the plot
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