# Define UI
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
      # Splash Screen
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
      
      # Prediction Tab
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
      
      # Data Tab
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
      
      # Analysis Tab with Select Input for Gas
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