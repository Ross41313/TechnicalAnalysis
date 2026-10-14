# BDA400 Assignment 6 - Technical Analysis using R, Visualization Phase

# Step 1: Data Collection and Setup

# Required packages
# install.packages("shiny")
# install.packages("ggplot2")
# install.packages("quantmod")

library(shiny)
library(ggplot2)
library(quantmod)

stock_symbol <- "AAPL"
start_date <- "2018-01-01"
end_date <- Sys.Date()

stock_data <- getSymbols(
  stock_symbol,
  src = "yahoo",
  from = start_date,
  to = end_date,
  auto.assign = FALSE
)


# Step 2: Visualizing Stock Data

ui <- fluidPage(
  titlePanel("Portfolio Visualization Dashboard"),

  dateRangeInput(
    "date_range",
    "Select Date Range:",
    start = start_date,
    end = end_date
  ),

  selectInput(
    "time_frame",
    "Select Time Frame:",
    choices = c("Daily", "Weekly", "Monthly")
  ),

  checkboxGroupInput(
    "technical_indicators",
    "Technical Indicators:",
    choices = c("Moving Averages", "RSI", "MACD"),
    selected = "Moving Averages"
  ),

  plotOutput("stock_chart")
)


server <- function(input, output) {

  output$stock_chart <- renderPlot({

    # Filter data based on date range
    filtered_data <- subset(
      stock_data,
      index(stock_data) >= input$date_range[1] &
        index(stock_data) <= input$date_range[2]
    )

    # Apply the selected time frame
    if (input$time_frame == "Weekly") {
      filtered_data <- to.weekly(
        filtered_data,
        indexAt = "lastof",
        drop.time = TRUE
      )
    }

    if (input$time_frame == "Monthly") {
      filtered_data <- to.monthly(
        filtered_data,
        indexAt = "lastof",
        drop.time = TRUE
      )
    }

    validate(
      need(NROW(filtered_data) >= 50,
           "Select a wider date range so at least 50 observations are available.")
    )

    plot_data <- data.frame(
      Date = as.Date(index(filtered_data)),
      Close = as.numeric(Cl(filtered_data))
    )

    # Create the stock-price chart
    p <- ggplot(
      data = plot_data,
      aes(x = Date, y = Close)
    ) +
      geom_line() +
      labs(
        title = paste(stock_symbol, input$time_frame, "Stock Chart"),
        x = "Date",
        y = "Closing Price"
      ) +
      theme_minimal()


    # Step 3: Overlay Technical Indicators

    if ("Moving Averages" %in% input$technical_indicators) {

      short_ma <- SMA(Cl(filtered_data), n = 20)
      long_ma <- SMA(Cl(filtered_data), n = 50)

      plot_data$Short_MA <- as.numeric(short_ma)
      plot_data$Long_MA <- as.numeric(long_ma)

      p <- p +
        geom_line(
          data = plot_data,
          aes(x = Date, y = Short_MA),
          linetype = "dashed",
          na.rm = TRUE
        ) +
        geom_line(
          data = plot_data,
          aes(x = Date, y = Long_MA),
          linewidth = 1,
          na.rm = TRUE
        )
    }

    if ("RSI" %in% input$technical_indicators) {

      rsi_values <- RSI(Cl(filtered_data), n = 14)
      plot_data$RSI <- as.numeric(rsi_values)

      p <- p +
        geom_line(
          data = plot_data,
          aes(x = Date, y = RSI),
          na.rm = TRUE
        )
    }

    if ("MACD" %in% input$technical_indicators) {

      macd_values <- MACD(
        Cl(filtered_data),
        nFast = 12,
        nSlow = 26,
        nSig = 9,
        maType = "EMA"
      )

      plot_data$MACD <- as.numeric(macd_values[, 1])

      p <- p +
        geom_line(
          data = plot_data,
          aes(x = Date, y = MACD),
          na.rm = TRUE
        )
    }


    # Step 4: Implement Trading Rules and Annotations

    # Moving Average Crossover Trading Rule
    # If the short-term MA is above the long-term MA, generate a Buy signal.
    # If the short-term MA is below the long-term MA, generate a Sell signal.
    # Otherwise, generate a Hold signal.
    short_ma <- SMA(Cl(filtered_data), n = 20)
    long_ma <- SMA(Cl(filtered_data), n = 50)

    signals <- ifelse(
      short_ma > long_ma,
      "Buy",
      ifelse(short_ma < long_ma, "Sell", "Hold")
    )

    signals[is.na(signals)] <- "Hold"
    plot_data$Signal <- as.character(signals)

    # Add signal annotations to the chart
    p <- p +
      geom_text(
        data = plot_data,
        aes(x = Date, y = Close, label = Signal),
        vjust = 1.5,
        color = "blue",
        check_overlap = TRUE
      )

    print(p)
  })
}


shinyApp(ui, server)

