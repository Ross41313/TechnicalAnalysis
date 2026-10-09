# BDA400 Assignment 2 - Technical Analysis using R, Preliminary Stage



library(quantmod)
library(TTR)

# Read the stock symbols and download one year of daily market data.
load_stock_data <- function(file_name = "portfolio.txt",
                            from_date = Sys.Date() - 365,
                            to_date = Sys.Date()) {
  symbols <- readLines(file_name, warn = FALSE)
  symbols <- trimws(symbols)
  symbols <- symbols[symbols != ""]

  stock_data <- list()

  for (symbol in symbols) {
    stock_xts <- getSymbols(
      symbol,
      src = "yahoo",
      from = from_date,
      to = to_date,
      auto.assign = FALSE
    )

    stock_data[[symbol]] <- data.frame(
      Date = index(stock_xts),
      Open = as.numeric(Op(stock_xts)),
      High = as.numeric(Hi(stock_xts)),
      Low = as.numeric(Lo(stock_xts)),
      Close = as.numeric(Cl(stock_xts)),
      Volume = as.numeric(Vo(stock_xts)),
      Adjusted = as.numeric(Ad(stock_xts))
    )
  }

  return(stock_data)
}

# Base R's mode() reports an object's storage mode, not the statistical mode.
# This helper returns the first value with the highest frequency.
statistical_mode <- function(x) {
  x <- na.omit(x)
  values <- unique(x)
  values[which.max(tabulate(match(x, values)))]
}

# Add a 20-day simple moving average and compute the required summary statistics.
calculate_statistics <- function(stock_data, n = 20) {
  statistics <- data.frame(
    Symbol = character(),
    Moving_Average_20 = numeric(),
    Mean = numeric(),
    Mode = numeric(),
    Median = numeric(),
    Standard_Deviation = numeric(),
    stringsAsFactors = FALSE
  )

  for (symbol in names(stock_data)) {
    prices <- stock_data[[symbol]]$Adjusted
    stock_data[[symbol]]$MovingAverage20 <- SMA(prices, n = n)

    statistics <- rbind(
      statistics,
      data.frame(
        Symbol = symbol,
        Moving_Average_20 = tail(na.omit(stock_data[[symbol]]$MovingAverage20), 1),
        Mean = mean(prices, na.rm = TRUE),
        Mode = statistical_mode(prices),
        Median = median(prices, na.rm = TRUE),
        Standard_Deviation = sd(prices, na.rm = TRUE),
        stringsAsFactors = FALSE
      )
    )
  }

  return(list(stock_data = stock_data, statistics = statistics))
}

# Display the first six rows of each stock data frame.
display_stock_data <- function(stock_data) {
  for (symbol in names(stock_data)) {
    cat("\n---", symbol, "---\n")
    print(head(stock_data[[symbol]]))
  }
}

# Display one simple visualization for each stock in a single plotting window.
plot_stock_data <- function(stock_data) {
  par(mfrow = c(length(stock_data), 1), mar = c(4, 4, 2, 1))

  for (symbol in names(stock_data)) {
    df <- stock_data[[symbol]]

    plot(
      df$Date,
      df$Adjusted,
      type = "l",
      xlab = "Date",
      ylab = "Adjusted Close",
      main = paste(symbol, "- Adjusted Close and 20-Day Moving Average")
    )

    lines(df$Date, df$MovingAverage20, lty = 2)
    legend(
      "topleft",
      legend = c("Adjusted Close", "20-Day Moving Average"),
      lty = c(1, 2),
      bty = "n"
    )
  }

  par(mfrow = c(1, 1))
}

# ----------------------------
# RUN THE ASSIGNMENT ANALYSIS
# ----------------------------

stocks <- load_stock_data("portfolio.txt")
analysis <- calculate_statistics(stocks)
stocks <- analysis$stock_data
statistics <- analysis$statistics

display_stock_data(stocks)
print(statistics)
plot_stock_data(stocks)
