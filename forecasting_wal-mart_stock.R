library("forecast")

walmart.data <- read.csv("../PTSF-Datasets/WalMartStock.csv")
walmart.data$Date <- as.Date(walmart.data$Date, format="%d-%b-%y")
close.ts <- ts(walmart.data$Close)

# Figure 6.21
par(mfrow = c(1,1))
plot(walmart.data$Date, walmart.data$Close, type = "l", ylab = "Close Price ($)", xlab = "Time", xaxt = "n")
axis.Date(side = 1, walmart.data$Date, format = "%b-%y")

# Figure 6.22
par(mfrow = c(1,2))
Acf(close.ts, lag.max = 10, main = "ACF Plot for Close")
Acf(diff(close.ts,1), lag.max = 10, main = "ACF Plot for Differenced Series")

# Plot the time series of the first differences
diff.ts <- ts(diff(close.ts,1))
plot(diff.ts, type = "l", ylab = "Diff", xlab = "Time")


# Table 6.7
Arima(close.ts, order = c(1,0,0))
Arima(diff(close.ts,1), order = c(1,0,0))
