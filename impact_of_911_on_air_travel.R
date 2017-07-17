library(forecast)
library(xts) # time frame on the plot
###########################################
# Data exploration & data viz
###########################################
travel.data <- read.csv("../PTSF-Datasets/Sept11Travel.csv", 
                          header = F, sep=",",
                          skip = 1, 
                          col.names = c('date', 'air_rpm', 'rail_pm', 'vmt_bn'),
                          na.strings = 'NA',
                          stringsAsFactors = F)
str(travel.data)
head(travel.data)
travel.data$air_rpm <- as.numeric(gsub(",", "", travel.data$air_rpm)) / 1000000 
travel.data$rail_pm <- as.numeric(gsub(",", "", travel.data$rail_pm)) / 1000000

n.valid <- 32
n.train <- length(travel.data$air_rpm) - n.valid

air.ts <- ts(travel.data$air_rpm, start = c(1990, 1), end = c(2004, 4), 
             freq = 12)
auto.ts <- ts(travel.data$vmt_bn, start = c(1990, 1), end = c(2004, 4), 
              freq = 12)
rail.ts <- ts(travel.data$rail_pm, start = c(1990, 1), end = c(2004, 4), 
              freq = 12)

air.train.ts <- window(air.ts, start = c(1990, 1), end = c(1990, n.train))
air.valid.ts <- window(air.ts, start = c(1990, n.train + 1), 
                       end = c(1990, n.train + n.valid))

auto.train.ts <- window(auto.ts, start = c(1990, 1), end = c(1990, n.train))
auto.valid.ts <- window(auto.ts, start = c(1990, n.train + 1), 
                        end = c(1990, n.train + n.valid))

rail.train.ts <- window(rail.ts, start = c(1990, 1), end = c(1990, n.train))
rail.valid.ts <- window(rail.ts, start = c(1990, n.train + 1), 
                        end = c(1990, n.train + n.valid))
rail.train.ts
rail.ts
# Plot the pre-911 air time series
plot(air.train.ts, xlab="Time", 
     ylab="Air Revenue Passenger Miles (billions)", 
     main="Pre 911 Air Time Series",
     axes=F
     )
axis(side=1, at=seq(1990, 2002, by=1))
axis(side=2, at=seq(25, 70, by=10))
box()

# add the trendline 
air.pre.911.expo.trend <- tslm(air.train.ts ~ trend, lambda = 0) # expo
air.pre.911.linear.trend <- tslm(air.train.ts ~ trend, lambda = 1) # linear
summary(air.pre.911.expo.trend)
summary(air.pre.911.linear.trend)
lines(air.pre.911.expo.trend$fitted, lwd=2, col="blue")
lines(air.pre.911.linear.trend$fitted, lwd=2, col="red")
air.pre.911.expo.trend$data

# Observations(in terms of level, trend, and seaonality components):
#
# The time plot is based on pre September 11 period - from Jan. 1990 to 
# Aug. 2001. The revenue shows an exponential upward trend with
# multiplicative seasonality. 
# 

#################################################
# Build model & forecast
# To capture the trend and the multiplicative
# seasonality
#################################################

air.train.lm.expo <- tslm(air.train.ts ~ trend + season,
                          lambda = 0)
summary(air.train.lm.expo)
air.train.lm.expo.pred <- forecast(air.train.lm.expo, 
                                   h = n.valid,
                                   level=0)

# plot the prediction
plot(air.train.lm.expo.pred)

# create ACF plot on the residuals
res <- residuals(air.train.lm.expo.pred)
plot(res, ylab="Residuals",xlab="Year")
acf(res)
# Draw a histogram on the residuals to check if they are normally distributed.
hist(res, breaks="FD", xlab="Residuals", 
     main="Histogram of residuals")
#x <- -0.2:0.2
#lines(x, dnorm(x,0,sd(res)),lwd=2, col="blue")

#################################################
# Model and forecast Air, Rail, and Auto data
#################################################
# Air - linear trend with additive seasonality
air.train.lm.linear <- tslm(air.train.ts ~ trend + season, lambda = 1)
air.train.lm.linear.pred <- forecast(air.train.lm.linear, 
                                     h = n.valid,
                                     level=0)
summary(air.train.lm.linear)
# Auto - linear trend with additive seasonality
auto.train.lm.linear <- tslm(auto.train.ts ~ trend + season, lambda = 1)
auto.train.lm.linear.pred <- forecast(auto.train.lm.linear, 
                                      h = n.valid,
                                      level=0)
# Rail - quadratic trend with additive seasonality
rail.train.lm.quadr <- tslm(rail.train.ts ~ trend + I(trend^2) + season)
rail.train.lm.quadr.pred <- forecast(rail.train.lm.quadr, 
                                     h = n.valid,
                                    level=0)

# Plot air model and forecast
plot(air.train.lm.linear.pred, ylab = "Airline Revenue ($Billions)", 
     ylim =c(20, 80), xlab = "Time", bty = "l", xaxt = "n", 
     xlim = c(1990,2006), main = "", flty = 2)
axis(1, at = seq(1990, 2006, 1), labels = format(seq(1990, 2006, 1)))
lines(air.train.lm.linear.pred$fitted, lwd = 2, col = "blue")
lines(air.valid.ts)
lines(c(2001.65, 2001.70), c(0, 120)) 
lines(c(2004.25, 2004.25), c(0, 120))
text(1994.25, 80, "Training")
text(2003.0, 80, "Validation")
text(2005.25, 80, "Future")
#arrows(2004 - 3, 2400, 1991.25, 2400, code = 3, length = 0.1, lwd = 1,
#       angle = 30)
#arrows(2004.5 - 3, 2400, 2004, 2400, code = 3, length = 0.1, lwd = 1,
#       angle = 30)
#arrows(2004.5, 2400, 2006, 2400, code = 3, length = 0.1, lwd = 1, 
#       angle = 30)

# Plot auto model and forecast
plot(auto.train.lm.linear.pred, ylab = "Vehicle Miles Traveled (Billions)", 
     ylim =c(120, 280),
     xlab = "Time", bty = "l", xaxt = "n", xlim = c(1990,2006), 
     main = "", flty = 2)
axis(1, at = seq(1990, 2006, 1), labels = format(seq(1990, 2006, 1)))
lines(auto.train.lm.linear.pred$fitted, lwd = 2, col = "blue")
lines(auto.valid.ts)
lines(c(2001.65, 2001.70), c(0, 300)) 
lines(c(2004.25, 2004.25), c(0, 300))
text(1994.25, 280, "Training")
text(2003.0, 280, "Validation")
text(2005.25, 280, "Future")

# Plot rail model and forecast
plot(rail.train.lm.quadr.pred, ylab = "Rail Passenger Miles (Millions)", 
     #ylim =c(120, 280),
     xlab = "Time", bty = "l", xaxt = "n", xlim = c(1990,2006), 
     main = "", flty = 2)
axis(1, at = seq(1990, 2006, 1), labels = format(seq(1990, 2006, 1)))
lines(rail.train.lm.quadr.pred$fitted, lwd = 2, col = "blue")
lines(rail.valid.ts)
lines(c(2001.65, 2001.70), c(0, 700)) 
lines(c(2004.25, 2004.25), c(0, 700))
text(1994.25, 650, "Training")
text(2003.0, 650, "Validation")
text(2005.25, 650, "Future")

#################################################
# Model validation - RMSE & MAPE
#################################################
#Get RMSE MAPE
accuracy(snaive.pred,valid.ts)

# histogram of the forecast errors from the naive forecasts
hist(snaive.pred$residuals, ylab="Frequency", xlab = "Forecast Error", 
     bty="l", main = "Forecast Error Histogram")
box()

# Time plot of the seasonal naive forecasts and the actual sales numbers 
# in the validation period

plot(as.xts(valid.ts), xlab="Time", ylab = "Sales (Dollars)", 
     #xlim=c(2001, 2001.9), 
     major.format = "%Y-%m",
     yaxt="n", main="Forecast vs Actual Sales")
axis(side=2, at=seq(1500, 110000, by=20000))
lines(as.xts(snaive.pred$mean), lwd = 2, col = "blue", lty = 2)



