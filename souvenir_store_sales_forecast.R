library(forecast)
library(xts) # time frame on the plot
###########################################
# Data exploration & data viz
###########################################
souvenir.data <- read.csv("../PTSF-Datasets/SouvenirSales.csv", 
                          header = F, sep=",",
                          skip = 1, 
                          col.names = c('Date', 'Sales', 'NA_col',
                                        'From_website'),
                          na.strings = 'NA',
                          stringsAsFactors = F)
souvenir.data <- souvenir.data[-3]
str(souvenir.data)
souvenir.ts <- ts(souvenir.data$Sales, start=c(1995,1), end=c(2001,12), 
                  freq=12)

# The plot for the original dataset
plot(souvenir.ts, xlab="Time", ylab="Sales (dollars)", 
     main="Original Dataset", axes=F)
axis(side=1, at=seq(1995, 2002, by=1))
axis(side=2, at=seq(1500, 110000, by=10000))
box()

# add the trendline to the original 
souvenir.ts.lm <- tslm(souvenir.ts ~ poly(trend, 2))
lines(souvenir.ts.lm$fitted, lwd=2, col="blue")

# transform the time series by calculating the natural log of the original data
souvenir.ts.log <- log(souvenir.ts)

# plot the transformed data
plot(souvenir.ts.log, xlab="Time", ylab="Sales (log)",
     main="Transformed dataset")

# add the trendline to the transformed 
souvenir.ts.log.lm <- tslm(souvenir.ts.log ~ poly(trend, 1))
lines(souvenir.ts.log.lm$fitted, lwd=2, col="blue")

#################################################
# Data partition - training set and valiation set
#################################################
fixed.n.valid <- 12 # forecast horizon, 12 month
fixed.n.train <- length(souvenir.ts) - fixed.n.valid
train.ts <- window(souvenir.ts, start=c(1995,1), end=c(1995,fixed.n.train))
valid.ts <- window(souvenir.ts, start=c(1995,fixed.n.train + 1), 
                   end=c(1995,fixed.n.train + fixed.n.valid))

#################################################
# Applying forecast model
#################################################

snaive.pred <- snaive(train.ts, h= fixed.n.valid)
snaive.pred

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



