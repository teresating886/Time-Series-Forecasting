library("forecast")

#######################################
# data preparation and partition
#######################################
# 
transit.data <- read.csv("../PTSF-Datasets/bicup2006.csv",
                         na.strings = 'NA',
                         stringsAsFactors = F)

str(transit.data)
# zero values cause Infinite MAPE, so replace zero with .01
#transit.data$DEMAND[transit.data$DEMAND == 0] <- 0.1
#transit.data$DEMAND 
# Remove zero values

ms.transit.all.ts <- msts(transit.data$DEMAND, seasonal.periods=c(63,441))

# verify data is there
plot(ms.transit.all.ts, 
     xlab="Time", ylab = "Demand" )
# there are 63 record counts per day. The validation set is 3-day period
# Set partition size
n.valid <- 3*63
n.train <- length(transit.data$DEMAND) - n.valid

# Partition training and validation data sets
train.ts <- window(ms.transit.all.ts, start = c(1,1), 
                         end = c(1,n.train))
valid.ts <- window(ms.transit.all.ts, start = c(1, n.train + 1), 
                         end = c(1, n.train + n.valid) )

#######################################
# linear regression model
#######################################
# multiple seasons - daily and weekly
# linear trend with additive seasonality
transit.train.linear <- tslm(train.ts ~ trend + season, lambda = 1)
transit.train.linear.pred <- forecast(transit.train.linear, 
                                     h = n.valid,
                                     level=0)
# Plot the training and validation set
plot(transit.train.linear.pred, flty = 2)
lines(transit.train.linear.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts, lwd = 2)
lines(c(3.58, 3.58), c(-5, 150)) 
lines(c(4, 4), c(-5, 150))
text(2.5, 120, "Training")
text(3.7, 120, "Validation")

summary(transit.train.linear)

# Check the ACF plot on the residuals, and we can see a strong positive 
# autocorrelation from lag 1. 
plot(transit.train.linear.pred$residuals)
acf(transit.train.linear.pred$residuals)

# Accuracy
accuracy(transit.train.linear.pred, valid.ts)

#######################################
# Forecast future three-day period
#######################################
transit.forecast <- tslm(ms.transit.all.ts ~ trend + season, lambda = 1)
transit.forecast.pred <- forecast(transit.forecast, 
                                  h = n.valid,
                                  level=0)

plot(transit.forecast.pred, flty = 2)
lines(transit.forecast.pred$fitted, lwd = 2, col = "blue")
lines(valid.ts, lwd = 1)
lines(c(3.58, 3.58), c(-5, 150)) 
lines(c(4, 4), c(-5, 150))
text(2.5, 120, "Training")
text(3.8, 120, "Validation")
text(4.2, 120, "Future")
# Forecast values for the future 3-day period
round(transit.forecast.pred$mean)










