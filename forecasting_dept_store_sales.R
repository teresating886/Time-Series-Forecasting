library("forecast")
dept.store.data <- read.csv("../PTSF-Datasets/DepartmentStoreSales.csv")
sales.all.ts <- ts(dept.store.data$Sales, freq = 4, start = 1900)
plot(sales.all.ts)
n.train <- 20
n.valid <- 2
sales.train.ts <- window(sales.all.ts, start = c(1900, 1), 
                         end = c(1900, n.train) )
sales.valid.ts <- window(sales.all.ts, start = c(1900, n.train + 1), 
                         end = c(1900, n.train + n.valid) )

sales.lm.expo <- tslm(sales.train.ts ~ trend + season, lambda = 0)
summary(sales.lm.expo)
sales.lm.expo.pred <- forecast(sales.lm.expo, h = n.valid, level=0)

plot(sales.lm.expo.pred, xlim = c(1900,1906), ylim = c(40000, 100000),  
     ylab = "Sales", xlab = "Quarter", bty = "l", main = "", 
     yaxt ="n", xaxt ="n", type = "o")
axis(2, at = seq(40000, 100000, 10000), 
     labels = format(seq(40000, 100000, 10000), 
                     scientific=FALSE))
axis(1, at = seq(1900.25, 1906, .5), labels = format(seq(2, 24, 2)))
lines(sales.lm.expo$fitted, lwd = 2, col = "blue", type = "o")
lines(sales.valid.ts, lwd = 2, type = "o")
lines(c(1904.90, 1904.90), c(0, 100000)) 
text(1902.50, 100000, "Training")
text(1905.50, 100000, "Validation")

#existing & residuals
res <- sales.train.ts - sales.lm.expo$fitted.values
acf(res)
plot(res, xlim = c(1900,1905),   
ylab = "Sales", xlab = "Quarter", bty = "l", main = "", xaxt ="n", type = "o")
axis(1, at = seq(1900.25, 1905, .5), labels = format(seq(2, 20, 2)))
#lines(sales.lm.expo$fitted, lwd = 2, col = "blue", type = "o")
#lines(sales.valid.ts, lwd = 2, type = "o")
#lines(c(1904.90, 1904.90), c(0, 100000)) 
plot(sales.lm.expo$residuals)
#lines(c(1905.30, 1905.30), c(0, 100000))
text(1902.50, 100000, "Training")
text(1905.50, 100000, "Validation")
sales.lm.expo$residuals
sales.lm.expo$fitted.values
sales.train.ts


##########################################################
##########################################################


# Smoothing model
# Use Holt Winter's exponential smoothing method

library("forecast")
dept.store.data <- read.csv("../PTSF-Datasets/DepartmentStoreSales.csv")
sales.all.ts <- ts(dept.store.data$Sales, freq = 4, start = 1900)
plot(sales.all.ts)
n.train <- 20
n.valid <- 4
sales.train.ts <- window(sales.all.ts, start = c(1900, 1), 
                         end = c(1900, n.train) )
sales.valid.ts <- window(sales.all.ts, start = c(1900, n.train + 1), 
                         end = c(1900, n.train + n.valid) )
hw.expo <- ets(sales.train.ts, model="AAM", restrict = FALSE, 
               alpha=0.2, beta=0.15, gamma=0.05)
hw.expo.pred <- forecast(hw.expo, h=n.valid, level=0)

plot(hw.expo.pred, xlim = c(1900,1906), ylim = c(40000, 110000),  
     ylab = "Sales", xlab = "Quarter", bty = "l", main = "", 
     yaxt ="n", xaxt ="n", type = "o")
axis(2, at = seq(40000, 100000, 10000), 
     labels = format(seq(40000, 100000, 10000), 
                     scientific=FALSE))
axis(1, at = seq(1900.25, 1906, .5), labels = format(seq(2, 24, 2)))
lines(hw.expo$fitted, lwd = 2, col = "blue", type = "o")
lines(sales.valid.ts, lwd = 2, type = "o")
lines(c(1904.90, 1904.90), c(0, 120000)) 
text(1902.50, 110000, "Training")
text(1905.50, 110000, "Validation")

# Get MAPE
accuracy(hw.expo.pred, sales.valid.ts)

# Manual calculation
( (1415.434145/60800) + (3243.505741/64900) )/2
# + (5143.985579/76997) + (8262.301585/103337)
##################################################
# Apply differencing and then simple exponential smoothing

# Apply detrend first then deseasonality

diff.season.first <- diff(diff(sales.all.ts, lag = 1), lag = 4)

n.train <- length(diff.season.first)-4
n.valid <- 4

d.train.ts <- window(diff.season.first, start = c(1901, 2), 
                     end = c(1901, n.train) )
d.valid.ts <- window(diff.season.first, start = c(1901, n.train + 1), 
                     end = c(1901, n.train + n.valid) )
sim.expo <- ets(d.train.ts, model = "ANN", alpha=.2)
sim.expo.pred <- forecast(sim.expo, h=n.valid, level=0)

plot(sim.expo.pred, xlim = c(1901,1906), 
     ylab = "Sales", xlab = "Quarter", bty = "l", main = "De-trend first", 
     xaxt ="n", type = "o")
axis(1, at = seq(1901.25, 1906, .5), labels = format(seq(6, 24, 2)))
lines(sim.expo$fitted, lwd = 2, col = "blue", type = "o")
lines(d.valid.ts, lwd = 2, type = "o")

accuracy(sim.expo.pred, d.valid.ts) #MAPE: 53.80705

###########
# Apply deseasonality first

diff.season.first <- diff(diff(sales.all.ts, lag = 4), lag = 1)

n.train <- length(diff.season.first)-4
n.valid <- 4

d.train.ts <- window(diff.season.first, start = c(1901, 2), 
                     end = c(1901, n.train) )
d.valid.ts <- window(diff.season.first, start = c(1901, n.train + 1), 
                     end = c(1901, n.train + n.valid) )
sim.expo <- ets(d.train.ts, model = "ANN", alpha=.2)
sim.expo.pred <- forecast(sim.expo, h=n.valid, level=0)

plot(sim.expo.pred, xlim = c(1901,1906), 
     ylab = "Sales", xlab = "Quarter", bty = "l", main = "De-season first", 
     xaxt ="n", type = "o")
axis(1, at = seq(1901.25, 1906, .5), labels = format(seq(6, 24, 2)))
lines(sim.expo$fitted, lwd = 2, col = "blue", type = "o")
lines(d.valid.ts, lwd = 2, type = "o")

accuracy(sim.expo.pred, d.valid.ts) #MAPE: 53.80705

#############################
# e. 
d.valid.ts
