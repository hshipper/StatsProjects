library(plyr) ###for ldply()
library(XML) ###for readHTMLTable()
library(lattice) ###for plots

url <- "http://cdec.water.ca.gov/cgi-progs/selectQuery?station_id=SHA&dur_code=M&sensor_num=15&start_date=01/01/1960+00:00&end_date=01/01/2015+00:00"
db <- readHTMLTable(url, stringsAsFactors = FALSE)

reslevel <- db[[1]]
names(reslevel) <- c("Date", "Storage")
reslevel[,1] <- sapply(reslevel[,1], function(x) paste0("01/", x))
reslevel[,1] <- as.Date(reslevel[,1], "%d/%m%Y")
reslevel$Storage <- as.numeric(reslevel$Storage)
reslevel <- ts(reslevel$Storage, frequency = 12, start = c(1960, 1))
reslevel <- as.numeric(reslevel)
#####Fill missing value for January 1970
which( is.na(reslevel == TRUE)) #returns 121 as location of January 1970
subset.previous <- reslevel[108:120] #use previous year to estimate
subset.future <- reslevel[134:122] #use next year to estimate
fit.sub.pre <- ar.yw(subset.previous)
fit.sub.fut <- ar.yw(subset.future)
tmp <- (predict(fit.sub.pre)$pred[1] + predict(fit.sub.fut)$pred[1])/2
plot(reslevel, type = "l")
points(x = 1970, y = tmp, col = "red")
reslevel[121] <- tmp
reslevel <- ts(reslevel, start = c(1960, 1), frequency = 12)
plot(reslevel, type = "l" , main = "Monthly Reservoir Storage 1960-2015", 
     ylab = "Reservoir Storage (1,000,000 AF)", yaxt = "n", sub = "Dotted line denotes historical average")
axis(2, at = (1:4*(1000000)), labels = 1:4, las = 2)
abline(h = mean(reslevel), lty = 3, col = "red")

par(mfrow = c(2, 1))
plot(log(reslevel), ylab = "", type = "l", main = "Monthly Reservoir Storage 1960-2015 (log)")
plot(diff(log(reslevel)), type = "l", ylab = "", main = "Difference of Log Storage Levels at Lag 1")

log.res <- log(reslevel[-661])
hatm <- filter(log.res, sides = 2, c(.5, rep(1, 11), .5)/12)

A <- matrix(log.res, ncol = 12, byrow = "TRUE")
M <- matrix(hatm, ncol = 12, byrow = "TRUE")
mu <- array(0,12)
for(k in 1:6) mu[k] = sum(A[2:55,k]-M[2:55,k])/54
for(k in 7:12) mu[k] = sum(A[1:54,k]-M[1:54,k])/54
hats <- rep(mu - mean(mu), 55)
trends <- array(0, 55)
for(i in 1:55) trends[i] <- sum(A[i, 1:12])/12
fulltrends <- rep(trends, each = 12)

desea <- log.res - hats
plot(log.res, type = "l", main = "Before and After Deseasonalizing", ylab = "")
plot(desea, type = "l", ylab = "")
lines(desea, col = "red")
detrend <- filter(desea, sides = 1, c(rep(1, 4)/4))
res <- desea - detrend
plot(res, main = "Residuals after Moving Average Detrending", ylab = "")

which.max(res) ####returns 217, location of outlier
sub.pre <- res[197:216]
sub.fut <- res[238:218]
fit.pre <- ar.yw(sub.pre)
fit.fut <- ar.yw(sub.fut)
res[217] <- (predict(fit.pre)$pred[1] + predict(fit.fut)$pred[1]) /2
plot(res)

qqnorm(res)
qqline(res)

acf(res, na.action = na.pass)
pacf(res, na.action = na.pass)

fit.ar <- ar.ols(res[-(1:3)], demean = T)
tmp <- detrend
trend <- function(tmp){
  c(tmp, (1/4) * (mean(tmp, na.rm = TRUE) + tmp[length(tmp)] + tmp[length(tmp)-1] + tmp[length(tmp)-2]))
}
#####Forecasting for January, February, March 2015
detrend <- trend(detrend)
trend.jan <- detrend[length(detrend)]
jan <- predict(fit.ar)$pred[1] + hats[1] + trend.jan
detrend <- trend(detrend)
trend.feb <- detrend[length(detrend)]
feb <- predict(fit.ar, n.ahead = 2)$pred[2] + hats[2] + trend.feb
detrend <- trend(detrend)
trend.mar <- detrend[length(detrend)]
mar <- predict(fit.ar, n.ahead = 3)$pred[3] + hats[3] + trend.mar
