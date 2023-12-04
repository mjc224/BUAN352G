#BUAN352
#Group G
#Analysis of Magnificent 7 stocks

#----------------------------------------------------------
#MSFT
library(forecast)
options(scipien=999)
MSFT <- read.csv("MSFT.csv")
MSFT$date <- as.Date(MSFT$date, format="%m/%d/%Y")


###### 2 year subset
MSFT.2yr <- subset(MSFT, date >= "2021-11-30" & date < "2023-12-01")
#252 trading days a year
MSFT.2yr.ts <- ts(MSFT.2yr$close, frequency = 252)
#plot(MSFT.2yr.ts)
plot(MSFT.2yr.ts, xaxt = "n", main = "MSFT Time Series (2 Years)", xlab = "Time", ylab = "Price")
axis(1, at=c(1,2,3), labels=c("11/30/21","11/30/22","11/30/23"))
MSFT.2yr.lm <- lm(MSFT.2yr$close ~ time(MSFT.2yr.ts))
abline(MSFT.2yr.lm, col = "green", lwd = 2)
summary(MSFT.2yr.lm)

options(scipien=999)
MSFT.2yr.lm <- tslm(MSFT.2yr.ts ~ trend + season)
summary(MSFT.2yr.lm)


####### 100 day subset
MSFT.100day <- subset(MSFT, date >= "2023-7-12" & date < "2023-12-01")
#252 trading days a year
MSFT.100day.ts <- ts(MSFT.100day$close, frequency = 252)
#plot(MSFT.100day.ts)
plot(MSFT.100day.ts, xaxt = "n", main = "MSFT Time Series (100 day)", xlab = "Time", ylab = "Price")
axis(1, at=c(1,1.2,1.4), labels=c("7/12/2023", "9/20/2023", "11/30/2023"))

MSFT.100day.lm <- lm(MSFT.100day$close ~ time(MSFT.100day.ts))

abline(MSFT.100day.lm, col = "green", lwd = 2)
summary(MSFT.100day.lm)

options(scipien=999)
MSFT.100day.lm <- tslm(MSFT.100day.ts ~ trend)
summary(MSFT.100day.lm)

#----------------------------------------------------------
#AMZN
library(forecast)
AMZN <- read.csv("AMZN.csv")
AMZN$date <- as.Date(AMZN$date, format="%m/%d/%Y")
AMZN <- subset(AMZN, date >= "2018-01-01")

###### 2 year subset
AMZN.2yr <- subset(AMZN, date >= "2021-11-30" & date < "2023-12-01")
#252 trading days a year
AMZN.2yr.ts <- ts(AMZN.2yr$close, frequency = 252)
#plot(AMZN.2yr.ts)
plot(AMZN.2yr.ts, xaxt = "n", main = "AMZN Time Series (2 Years)", xlab = "Time", ylab = "Price")
axis(1, at=c(1,2,3), labels=c("11/30/21","11/30/22","11/30/23"))
AMZN.2yr.lm <- lm(AMZN.2yr$close ~ time(AMZN.2yr.ts))
abline(AMZN.2yr.lm, col = "red", lwd = 2)
summary(AMZN.2yr.lm)

options(scipien=999)
AMZN.2yr.lm <- tslm(AMZN.2yr.ts ~ trend + season)
summary(AMZN.2yr.lm)


####### 100 day subset
AMZN.100day <- subset(AMZN, date >= "2023-7-12" & date < "2023-12-01")
#252 trading days a year
AMZN.100day.ts <- ts(AMZN.100day$close, frequency = 252)
#plot(AMZN.100day.ts)
plot(AMZN.100day.ts, xaxt = "n", main = "AMZN Time Series (100 day)", xlab = "Time", ylab = "Price")
axis(1, at=c(1,1.2,1.4), labels=c("7/12/2023", "9/20/2023", "11/30/2023"))

AMZN.100day.lm <- lm(AMZN.100day$close ~ time(AMZN.100day.ts))

abline(AMZN.100day.lm, col = "green", lwd = 2)
summary(AMZN.100day.lm)

options(scipien=999)
AMZN.100day.lm <- tslm(AMZN.100day.ts ~ trend)
summary(AMZN.100day.lm)


#----------------------------------------------------------
#NVDA
library(forecast)
NVDA <- read.csv("NVDA.csv")
NVDA$date <- as.Date(NVDA$date, format="%m/%d/%Y")


###### 2 year subset
NVDA.2yr <- subset(NVDA, date >= "2021-11-30" & date < "2023-12-01")
#252 trading days a year
NVDA.2yr.ts <- ts(NVDA.2yr$close, frequency = 252)
#plot(NVDA.2yr.ts)
plot(NVDA.2yr.ts, xaxt = "n", main = "NVDA Time Series (2 Years)", xlab = "Time", ylab = "Price")
axis(1, at=c(1,2,3), labels=c("11/30/21","11/30/22","11/30/23"))
NVDA.2yr.lm <- lm(NVDA.2yr$close ~ time(NVDA.2yr.ts))
abline(NVDA.2yr.lm, col = "green", lwd = 2)
summary(NVDA.2yr.lm)

options(scipien=999)
NVDA.2yr.lm <- tslm(NVDA.2yr.ts ~ trend + season)
summary(NVDA.2yr.lm)


####### 100 day subset
NVDA.100day <- subset(NVDA, date >= "2023-7-12" & date < "2023-12-01")
#252 trading days a year
NVDA.100day.ts <- ts(NVDA.100day$close, frequency = 252)
#plot(NVDA.100day.ts)
plot(NVDA.100day.ts, xaxt = "n", main = "NVDA Time Series (100 day)", xlab = "Time", ylab = "Price")
axis(1, at=c(1,1.2,1.4), labels=c("7/12/2023", "9/20/2023", "11/30/2023"))

NVDA.100day.lm <- lm(NVDA.100day$close ~ time(NVDA.100day.ts))

abline(NVDA.100day.lm, col = "green", lwd = 2)
summary(NVDA.100day.lm)

options(scipien=999)
NVDA.100day.lm <- tslm(NVDA.100day.ts ~ trend)
summary(NVDA.100day.lm)


#----------------------------------------------------------
#AAPL
library(forecast)
AAPL <- read.csv("AAPL.csv")
AAPL$date <- as.Date(AAPL$date, format="%m/%d/%Y")


###### 2 year subset
AAPL.2yr <- subset(AAPL, date >= "2021-11-30" & date < "2023-12-01")
#252 trading days a year
AAPL.2yr.ts <- ts(AAPL.2yr$close, frequency = 252)
#plot(AAPL.2yr.ts)
plot(AAPL.2yr.ts, xaxt = "n", main = "AAPL Time Series (2 Years)", xlab = "Time", ylab = "Price")
axis(1, at=c(1,2,3), labels=c("11/30/21","11/30/22","11/30/23"))
AAPL.2yr.lm <- lm(AAPL.2yr$close ~ time(AAPL.2yr.ts))
abline(AAPL.2yr.lm, col = "green", lwd = 2)
summary(AAPL.2yr.lm)

options(scipien=999)
AAPL.2yr.lm <- tslm(AAPL.2yr.ts ~ trend + season)
summary(AAPL.2yr.lm)


####### 100 day subset
AAPL.100day <- subset(AAPL, date >= "2023-7-12" & date < "2023-12-01")
#252 trading days a year
AAPL.100day.ts <- ts(AAPL.100day$close, frequency = 252)
#plot(AAPL.100day.ts)
plot(AAPL.100day.ts, xaxt = "n", main = "AAPL Time Series (100 day)", xlab = "Time", ylab = "Price")
axis(1, at=c(1,1.2,1.4), labels=c("7/12/2023", "9/20/2023", "11/30/2023"))

AAPL.100day.lm <- lm(AAPL.100day$close ~ time(AAPL.100day.ts))

abline(AAPL.100day.lm, col = "red", lwd = 2)
summary(AAPL.100day.lm)

options(scipien=999)
AAPL.100day.lm <- tslm(AAPL.100day.ts ~ trend)
summary(AAPL.100day.lm)



#----------------------------------------------------------
#GOOG (Alphabet)
library(forecast)
GOOG <- read.csv("GOOG.csv")
GOOG$date <- as.Date(GOOG$date, format="%m/%d/%Y")


###### 2 year subset
GOOG.2yr <- subset(GOOG, date >= "2021-11-30" & date < "2023-12-01")
#252 trading days a year
GOOG.2yr.ts <- ts(GOOG.2yr$close, frequency = 252)
#plot(GOOG.2yr.ts)
plot(GOOG.2yr.ts, xaxt = "n", main = "GOOG Time Series (2 Years)", xlab = "Time", ylab = "Price")
axis(1, at=c(1,2,3), labels=c("11/30/21","11/30/22","11/30/23"))
GOOG.2yr.lm <- lm(GOOG.2yr$close ~ time(GOOG.2yr.ts))
abline(GOOG.2yr.lm, col = "red", lwd = 2)
summary(GOOG.2yr.lm)

options(scipien=999)
GOOG.2yr.lm <- tslm(GOOG.2yr.ts ~ trend + season)
summary(GOOG.2yr.lm)


####### 100 day subset
GOOG.100day <- subset(GOOG, date >= "2023-7-12" & date < "2023-12-01")
#252 trading days a year
GOOG.100day.ts <- ts(GOOG.100day$close, frequency = 252)
#plot(GOOG.100day.ts)
plot(GOOG.100day.ts, xaxt = "n", main = "GOOG Time Series (100 day)", xlab = "Time", ylab = "Price")
axis(1, at=c(1,1.2,1.4), labels=c("7/12/2023", "9/20/2023", "11/30/2023"))

GOOG.100day.lm <- lm(GOOG.100day$close ~ time(GOOG.100day.ts))

abline(GOOG.100day.lm, col = "green", lwd = 2)
summary(GOOG.100day.lm)

options(scipien=999)
GOOG.100day.lm <- tslm(GOOG.100day.ts ~ trend)
summary(GOOG.100day.lm)



#----------------------------------------------------------
#TSLA
library(forecast)
TSLA <- read.csv("TSLA.csv")
TSLA$date <- as.Date(TSLA$date, format="%m/%d/%Y")


###### 2 year subset
TSLA.2yr <- subset(TSLA, date >= "2021-11-30" & date < "2023-12-01")
#252 trading days a year
TSLA.2yr.ts <- ts(TSLA.2yr$close, frequency = 252)
#plot(TSLA.2yr.ts)
plot(TSLA.2yr.ts, xaxt = "n", main = "TSLA Time Series (2 Years)", xlab = "Time", ylab = "Price")
axis(1, at=c(1,2,3), labels=c("11/30/21","11/30/22","11/30/23"))
TSLA.2yr.lm <- lm(TSLA.2yr$close ~ time(TSLA.2yr.ts))
abline(TSLA.2yr.lm, col = "red", lwd = 2)
summary(TSLA.2yr.lm)

options(scipien=999)
TSLA.2yr.lm <- tslm(TSLA.2yr.ts ~ trend + season)
summary(TSLA.2yr.lm)


####### 100 day subset
TSLA.100day <- subset(TSLA, date >= "2023-7-12" & date < "2023-12-01")
#252 trading days a year
TSLA.100day.ts <- ts(TSLA.100day$close, frequency = 252)
#plot(TSLA.100day.ts)
plot(TSLA.100day.ts, xaxt = "n", main = "TSLA Time Series (100 day)", xlab = "Time", ylab = "Price")
axis(1, at=c(1,1.2,1.4), labels=c("7/12/2023", "9/20/2023", "11/30/2023"))

TSLA.100day.lm <- lm(TSLA.100day$close ~ time(TSLA.100day.ts))

abline(TSLA.100day.lm, col = "red", lwd = 2)
summary(TSLA.100day.lm)

options(scipien=999)
TSLA.100day.lm <- tslm(TSLA.100day.ts ~ trend)
summary(TSLA.100day.lm)





#----------------------------------------------------------
#META
library(forecast)
META <- read.csv("META.csv")
META$date <- as.Date(META$date, format="%m/%d/%Y")


###### 2 year subset
META.2yr <- subset(META, date >= "2021-11-30" & date < "2023-12-01")
#252 trading days a year
META.2yr.ts <- ts(META.2yr$close, frequency = 252)
#plot(META.2yr.ts)
plot(META.2yr.ts, xaxt = "n", main = "META Time Series (2 Years)", xlab = "Time", ylab = "Price")
axis(1, at=c(1,2,3), labels=c("11/30/21","11/30/22","11/30/23"))
META.2yr.lm <- lm(META.2yr$close ~ time(META.2yr.ts))
abline(META.2yr.lm, col = "green", lwd = 2)
summary(META.2yr.lm)

options(scipien=999)
META.2yr.lm <- tslm(META.2yr.ts ~ trend + season)
summary(META.2yr.lm)


####### 100 day subset
META.100day <- subset(META, date >= "2023-7-12" & date < "2023-12-01")
#252 trading days a year
META.100day.ts <- ts(META.100day$close, frequency = 252)
#plot(META.100day.ts)
plot(META.100day.ts, xaxt = "n", main = "META Time Series (100 day)", xlab = "Time", ylab = "Price")
axis(1, at=c(1,1.2,1.4), labels=c("7/12/2023", "9/20/2023", "11/30/2023"))

META.100day.lm <- lm(META.100day$close ~ time(META.100day.ts))

abline(META.100day.lm, col = "green", lwd = 2)
summary(META.100day.lm)

options(scipien=999)
META.100day.lm <- tslm(META.100day.ts ~ trend)
summary(META.100day.lm)


META.100day.lm$coefficients[2]


