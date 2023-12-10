#BUAN352
#Group G
#Analysis of Magnificent 7 stocks

#----------------------------------------------------------
#MSFT
library(forecast)
options(scipen=999)
MSFT <- read.csv("MSFT.csv")
MSFT$date <- as.Date(MSFT$date, format="%m/%d/%Y")


###### 2 year subset
MSFT.2yr <- subset(MSFT, date >= "2021-11-30" & date < "2023-12-01")
#252 trading days a year
MSFT.2yr.ts <- ts(MSFT.2yr$close)
#plot(MSFT.2yr.ts)
plot(MSFT.2yr.ts, xaxt = "n", main = "MSFT Time Series (2 Years)", xlab = "Time", ylab = "Price")
axis(1, at=c(0,250,500), labels=c("11/30/21","11/30/22","11/30/23"))

MSFT.2yr.poly.lm <- tslm(MSFT.2yr.ts ~ trend + I(trend^2))
summary(MSFT.2yr.poly.lm)
lines(MSFT.2yr.poly.lm$fitted.values, col=4, lty=3, lwd=3)

text(250, max(MSFT.2yr.ts) - 2, sprintf("tslm trend: %.4f", MSFT.2yr.poly.lm$coefficients[3]), col = "black")


####### 100 day subset
MSFT.100day <- subset(MSFT, date >= "2023-7-12" & date < "2023-12-01")
#252 trading days a year
MSFT.100day.ts <- ts(MSFT.100day$close)
#plot(MSFT.100day.ts)
plot(MSFT.100day.ts, xaxt = "n", main = "MSFT Time Series (100 day)", xlab = "Time", ylab = "Price")
axis(1, at=c(0,50,100), labels=c("7/12/2023", "9/20/2023", "11/30/2023"))

MSFT.100day.poly.lm <- tslm(MSFT.100day.ts ~ trend + I(trend^2))
summary(MSFT.100day.poly.lm)

lines(MSFT.100day.poly.lm$fitted.values, col=4, lty=3, lwd=3)
text(50, max(MSFT.100day.ts) - 0.5, sprintf("tslm trend: %.4f", MSFT.100day.poly.lm$coefficients[3]), col = "black")

#----------------------------------------------------------
#AMZN
library(forecast)
AMZN <- read.csv("AMZN.csv")
AMZN$date <- as.Date(AMZN$date, format="%m/%d/%Y")
AMZN <- subset(AMZN, date >= "2018-01-01")

###### 2 year subset
AMZN.2yr <- subset(AMZN, date >= "2021-11-30" & date < "2023-12-01")
#252 trading days a year
AMZN.2yr.ts <- ts(AMZN.2yr$close)
#plot(AMZN.2yr.ts)
plot(AMZN.2yr.ts, xaxt = "n", main = "AMZN Time Series (2 Years)", xlab = "Time", ylab = "Price")
axis(1, at=c(0,250,500), labels=c("11/30/21","11/30/22","11/30/23"))


AMZN.2yr.poly.lm <- tslm(AMZN.2yr.ts ~ trend + I(trend^2))
summary(AMZN.2yr.poly.lm)
lines(AMZN.2yr.poly.lm$fitted.values, col=4, lty=3, lwd=3)

text(250, max(AMZN.2yr.ts) - 2, sprintf("tslm trend: %.4f", AMZN.2yr.poly.lm$coefficients[3]), col = "black")


####### 100 day subset
AMZN.100day <- subset(AMZN, date >= "2023-7-12" & date < "2023-12-01")
#252 trading days a year
AMZN.100day.ts <- ts(AMZN.100day$close)
#plot(AMZN.100day.ts)
plot(AMZN.100day.ts, xaxt = "n", main = "AMZN Time Series (100 day)", xlab = "Time", ylab = "Price")
axis(1, at=c(0,50,100), labels=c("7/12/2023", "9/20/2023", "11/30/2023"))


AMZN.100day.poly.lm <- tslm(AMZN.100day.ts ~ trend + I(trend^2))
summary(AMZN.100day.poly.lm)

lines(AMZN.100day.poly.lm$fitted.values, col=4, lty=3, lwd=3)
text(50, max(AMZN.100day.ts) - 0.5, sprintf("tslm trend: %.4f", AMZN.100day.poly.lm$coefficients[3]), col = "black")


#----------------------------------------------------------
#NVDA
library(forecast)
NVDA <- read.csv("NVDA.csv")
NVDA$date <- as.Date(NVDA$date, format="%m/%d/%Y")


###### 2 year subset
NVDA.2yr <- subset(NVDA, date >= "2021-11-30" & date < "2023-12-01")
#252 trading days a year
NVDA.2yr.ts <- ts(NVDA.2yr$close)
#plot(NVDA.2yr.ts)
plot(NVDA.2yr.ts, xaxt = "n", main = "NVDA Time Series (2 Years)", xlab = "Time", ylab = "Price")
axis(1, at=c(0,250,500), labels=c("11/30/21","11/30/22","11/30/23"))



NVDA.2yr.poly.lm <- tslm(NVDA.2yr.ts ~ trend + I(trend^2))
summary(NVDA.2yr.poly.lm)
lines(NVDA.2yr.poly.lm$fitted.values, col=4, lty=3, lwd=3)

text(250, max(NVDA.2yr.ts) - 2, sprintf("tslm trend: %.4f", NVDA.2yr.poly.lm$coefficients[3]), col = "black")


####### 100 day subset
NVDA.100day <- subset(NVDA, date >= "2023-7-12" & date < "2023-12-01")
#252 trading days a year
NVDA.100day.ts <- ts(NVDA.100day$close)
#plot(NVDA.100day.ts)
plot(NVDA.100day.ts, xaxt = "n", main = "NVDA Time Series (100 day)", xlab = "Time", ylab = "Price")
axis(1, at=c(0,50,100), labels=c("7/12/2023", "9/20/2023", "11/30/2023"))

NVDA.100day.poly.lm <- tslm(NVDA.100day.ts ~ trend + I(trend^2))
summary(NVDA.100day.poly.lm)

lines(NVDA.100day.poly.lm$fitted.values, col=4, lty=3, lwd=3)
text(50, max(NVDA.100day.ts) - 0.5, sprintf("tslm trend: %.4f", NVDA.100day.poly.lm$coefficients[3]), col = "black")

#----------------------------------------------------------
#AAPL
library(forecast)
AAPL <- read.csv("AAPL.csv")
AAPL$date <- as.Date(AAPL$date, format="%m/%d/%Y")


###### 2 year subset
AAPL.2yr <- subset(AAPL, date >= "2021-11-30" & date < "2023-12-01")
#252 trading days a year
AAPL.2yr.ts <- ts(AAPL.2yr$close)
#plot(AAPL.2yr.ts)
plot(AAPL.2yr.ts, xaxt = "n", main = "AAPL Time Series (2 Years)", xlab = "Time", ylab = "Price")
axis(1, at=c(0,250,500), labels=c("11/30/21","11/30/22","11/30/23"))

AAPL.2yr.poly.lm <- tslm(AAPL.2yr.ts ~ trend + I(trend^2))
summary(AAPL.2yr.poly.lm)
lines(AAPL.2yr.poly.lm$fitted.values, col=4, lty=3, lwd=3)

text(250, max(AAPL.2yr.ts) - 2, sprintf("tslm trend: %.4f", AAPL.2yr.poly.lm$coefficients[3]), col = "black")


####### 100 day subset
AAPL.100day <- subset(AAPL, date >= "2023-7-12" & date < "2023-12-01")
#252 trading days a year
AAPL.100day.ts <- ts(AAPL.100day$close)
#plot(AAPL.100day.ts)
plot(AAPL.100day.ts, xaxt = "n", main = "AAPL Time Series (100 day)", xlab = "Time", ylab = "Price")
axis(1, at=c(0,50,100), labels=c("7/12/2023", "9/20/2023", "11/30/2023"))

AAPL.100day.poly.lm <- tslm(AAPL.100day.ts ~ trend + I(trend^2))
summary(AAPL.100day.poly.lm)

lines(AAPL.100day.poly.lm$fitted.values, col=4, lty=3, lwd=3)
text(50, max(AAPL.100day.ts) - 0.5, sprintf("tslm trend: %.4f", AAPL.100day.poly.lm$coefficients[3]), col = "black")



#----------------------------------------------------------
#GOOG (Alphabet)
library(forecast)
GOOG <- read.csv("GOOG.csv")
GOOG$date <- as.Date(GOOG$date, format="%m/%d/%Y")


###### 2 year subset
GOOG.2yr <- subset(GOOG, date >= "2021-11-30" & date < "2023-12-01")
#252 trading days a year
GOOG.2yr.ts <- ts(GOOG.2yr$close)
#plot(GOOG.2yr.ts)
plot(GOOG.2yr.ts, xaxt = "n", main = "GOOG Time Series (2 Years)", xlab = "Time", ylab = "Price")
axis(1, at=c(0,250,500), labels=c("11/30/21","11/30/22","11/30/23"))

GOOG.2yr.poly.lm <- tslm(GOOG.2yr.ts ~ trend + I(trend^2))
summary(GOOG.2yr.poly.lm)
lines(GOOG.2yr.poly.lm$fitted.values, col=4, lty=3, lwd=3)

text(250, max(GOOG.2yr.ts) - 2, sprintf("tslm trend: %.4f", GOOG.2yr.poly.lm$coefficients[3]), col = "black")


####### 100 day subset
GOOG.100day <- subset(GOOG, date >= "2023-7-12" & date < "2023-12-01")
#252 trading days a year
GOOG.100day.ts <- ts(GOOG.100day$close)
#plot(GOOG.100day.ts)
plot(GOOG.100day.ts, xaxt = "n", main = "GOOG Time Series (100 day)", xlab = "Time", ylab = "Price")
axis(1, at=c(0,50,100), labels=c("7/12/2023", "9/20/2023", "11/30/2023"))

GOOG.100day.poly.lm <- tslm(GOOG.100day.ts ~ trend + I(trend^2))
summary(GOOG.100day.poly.lm)

lines(GOOG.100day.poly.lm$fitted.values, col=4, lty=3, lwd=3)
text(50, max(GOOG.100day.ts) - 0.5, sprintf("tslm trend: %.4f", GOOG.100day.poly.lm$coefficients[3]), col = "black")



#----------------------------------------------------------
#TSLA
library(forecast)
TSLA <- read.csv("TSLA.csv")
TSLA$date <- as.Date(TSLA$date, format="%m/%d/%Y")


###### 2 year subset
TSLA.2yr <- subset(TSLA, date >= "2021-11-30" & date < "2023-12-01")
#252 trading days a year
TSLA.2yr.ts <- ts(TSLA.2yr$close)
#plot(TSLA.2yr.ts)
plot(TSLA.2yr.ts, xaxt = "n", main = "TSLA Time Series (2 Years)", xlab = "Time", ylab = "Price")
axis(1, at=c(0,250,500), labels=c("11/30/21","11/30/22","11/30/23"))

TSLA.2yr.poly.lm <- tslm(TSLA.2yr.ts ~ trend + I(trend^2))
summary(TSLA.2yr.poly.lm)
lines(TSLA.2yr.poly.lm$fitted.values, col=4, lty=3, lwd=3)

text(250, max(TSLA.2yr.ts) - 2, sprintf("tslm trend: %.4f", TSLA.2yr.poly.lm$coefficients[3]), col = "black")


####### 100 day subset
TSLA.100day <- subset(TSLA, date >= "2023-7-12" & date < "2023-12-01")
#252 trading days a year
TSLA.100day.ts <- ts(TSLA.100day$close)
#plot(TSLA.100day.ts)
plot(TSLA.100day.ts, xaxt = "n", main = "TSLA Time Series (100 day)", xlab = "Time", ylab = "Price")
axis(1, at=c(0,50,100), labels=c("7/12/2023", "9/20/2023", "11/30/2023"))

TSLA.100day.poly.lm <- tslm(TSLA.100day.ts ~ trend + I(trend^2))
summary(TSLA.100day.poly.lm)

lines(TSLA.100day.poly.lm$fitted.values, col=4, lty=3, lwd=3)
text(50, max(TSLA.100day.ts) - 0.5, sprintf("tslm trend: %.4f", TSLA.100day.poly.lm$coefficients[3]), col = "black")





#----------------------------------------------------------
#META
library(forecast)
META <- read.csv("META.csv")
META$date <- as.Date(META$date, format="%m/%d/%Y")


###### 2 year subset
META.2yr <- subset(META, date >= "2021-11-30" & date < "2023-12-01")
#252 trading days a year
META.2yr.ts <- ts(META.2yr$close)
#plot(META.2yr.ts)
plot(META.2yr.ts, xaxt = "n", main = "META Time Series (2 Years)", xlab = "Time", ylab = "Price")
axis(1, at=c(0,250,500), labels=c("11/30/21","11/30/22","11/30/23"))

META.2yr.poly.lm <- tslm(META.2yr.ts ~ trend + I(trend^2))
summary(META.2yr.poly.lm)
lines(META.2yr.poly.lm$fitted.values, col=4, lty=3, lwd=3)

text(250, max(META.2yr.ts) - 2, sprintf("tslm trend: %.4f", META.2yr.poly.lm$coefficients[3]), col = "black")


####### 100 day subset
META.100day <- subset(META, date >= "2023-7-12" & date < "2023-12-01")
#252 trading days a year
META.100day.ts <- ts(META.100day$close)
#plot(META.100day.ts)
plot(META.100day.ts, xaxt = "n", main = "META Time Series (100 day)", xlab = "Time", ylab = "Price")
axis(1, at=c(0,50,100), labels=c("7/12/2023", "9/20/2023", "11/30/2023"))

META.100day.poly.lm <- tslm(META.100day.ts ~ trend + I(trend^2))
summary(META.100day.poly.lm)

lines(META.100day.poly.lm$fitted.values, col=4, lty=3, lwd=3)
text(50, max(META.100day.ts) - 0.5, sprintf("tslm trend: %.4f", META.100day.poly.lm$coefficients[3]), col = "black")




#-----------------------------------------------------------------
#------------- 
#polynomial trend
#2 year analysis
company <- c("AAPL", "AMZN", "GOOG", "META", "MSFT", "NVDA","TSLA")
trend.poly.2yr <- c(AAPL.2yr.poly.lm$coefficients[3], AMZN.2yr.poly.lm$coefficients[3],
               GOOG.2yr.poly.lm$coefficients[3], META.2yr.poly.lm$coefficients[3],
               MSFT.2yr.poly.lm$coefficients[3], NVDA.2yr.poly.lm$coefficients[3],
               TSLA.2yr.poly.lm$coefficients[3])

#Creating table of trend values
table.poly <- data.frame(company,trend.poly.2yr)
table.poly

orderedTrend.poly <- table.poly[order(table.poly$trend.poly.2yr, decreasing = TRUE),]
colnames(orderedTrend.poly) <- c("Company", "Trend^2")
orderedTrend.poly

#------------
#100 day analysis
trend.poly.100day <- c(AAPL.100day.poly.lm$coefficients[3], AMZN.100day.poly.lm$coefficients[3],
                  GOOG.100day.poly.lm$coefficients[3], META.100day.poly.lm$coefficients[3],
                  MSFT.100day.poly.lm$coefficients[3], NVDA.100day.poly.lm$coefficients[3],
                  TSLA.100day.poly.lm$coefficients[3])

table100day.poly <- data.frame(company, trend.poly.100day)

ordered.100day.poly <-table100day.poly[order(table100day.poly$trend, decreasing = TRUE),]
colnames(ordered.100day.poly) <- c("Company", "Trend^2")
ordered.100day.poly


