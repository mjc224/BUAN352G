#BUAN352
#Group G


#-----------
#MSFT
library(forecast)
MSFT <- read.csv("MSFT.csv")
MSFT$date <- as.Date(MSFT$date, format="%m/%d/%Y")

#subset to start at 2019
MSFT.5yr <- subset(MSFT, date >= "2019-01-01")

#252 trading days a year
MSFT.5yr.ts <- ts(MSFT.5yr$close, frequency = 252) 
plot(MSFT.5yr.ts, xaxt = "n", main = "MSFT Time Series")
axis(1, at=1:6, labels=c(2019,2020,2021,2022,2023,2024))


#subset to make it start from 2022
MSFT.2yr <- subset(MSFT, date >= "2022-01-01")
#252 trading days a year
MSFT.2yr.ts <- ts(MSFT.2yr$close, frequency = 252)
plot(MSFT.2yr.ts)
plot(MSFT.2yr.ts, xaxt = "n", main = "MSFT Time Series")
axis(1, at=1:2, labels=c(2023,2024))

#subset to make 1 yr
MSFT.1yr <- subset(MSFT, date >= "2022-11-30")
#252 trading days a year
MSFT.1yr.ts <- ts(MSFT.1yr$close, frequency = 252) 
plot(MSFT.1yr.ts, xaxt = "n", main = "MSFT Time Series")
axis(1, at=c(1,2), labels=c("11/30/2022","11/30/2023"))


#subset to make 100 day
MSFT.100day <- subset(MSFT, date >= "2023-7-12" & date < "2023-12-01")
#252 trading days a year
MSFT.100day.ts <- ts(MSFT.100day$close, frequency = 252)
plot(MSFT.100day.ts)
plot(MSFT.100day.ts, xaxt = "n", main = "MSFT Time Series")
axis(1, at=c(1,1.2,1.4), labels=c("7/12/2023", "9/20/2023", "11/30/2023"))

options(scipien=999)
MSFT.lm <- tslm(MSFT.ts ~ trend + season)
summary(MSFT.lm)


#----------------
#AMZN
library(forecast)
AMZN <- read.csv("AMZN.csv")
AMZN$date <- as.Date(AMZN$date, format="%m/%d/%Y")
AMZN <- subset(AMZN, date >= "2018-01-01")


#252 trading days a year
AMZN.ts <- ts(AMZN$close, frequency = 252) 
plot(AMZN.ts)

options(scipien=999)
AMZN.lm <- tslm(AMZN.ts ~ trend + season)
summary(AMZN.lm)


#NVDA
library(forecast)
NVDA <- read.csv("NVDA.csv")
NVDA$date <- as.Date(NVDA$date, format="%m/%d/%Y")
NVDA <- subset(NVDA, date >= "2018-01-01")

#252 trading days a year
NVDA.ts <- ts(NVDA$close, frequency = 252) 
plot(NVDA.ts)

options(scipien=999)
NVDA.lm <- tslm(NVDA.ts ~ trend + season)
summary(NVDA.lm)

