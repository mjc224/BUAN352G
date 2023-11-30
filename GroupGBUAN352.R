#BUAN352
#Group G


#-----------
#MSFT
library(forecast)
str(MSFT)

#252 trading days a year
MSFT.ts <- ts(MSFT$close, frequency = 252) 
plot(MSFT.ts)

options(scipien=999)
train.lm <- tslm(MSFT.ts ~ trend + season)
summary(train.lm)


#----------------
#AMZN
library(forecast)
AMZN <- read.csv("AMZN.csv")
str(AMZN)

#252 trading days a year
AMZN.ts <- ts(AMZN$close, frequency = 252) 
plot(AMZN.ts)

options(scipien=999)
AMZN.lm <- tslm(AMZN.ts ~ trend + season)
summary(AMZN.lm)


#NVDA
library(forecast)
NVDA <- read.csv("NVDA.csv")
str(NVDA)

#252 trading days a year
NVDA.ts <- ts(NVDA$close, frequency = 252) 
plot(NVDA.ts)

options(scipien=999)
NVDA.lm <- tslm(NVDA.ts ~ trend + season)
summary(NVDA.lm)

