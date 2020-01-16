library(tidyverse)
library(MASS)
library(forecast)
library(fpp)
library(corrplot)
library(magrittr)
library(zoo)
library(RColorBrewer)
library(gridExtra)
library(Amelia)

setwd("~/Northwestern University/Spring 2018 Quarter/R Studio CSV Files/MSDS 413 Final Project")
trainf <- read.csv('DengAI_Training_Data_Features.csv')
trainl <- read.csv('DengAI_Training_Data_Labels.csv')
testf <- read.csv('DengAI_Test_Data_Features.csv')
subform <- read.csv('DengAI_Submission_Format.csv')

sjtrainf = trainf %>% filter(city == 'sj')
sjtrainl = trainl %>% filter(city == 'sj')
cat('\nSan Juan\n',
    '\t features: ', sjtrainf %>% ncol,
    '\t entries: ' , sjtrainf %>% nrow,
    '\t labels: '  , sjtrainl %>% nrow)
iqtrainf = trainf %>% filter(city == 'iq')
iqtrainl = trainl %>% filter(city == 'iq')
cat('\nIquitos\n',
    '\t features: ', iqtrainf %>% ncol,
    '\t entries: ' , iqtrainf %>% nrow,
    '\t labels: '  , iqtrainl %>% nrow)

sjtrainf %<>% dplyr::select(-week_start_date)
iqtrainf %<>% dplyr::select(-week_start_date)
apply(sjtrainf, 2, function(x) 
  round(100 * (length(which(is.na(x))))/length(x) , digits = 1)) %>%
  as.data.frame() %>%
  `names<-`('Percent of Missing Values')
missmap(sjtrainf)
apply(iqtrainf, 2, function(x) 
  round(100 * (length(which(is.na(x))))/length(x) , digits = 1)) %>%
  as.data.frame() %>%
  `names<-`('Percent of Missing Values')
missmap(iqtrainf)

sjtrainf$ndvi_ne %<>% na.locf(fromLast = TRUE)
iqtrainf$ndvi_ne %<>% na.locf(fromLast = TRUE)

sjtrainf %<>% mutate('total_cases' = sjtrainl$total_cases)
iqtrainf %<>% mutate('total_cases' = iqtrainl$total_cases)

sjtrainf %>% 
  dplyr::select(-city, -year, -weekofyear) %>%
  cor(use = 'pairwise.complete.obs') -> corr1

iqtrainf %>% 
  dplyr::select(-city, -year, -weekofyear) %>%
  cor(use = 'pairwise.complete.obs') -> corr2

sort(corr1[21,-21]) %>%  
  as.data.frame %>% 
  `names<-`('correlation') %>%
  ggplot(aes(x = reorder(row.names(.), -correlation), y = correlation, fill = correlation)) + 
  geom_bar(stat='identity', colour = 'black') + scale_fill_continuous(guide = FALSE) + scale_y_continuous(limits =  c(-.15,.25)) +
  labs(title = 'San Juan\n Correlations', x = NULL, y = NULL) + coord_flip() -> sjcorr

sort(corr2[21,-21]) %>%  
  as.data.frame %>% 
  `names<-`('correlation') %>%
  ggplot(aes(x = reorder(row.names(.), -correlation), y = correlation, fill = correlation)) + 
  geom_bar(stat='identity', colour = 'black') + scale_fill_continuous(guide = FALSE) + scale_y_continuous(limits =  c(-.15,.25)) +
  labs(title = 'Iquitos\n Correlations', x = NULL, y = NULL) + coord_flip() -> iqcorr

grid.arrange(sjcorr, iqcorr, nrow = 1)

sjts<-ts(sjtrainf$total_cases, frequency=52, start=c(1990, 18))
plot(sjts, main = "San Juan Time Series Plot")

iqts<-ts(iqtrainf$total_cases, frequency=52, start=c(2000, 26))
plot(iqts, main = "Iquitos Time Series Plot")

sjtstrain <- window(sjts, end=c(2004,37))
sjtstest <- window(sjts,start=c(2004,38))

iqtstrain <- window(iqts, end=c(2008,27))
iqtstest <- window(iqts,start=c(2008,28))

sjnnetfit <- nnetar(sjtstrain)
sjnnetfit

iqnnetfit <- nnetar(iqtstrain)
iqnnetfit

sjnnetfc <- forecast(sjnnetfit,h=260)
plot(sjnnetfc) 
lines(sjtstest, col="red")  
legend("topright",lty=1,col=c("red","blue"),c("actual values","forecast"))

iqnnetfc <- forecast(iqnnetfit,h=156)
plot(iqnnetfc) 
lines(iqtstest, col="red")  
legend("topright",lty=1,col=c("red","blue"),c("actual values","forecast"))

sjnnetacc <- accuracy(sjnnetfc, sjtstest)
sjnnetacc

iqnnetacc <- accuracy(iqnnetfc, iqtstest)
iqnnetacc

sjnnetsol=data.frame(subform[1:260, -4],total_cases=round(sjnnetfc$mean))
iqnnetsol=data.frame(subform[261:416,-4],total_cases=round(iqnnetfc$mean))

DengAI_NNET_Solution=bind_rows(sjnnetsol,iqnnetsol)
write.csv(DengAI_NNET_Solution,file="DengAI_NNET_Predictions.csv",row.names=F)

sjarimafit <- auto.arima(sjtstrain)
sjarimafit

sjarimafc=forecast(sjarimafit,h=260)
plot(sjarimafc)

sjarimaacc = accuracy(sjarimafc,sjtstest)
sjarimaacc

iqarimafit <- auto.arima(iqtstrain)
iqarimafit

iqarimafc=forecast(iqarimafit,h=156)
plot(iqarimafc)

iqarimaacc = accuracy(iqarimafc,iqtstest)
iqarimaacc

sjarimasol=data.frame(subform[1:260,-4],total_cases=round(sjarimafc$mean))
iqarimasol=data.frame(subform[261:416,-4],total_cases=round(iqarimafc$mean))

DengAI_ARIMA_Solution=bind_rows(sjarimasol,iqarimasol)
write.csv(DengAI_ARIMA_Solution,file="DengAI_ARIMA_Predictions.csv",row.names=F)

sjetsfit <- ets(sjtstrain)
sjetsfit

sjetsfc=forecast(sjetsfit,h=260)
plot(sjetsfc)

sjetsacc = accuracy(sjetsfc,sjtstest)
sjetsacc

iqetsfit <- ets(iqtstrain)
iqetsfit

iqetsfc=forecast(iqetsfit,h=156)
plot(iqetsfc)

iqetsacc = accuracy(iqetsfc,iqtstest)
iqetsacc

sjetssol=data.frame(subform[1:260,-4],total_cases=round(sjetsfc$mean))
iqetssol=data.frame(subform[261:416,-4],total_cases=round(iqetsfc$mean))

DengAI_ETS_Solution=bind_rows(sjetssol,iqetssol)
write.csv(DengAI_ETS_Solution,file="DengAI_ETS_Predictions.csv",row.names=F)
