setwd("E:\\Harddrive\\OneDrive - Lund University\\Mastern\\Spring 2020\\NEKN34 Time Series Analysis\\Assignments\\Ass 3\\Assignment-3-TS")
getwd()

install.packages("")
library(vars)
library(urca)
library(tseries)
library(tsDyn)
library(lmtest)
library(car)
library(data.table) #used for shifting, aka lagging
library(dynlm)
library(readxl)
library(ggplot2)


data = read_excel("MoneyIncome.xlsx")

#tests for stationarity----
plot(data$t, data$ip)
plot(data$t, data$m1)
adf.test(data$ip)
adf.test(data$m1)
summary(data)


plot(diff(data$ip))
plot(diff(data$m1))
adf.test(diff(data$ip))
adf.test(diff(data$m1))


coeff <- 10

ggplot(data, aes(x =t))+
  geom_line(aes(y = ip))


#make them identifiable
ggplot(data, aes(x=t)) +
  geom_line( aes(y=ip, colour = "Industrial Production")) + 
  geom_line( aes(y=m1 / coeff, colour = "Money/10")) + 
  scale_y_continuous(
    name = "Money/10",
    sec.axis = sec_axis(~.*coeff, name="Industrial Production")) +
  theme(legend.position= c(0.5, 0.9)) +
  xlab("Date")
  


#testing for cointegration----
VARselect(data[,2:3], lag.max = 10, type = "const", season = 12)$select #displays a bunch of information criterion values, 10 seems to be a good number of lags
cointest = ca.jo(data[,2:3], type = "trace", ecdet = "const", season = 12)
summary(cointest) #from this we can see that the test-statistic is much greater than the critical values, at the 5% level, we have two cointegrating variables

vecm = cajorls(cointest, r = 1)
vecm$rlm #really strange because it shows that the "error correction term" are almost zero. ("error correction term" = "speed of adjustment" in our notes)
vecm$beta

#split the data into pre, post and during the 80s as per the literature
datapre80 = data[1:252,]
datapost80 = data[373:length(data),]
data80 = data[253:length(data),]

#full dataset
vecm_full = VECM(data[,2:3], lag = 10, r = 1, estim = "2OLS")
summary(vecm_full) # the interpretation of the first value of "m1 -3" is that m1 from 3 periods ago affects current ip by -0.0043

#pre 80s dataset
vecm_pre = VECM(datapre80[,2:3], lag = 10, r = 1, estim = "2OLS")
summary(vecm_pre) 

#post 80s dataset
vecm_post = VECM(datapost80[,2:3], lag = 10, r = 1, estim = "2OLS")
summary(vecm_post)

#during 80s dataset
vecm_80 = VECM(data80[,2:3], lag = 10, r = 1, estim = "2OLS")
summary(vecm_80) 

# how VECM interprets significance codes: ** = 0.001    * = 0.01    . = 0.05
#VAR-----
Var_full = VAR(diff(ts(data[,2:3])), p =10 , type = "const", season = 12)
summary(Var_full)
causality(Var_full, cause = "m1") #the one of interest, m1 DOES cause ip
causality(Var_full, cause = "ip") #aux causality, ip causes m1

Var_pre = VAR(diff(ts(datapre80[,2:3])), p =10 , type = "const", season = 12)
summary(Var_pre)
causality(Var_pre, cause = "m1") # m1 does not cause ip
causality(Var_pre, cause = "ip")

Var_post = VAR(diff(ts(datapost80[,2:3])), p =10 , type = "const", season = 12)
summary(Var_post)
causality(Var_post, cause = "m1")  # m1 does not cause ip
causality(Var_post, cause = "ip")

Var_80 = VAR(diff(ts(data80[,2:3])), p =10 , type = "const", season = 12)
summary(Var_80)
causality(Var_80, cause = "m1")  # m1 DOES cause ip
causality(Var_80, cause = "ip")

#previous stuff & possible trash-----
data[,2:3]
po.test(data[,2:3])
tsdata = ts(data)
VectorError = VECM(tsdata[,2:3], lag = 1, r = 1, include = "const") #VECM MUST have time series
summary(VectorError)

dl = dynlm(ip ~ m1 + m1lag1, data = tsdata)
plot(dl$residuals)
###-----
#testing if VAR might be any good, forgot to take first difference on the linear regression parts :(


ddata = data.frame(diff(data$ip), diff(data$m1), data$t[1:731]) #creates differenced data and adds the time 
plot(ddata$diff.data.ip.)
plot(ddata$diff.data.m1.)

test = data
test$m1lag1 = shift(test$m1)
test$m1lag2 = shift(test$m1lag1)
test$iplag1 = shift(test$ip)
test$iplag2 = shift(test$iplag1)


dl = dynlm(ip ~ m1 + m1lag1 + m1lag2 + iplag1 + iplag2, data = test)
summary(dl)

grangertest(data$ip, data$m1, 2)
linearHypothesis(dl,"m1lag1 - m1lag2" )
naive = lm(ip ~ m1, data = data)
plot(naive$residuals)
print(data[,2:3])

var1 = VAR(data[,2:3],p = 2, type = "const")
print(var1)
summary(var1)  



#TRASH TRASH TRASH TRASH--------
datapre80 = data[1:252,]
VARselect(datapre80[,2:3], lag.max = 12, type = "const", season = 12)$select #displays a bunch of information criterion values, 10 seems to be a good number 
cointestpre80 = ca.jo(datapre80[,2:3], type = "trace", ecdet = "const", season = 12)
cointestpre80@cval
cointestpre80@teststat[1] #H0: r = 0 is not rejected at % level, critical value is 19.96

#post 1980s
datapost80 = data[373:length(data),]
VARselect(datapost80[,2:3], lag.max = 12, type = "const", season = 12)$select #displays a bunch of information criterion values, 10 seems to be a good number 
cointestpost80 = ca.jo(datapost80[,2:3], type = "trace", ecdet = "const", season = 12)
cointestpost80@cval
cointestpost80@teststat[1] #H0: r = 0 is not rejected at 5% level, critical value is 19.96
