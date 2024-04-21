okl=read.csv('C:/Users/harsh/Downloads/oklahama/Oklahoma/OKL_FINAL.csv')
sd(okl$CO)
library(tseries)
?ts
okl=as.data.frame(scale(okl[3 : 8])) 
View(okl)
okl


View(okl)

install.packages("portes")
library(portes)
okl
?Hosking
as.matrix(okl)
Hosking(as.matrix(okl),lag=1)

co
co=ts(okl$CO,start=c(2022,40),frequency=52)
ozn=ts(okl$Ozone,start=c(2022,40),frequency=52)
no2=ts(okl$NO2,start=c(2022,40),frequency=52)
so2=ts(okl$SO2,start=c(2022,40),frequency=52)
pm2.5=ts(okl$PM.2.5,start=c(2022,40),frequency=52)
pm10=ts(okl$PM.10,start=c(2022,40),frequency=52)

plot(pm10)
plot()

adf.test(ozn)#0.07168
kpss.test(ozn)
plot(diff(ozn,lag=1))
adf.test(diff(ozn,lag=1))


adf.test(no2)#0.7065
kpss.test(no2)
plot(no2)
adf.test(diff(no2,lag=1))

adf.test(so2)#0.6656
kpss.test(so2)


adf.test(pm2.5)#0.6656
kpss.test(pm2.5)

adf.test(pm10)#0.6656
kpss.test(pm10)

co=diff(co,lag=1)
ozn=diff(ozn,lag=1)
no2=diff(no2,lag=1)
so2=diff(so2,lag=1)
pm2.5=diff(pm2.5,lag=1)
pm10=diff(pm10,lag=1)


co
library(vars)
install.packages('vars')
mean(okl$CO)

co
data=cbind(co,ozn,no2,so2,pm2.5,pm10)
data
View(data)
?VARselect
ncol(data)
var=VARselect(data,type='none')
summary(var)
var
format(var$criteria,scientific = FALSE)


estim=VAR(data,p=1,type='none')
estim
summary(estim)

estim$varresult$co$re
estim$obs
#Residuals of VAR model p=1
co_er=estim$varresult$co$residuals
ozn_er=estim$varresult$ozn$residuals
no2_er=estim$varresult$no2$residuals
so2_er=estim$varresult$so2$residuals
pm2.5_er=estim$varresult$pm2.5$residuals
pm10_er=estim$varresult$pm10$residuals

plt_er=data.frame(co_er,ozn_er,no2_er,so2_er,pm2.5_er,pm10_er)
Hosking(as.matrix(plt_er))

cor(plt_er)
cor(okl)
adf.test(co_er)

#error is not white noise
acf(co_er)

?prcomp
pc=prcomp(plt_er,scale=T,center=T)
attributes(pc)
summary(pc)
pcs=pc$x[,1:3]
pcs
pcs
y=okl$target[-c(1,2)]
fin_data=cbind(pcs,y)
fin_data=as.data.frame(fin_data)

pois_model=glm(y~.,data=fin_data,family = poisson(link = "log"))
summary(pois_model)
pois_model$aic
pois_model$aic

gamma_model=glm(y~.,data=fin_data,family = "Gamma")
summary(gamma_model)
gamma_model$aic

mean(y)
var(y)

cbind(pois_model$fitted.values,y)

plot(density(y))
hist(y)

var(y)

library(MASS)
summary(m1 <- glm.nb(y ~., data = fin_data))
m1$aic
plot(cbind(m1$fitted.values,y))

#GAM
library(mgcv)
gam=gam(y~ti(PC1)+s(PC2)+t2(PC3),data=fin_data)
summary(gam)
cbind(gam$fitted.values,y)
gam$aic

?s
