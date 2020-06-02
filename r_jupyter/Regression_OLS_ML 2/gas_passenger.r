rm(list=ls())
setwd("c:/usr/doc/dropbox/daigakuin/")

dt <- read.csv("gas_10k.csv",header=T)

str(dt)
summary(dt)

## 1 year: survey year
## 2 month: survey month
## 3 mileage: [km/L]
## 4 daily_km: vehicle km_per day during survey
## 5 hybrid: hybrid dummy variable
## 6 displace_cc:engine displacement [cc]
## 7 weight_kg: vehicle weight [kg]
## 8 age_month: age of vehicle [month]
## 9 temp_ave: average temperature of registered place [Celsius degree]
##10 gasprice: gasoline price on surveyed month & place [yen/L]
##11 pop_density: population density at surveyed place [persons/km^2]

dt[,6] <- dt[,6]/1000  ## displacement unit to "litter"
dt[,7] <- dt[,7]/1000  ## weight unit to "ton"

x <- dt[,3:11]

par(ask=T)

pairs(x)

hist(x$mileage)
plot(density(x$mileage))
boxplot(x$mileage)
boxplot(x$mileage~x$hybrid)

plot(x$weight_kg,x$mileage)
round(cor(x),digits=3)

res1 <- lm(x$mileage~.,data=x)
summary(res1)

res2 <- lm(x$mileage~(x$daily_km + x$hybrid + x$displace_cc + x$weight_kg +
                      x$age_month + x$temp_ave + x$gasprice + x$pop_density)^2)
summary(res2)

res3 <- lm(x$mileage~ x$daily_km + x$hybrid + x$weight_kg + x$age_month + x$temp_ave + 
                      x$pop_density + x$daily_km*x$displace_cc + x$daily_km*x$weight_kg+
                      x$daily_km*x$age_month + x$daily_km*x$pop_density + x$hybrid*x$weight_kg +
                      x$hybrid*x$age_month + x$hybrid*x$temp_ave + x$weight_kg*x$pop_density +
                      x$temp_ave*x$pop_density)
summary(res3)

x <- cbind(x,x$temp_ave^2)
res4 <- lm(x$mileage~.,data=x)
summary(res4)


