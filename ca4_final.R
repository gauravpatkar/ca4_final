getwd()

#installing the required packages

install.packages("ggplot2")
install.packages("forecast")
install.packages("astsa")
install.packages("lmtest")
install.packages("fUnitRoots")
install.packages("FitARMA")
install.packages("strucchange")
install.packages("reshape")
install.packages("Rmisc")
install.packages("fBasics")

#running the library 
library(ggplot2)
library(forecast)
library(astsa)
library(lmtest)
library(fUnitRoots)
library(FitARMA)
library(strucchange)
library(reshape)
library(Rmisc)
library(fBasics)



#reading are dataset 
prediction <-read.csv("all_new.csv", header = TRUE)
prediction1 <- prediction[c(-1)]
prediction1


install.packages("reshape")
library("reshape")

#using melt fuction to assign are data properly  
df <- melt(prediction1, id = c("year"))
head(df)
View(df)


df <- melt(prediction1, id = c("year"))
tail(df)

#using ggplot to plot between value and variable data
install.packages("ggplot2")
library(ggplot2)
ggplot(data = df, aes(x = year)) + geom_line(aes(y = value, colour = variable)) +
  scale_colour_manual(values = c("blue", "red"))

#calculate sample test to check p value
t.test(value ~ variable, data = df)


library(fBasics)
basicStats(prediction1[-1])

#plotting boxplot and density plot to find mean difference
p1 <- ggplot(data = df, aes(x = variable, y = value)) + geom_boxplot()
p2 <- ggplot(data = prediction1, aes(Rainfall_readings)) + geom_density()
p3 <- ggplot(data = prediction1, aes(transport_emission)) + geom_density()
multiplot(p1, p2, p3, cols = 3)

#using autoplot to see the time series keeping the frequency as 1
excess_frac <- (prediction1$Rainfall_readings - prediction1$transport_emission)/prediction1$transport_emission
excess_ts <- ts(excess_frac, frequency = 1, start = prediction1$year[1])
autoplot(excess_ts)


basicStats(excess_frac)


#ploting acf and pacf
par(mfrow=c(1,2))
acf(excess_ts)
pacf(excess_ts)

summary(lm(excess_ts ~ 1))


#to check the intersection between two variable
(break_point <- breakpoints(excess_ts ~ 1))


plot(break_point)


summary(break_point)

plot(excess_ts)
lines(fitted(break_point, breaks = 1), col = 4)
lines(confint(break_point, breaks = 1))

fitted(break_point)[1]

fitted(break_point)[length(excess_ts)]

break_date <- breakdates(break_point)
win_1 <- window(excess_ts, end = break_date)
win_2 <- window(excess_ts, start = break_date + 1)
t.test(win_1, win_2)


#model1
(model_1 <- auto.arima(excess_ts, stepwise = FALSE, trace = TRUE))


summary(model_1)


coeftest(model_1)

#model2

model_2 <- Arima(excess_ts, order = c(1,0,0), 
                 seasonal = list(order = c(0,0,1), period = 10), 
                 include.mean = TRUE)
summary(model_2)


coeftest(model_2)

#model3

model_3 <- Arima(excess_ts, order = c(1,0,0), 
                 seasonal = list(order = c(1,0,0), period = 10), 
                 include.mean = TRUE)
summary(model_3)


coeftest(model_3)

#model4

level <- c(rep(0, break_point$breakpoints), 
           rep(1, length(excess_ts) - break_point$breakpoints))

model_4 <- Arima(excess_ts, order = c(0,0,0), 
                 seasonal = list(order = c(0,0,1), period = 10), 
                 xreg = level, include.mean = TRUE)
summary(model_4)


coeftest(model_4)


#model5

model_5 <- Arima(excess_ts, order = c(1,0,0), 
                 seasonal = list(order = c(0,0,1), period=10), 
                 xreg = level, include.mean = TRUE)
summary(model_5)

coeftest(model_5)

#model6
model_6 <- Arima(excess_ts, order = c(1,0,0), xreg = level, include.mean = TRUE)
summary(model_6)

coeftest(model_6)


#Model #1 Residuals Diagnostic

checkresiduals(model_1)

LjungBoxTest(residuals(model_1), k = 2, lag.max = 20)

sarima(excess_ts, p = 1, d = 1, q = 1)

#Model #2 Residuals Diagnostic

checkresiduals(model_2)

LjungBoxTest(residuals(model_2), k = 2, lag.max = 20)


sarima(excess_ts, p = 1, d = 0, q = 0, P = 0, D = 0, Q = 1, S = 10)

#Model #3 Residuals Diagnostic

checkresiduals(model_3)

LjungBoxTest(residuals(model_3), k = 2, lag.max = 20)

sarima(excess_ts, p = 1, d = 0, q = 0, P = 1, D = 0, Q = 0, S = 10)

#Model #4 Residuals Diagnostic

checkresiduals(model_4)

LjungBoxTest(residuals(model_4), k = 1, lag.max = 20)

sarima(excess_ts, p = 0, d = 0, q = 0, P = 0, D = 0, Q = 1, S = 10, xreg = level)

#Model #6 Residuals Diagnostic

checkresiduals(model_6)

LjungBoxTest(residuals(model_6), k = 1, lag.max = 20)

sarima(excess_ts, p = 1, d = 0, q = 0, xreg = level)



#forecast

h_fut <- 5
plot(forecast(model_1, h = h_fut, xreg = rep(1, h_fut)))


prediction1.ts <- ts(prediction1$Rainfall_readings + prediction1$transport_emission, frequency = 1 , 
                     start = prediction1$year[1])
autoplot(prediction1.ts)


summary(lm(prediction1.ts ~ 1))

(break_point <- breakpoints(prediction1.ts ~ 1))

plot(break_point)

summary(break_point)


plot(prediction1.ts)
fitted.ts <- fitted(break_point, breaks = 3)
lines(fitted.ts, col = 4)
lines(confint(break_point, breaks = 3))


unique(as.integer(fitted.ts))

breakdates(break_point, breaks = 3)

fitted.ts <- fitted(break_point, breaks = 3)
autoplot(fitted.ts)


prediction1_xreg <- Arima(prediction1.ts, order = c(0,1,1), xreg = fitted.ts, include.mean = TRUE)
summary(prediction1_xreg)

coeftest(prediction1_xreg)

checkresiduals(prediction1_xreg)

LjungBoxTest(residuals(prediction1_xreg), k=1, lag.max=20)


sarima(prediction1.ts, p=3, d=1, q=1, xreg = fitted.ts)









###########################################################################


