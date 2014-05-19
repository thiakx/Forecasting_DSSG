#code adapted from Forecasting: principles and practice https://www.otexts.org/fpp
#setwd("yourDir")

#install.packages("fpp")
library(fpp)

#define data
data<-elecequip
trainData<-window(elecequip,1996,c(2010,12))
testData<-window(elecequip,2011,c(2011,11))

#stl
stlMod <- stl(trainData,  s.window="periodic", robust=TRUE)
stlModf <- forecast(stlMod, method="naive",h=11)
plot(stlMod)
plot(stlModf, ylab="New orders index")
lines(testData, col="red")
legend("topleft",lty=1,bty = "n",col=c("red","blue"),c("testData","STLPred"))

#naive plot
stlModAdj <- seasadj(stlMod)
plot(naive(stlModAdj), xlab="New orders index",
     main="Naive forecasts of seasonally adjusted data")

#ses
sesMod <- ses(stlModAdj, alpha=0.2, initial="simple", h=11)
plot(sesMod)

#holt model
holtMod <- holt(stlModAdj, alpha=0.8, beta=0.2, initial="simple", h=7) 
holtModDamped <- holt(stlModAdj, alpha=0.8, beta=0.2, initial="simple", h=7,damped=TRUE) 
plot(holtMod)
plot(holtModDamped)

#holt-winter
holtWinMod <- hw(trainData,seasonal="multiplicative",
                 damped=TRUE, initial="optimal",h=11)
plot(holtWinMod)
lines(testData, col="red")
legend("topleft",lty=1,bty = "n",col=c("red","blue"),c("testData","holtWinPred"))

#auto arima
# #faster method, less accurate
# arimaModFast<-auto.arima(trainData)
# plot(forecast(arimaModFast))

#takes a while to compute
arimaMod<-auto.arima(trainData, stepwise=FALSE, approximation=FALSE)
arimaModF<-forecast(arimaMod,h=11)
plot(arimaModF)
lines(testData, col="red")
legend("topleft",lty=1,bty = "n",col=c("red","blue"),c("testData","ARIMAPred"))

#compare accuracy of hotwin vs ARIMA
plot(testData)
lines(stlModf$mean, col="blue")
lines(holtWinMod$mean, col="red")
lines(arimaModF$mean, col="green")
legend("bottomleft",lty=1,cex=1,y.intersp=0.4,bty = "n",col=c("black","blue","red","green"),c("data","STLPred","holtWinPred","arimaModPred"))

accuracy(stlModf,testData)[2,"RMSE"]
accuracy(holtWinMod,testData)[2,"RMSE"]
accuracy(arimaModF,testData)[2,"RMSE"]

#test residues of arima
tsdisplay(residuals(arimaMod))
#p-value large, means residuals are not distinguishable from a white noise series.
Box.test(residuals(arimaMod), lag=36, fitdf=6, type="Ljung")