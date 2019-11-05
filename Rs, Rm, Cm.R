library(readABF) #load 'readABF' library

wholefile <- readABF(file.choose()) # imprt ABF file 

Sweep01 <- as.data.frame(wholefile, sweep = 1) # make dataframe for Sweep #1

plot(Sweep01$`Time [s]`, Sweep01$`IN 0 [pA]`, type = "l", xlab = "Time [sec]", ylab = "I [pA]")

RsY <- Sweep01$`IN 0 [pA]`[Sweep01$`Time [s]` > 0.30 & Sweep01$`Time [s]` < 0.45] # extrat data in 0.3~0.45 sec from the dataframe 
RsX <- Sweep01$`Time [s]` [Sweep01$`Time [s]` > 0.30 & Sweep01$`Time [s]` < 0.45]

plot(RsX, RsY, type = "l", xlab = "Time [sec]", ylab = " I [pA]")

Baseline <- mean(RsY[RsX>= 0.3 & RsX <= 0.31])

NegativePeakPoint <- min(RsY)
PositivePeakPoint <- max(RsY)

NegativePeak <- NegativePeakPoint - Baseline 
PositivePeak <- PositivePeakPoint - Baseline

Isteadystate <- mean(RsY[RsX>= 0.4 & RsX <= 0.41]) # calculate averaged I at steadystate
deltaI <- Baseline - Isteadystate
deltaV <- 0.005

NegativePeakinAmpare <- NegativePeak * 1E-12
IsteadystateinAmpare <- Isteadystate * 1E-12

SeriesResistance <- deltaV / NegativePeakinAmpare
RsMOhom <- abs(SeriesResistance / 1000000)

Indexmin <- which(RsY == min(RsY))
ExpoRsY <- RsY[Indexmin:(Indexmin+900)]
ExpoRsX <- seq(length(ExpoRsX))
ExpoXY <- data.frame(ExpoRsX, ExpoRsY)

plot(ExpoRsX, ExpoRsY, pch = 2.0, type = "p")

# Prepare a good inital state
theta.0 <- max(ExpoXY$ExpoRsY) * 1.1
model.0 <- lm(log(- ExpoRsY + theta.0) ~ ExposRsX, data=ExpoXY)
alpha.0 <- -exp(coef(model.0)[1])
beta.0 <- coef(model.0)[2]

start <- list(alpha = alpha.0, beta = beta.0, theta = theta.0)

# Fit the model
model <- nls(ExpoRsY ~ alpha * exp(beta * ExpoRsX) + theta , data = ExpoXY, start = start)
summary(model)
# add fitted curve
plot(ExpoXY$ExpoRsX, ExpoXY$ExpoRsY, pch = 1, cex = 0.5)
lines(ExpoXY$ExpoRsX, predict(model, list(x = ExpoXY$ExpoRsX)), col = 'red', lwd = 3)



