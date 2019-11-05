library(psych)
library(ggplot2)

RawData <- read.delim("C:/users/KimYG/Desktop/Rp example3.txt", sep ='\t', header = T)

colnames(RawData) <- c("Pupil X", "CR X", "Pupil Diam", "Table position", "Drum position", "Camera Arm Position", "Pupil Y", "CR Y")

SamplingRate <- 100
ScalingFactor <- 0.018181818
Freq <- 0.5

PminusCR <- RawData$`Pupil X` - RawData$`CR X`
RawData <- cbind(RawData, PminusCR)

Rp <- c()
MeanPdim <- c()

for (i in 1:30) {
  SubsetData <- RawData[100*(1/Freq)*(i-1)+1:(100*(1/Freq)*(i-1)+200),]
  LeftSide <- ((SubsetData$PminusCR[c(45:55)])*ScalingFactor)
  RightSide <-((SubsetData$PminusCR[c(145:155)])*ScalingFactor)
  LeftSideDia <- ((SubsetData$`Pupil Diam`[c(45:55)])*ScalingFactor)
  RightSideDia <-((SubsetData$`Pupil Diam`[c(145:155)])*ScalingFactor)
  Rp[i] <- (describe(LeftSide)$mean - describe(RightSide)$mean)/(20*pi/180)
  MeanPdim[i] <- (describe(RightSideDia)$mean + describe(LeftSideDia)$mean)/2
}

Result <- data.frame(Rp, MeanPdim)

linearMod <- lm(Rp~MeanPdim, data=Result)
linearModX <- seq(from = min(Result$MeanPdim), to = max(Result$MeanPdim), length.out = 100)
linearModY <- c()

for (i in 1:100) {
  linearModY[i] <- linearMod$coefficients[1] + linearMod$coefficients[2]*linearModX[i]
  i <- i+1
}
linearModXY <- data.frame(linearModX, linearModY)

ScatterPlot <- ggplot(Result, aes(x=MeanPdim, y = Rp)) + 
                      geom_point(size=2) + 
                      geom_text(label = rownames(Result), vjust = 0, nudge_y = 0.007) +
                      ylim(0.1,1.5)

CombinedPlot <- ScatterPlot + geom_line(data=linearModXY, aes(x=linearModX, y=linearModY), linetype=2, size = 1.5, color ='red')
                            
print(CombinedPlot)
print(linearMod)

dfbeta(linearMod) ## check absence of influenstial data points
