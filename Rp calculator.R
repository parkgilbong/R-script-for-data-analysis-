library(psych)
library(ggplot2)

RawData <- read.delim("C:/users/KimYG/Desktop/Rp example1.txt", sep ='\t', header = T)

colnames(RawData) <- c("Pupil X", "CR X", "Pupil Diam", "Table position", "Drum position", "Camera Arm Position", "Pupil Y", "CR Y")

SamplingRate <- 100

ScalingFactor <- 0.018181818

Freq <- 0.5

PminusCR <- RawData$`Pupil X` - RawData$`CR X`

RawData <- cbind(RawData, PminusCR)

RightRp <- c()
LeftRp <- c()
RightPdiam <- c()
LeftPdiam <- c()
Rp <- c()
MeanPdim <- c()

for (i in 1:30) {
  SubsetData <- RawData[100*(1/Freq)*(i-1)+1:(100*(1/Freq)*(i-1)+200),]
  LeftSide <- (subset(SubsetData, c(9, 3), subset = (SubsetData[,6] > 30000))*ScalingFactor)
  RightSide <- (subset(SubsetData, c(9, 3), subset = (SubsetData[,6] < -30000))*ScalingFactor)
  Rp[i] <- (describe(LeftSide)$mean - describe(RightSide)$mean)[1]/(20*pi/180)
  MeanPdim[i] <- (describe(RightSide)$mean + describe(LeftSide)$mean)[2]/2
  LeftRp[i] <- (describe(LeftSide)$mean)[1] 
  RightRp[i] <- (describe(RightSide)$mean)[1] 
  LeftPdiam[i] <- (describe(LeftSide)$mean)[2]
  RightPdiam[i] <- (describe(RightSide)$mean)[2] 
}

Result <- data.frame(Rp, MeanPdim)
Result2 <- data.frame(LeftRp, RightRp, LeftPdiam, RightPdiam)

ggplot(Result, aes(x=MeanPdim, y = Rp)) + 
  geom_point(size=2) + 
  geom_text(label = rownames(Result), vjust = 0, nudge_y = 0.007) +
  ylim(0.5,1.0)
