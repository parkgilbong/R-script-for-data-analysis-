library(readABF) #load 'readABF' library

wholefile <- readABF(file.choose()) # imprt ABF file 

Sweep01 <- as.data.frame(wholefile, sweep = 1) # make dataframe for Sweep #1

plot(Sweep01$`Time [s]`, Sweep01$`IN 0 [pA]`, type = "l", xlab = "Time [sec]", ylab = "I [pA]")

RsY <- Sweep01$`IN 0 [pA]`[Sweep01$`Time [s]` > 0.30 & Sweep01$`Time [s]` < 0.45] # extrat data in 0.3~0.45 sec from the dataframe 
RsX <- Sweep01$`Time [s]` [Sweep01$`Time [s]` > 0.30 & Sweep01$`Time [s]` < 0.45]

plot(RsX, RsY, type = "l", xlab = "Time [sec]", ylab = " I [pA]")