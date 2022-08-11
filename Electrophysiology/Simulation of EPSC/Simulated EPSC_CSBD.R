A <- -0.000007 # -3834.927 nS
Tau1 <- 0.0005 # 0.5 ms
Tau2 <- 0.002 # 2 ms
SamplingRate <- 10000
Duration <- 0.01

#randomfactor1 <- rnorm(500, mean=1, sd=0.3)
#RandomFactor <- rexp(500, rate=1)
while (any(duplicated(NOEinSegment))) {NOEinSegment <- round(runif(20, min = 0, max = 100))
                                      } # The number of events for a 500ms-long segment is drawn from uniform distribution. To avoid being any duplicated element in the vector,  
TOEinSegment <- list()
for (i in 1:length(NOEinSegment)) {TOEinSegment[[as.character(NOEinSegment[i])]] <- sort(runif(NOEinSegment[i], min = 0, max = 0.5))} #The Timepoint of events in a 500ms-long segment is drawn from uniform distribution. 

#simulatedEPSC <- list()

oneEvent <- generateEPSC(A, 
                        Tau1, 
                        Tau2, 
                        SamplingRate, 
                        Duration, 
                        1)
plot((1:length(oneEvent)/SamplingRate), oneEvent, type = "l")    
#stackedEPSC <- as.data.frame(do.call(rbind, simulatedEPSC)

plot(as.vector(unlist(stackedEPSC[1,])), type = 'l', xlab = "Time(sec)")

generateEPSC <- function(A, Tau1, Tau2, SamplingRate, Duration, RandomFactor) {  
    A_prime <- (Tau2^(Tau1/(Tau1-Tau2)))/Tau1
    
    t <- seq(from = 0, length.out = SamplingRate*Duration, by = 1/SamplingRate)
    
    SimulatedEPSC <- (A/A_prime)*(exp(-t/Tau1) + (-exp(-t/Tau2))) #positive-going events
    SimulatedEPSC <- c(0, SimulatedEPSC[-1])
    
    #noise <- runif(length(SimulatedEPSC), -3, 3)
    
    #noisyfiedEPSC <- SimulatedEPSC + noise   
}

sweepdata <- list()
#position_array <- list()
for(n in 1:20) {
    sweep <- rep(0, 5000)
    sweepdata[[n]] <- sweep
}

for(i in 1:20) {
                temp <- sweepdata[[i]]
                position <- round(TOEinSegment[[i]]*SamplingRate)
                print(position)
                for(j in 1:NOEinSegment[i]) {temp <- replace(temp, position[j]:(position[j]+99), oneEvent+temp[position[j]])}
                
            sweepdata[[i]] <- temp
            #FinalEPSC <- as.data.frame(do.call(cbind, sweepdata))
            
            #position_array[[i]] <- position
                }

position_df <- as.data.frame(do.call(rbind, position_array))
plot(x=(1:length(unlist(sweepdata)))/SamplingRate, y=unlist(sweepdata), type="l", xlab = "time (sec)", ylab = "pA")

noise3 <- rnorm(unlist(sweepdata), mean = 0, sd = 6)*0.000000000001
noisyfiedEvent <- oneEvent+noise3

noise <- rnorm(dim(FinalEPSC)[1], mean = 0, sd = 6)
noise2 <- sgolayfilt(noise, p=3, n=15)
noisyfiedEPSC <- (FinalEPSC + noise)

plot(as.vector(unlist(FinalEPSC2[,1])), type = 'l', xlab = "Time(sec)")

write.csv(noisyfiedEPSC, file = "Simulated_EPSC_sd10.csv")
