A <- -0.000003834927 # -3834.927 nS
Tau1 <- 0.0005 # 0.5 ms
Tau2 <- 0.002 # 3 ms
SamplingRate <- 10000
Duration <- 0.01

randomfactor1 <- rnorm(500, mean=1, sd=0.3)
RandomFactor <- rexp(500, rate=1)
NOEinSegment <- round(runif(20, min = 0, max = 250)) #The number of events for a 500ms-long segment is drawn from uniform distribution.
TOEinSegment <- list()
for (i in 1:length(NOEinSegment)) {TOEinSegment[[i]] <- runif(NOEinSegment[i], min = 0, max = 0.5)} #The Timepoint of events in a 500ms-long segment is drawn from uniform distribution. 

simulatedEPSC <- list()

oneEvent <- generateEPSC(A, 
                        Tau1, 
                        Tau2, 
                        SamplingRate, 
                        Duration, 
                        1)
plot(0.00001:length(oneEvent), oneEvent, type = "l")    
#stackedEPSC <- as.data.frame(do.call(rbind, simulatedEPSC)

plot(as.vector(unlist(stackedEPSC[1,])), type = 'l', xlab = "Time(sec)")

generateEPSC <- function(A, Tau1, Tau2, SamplingRate, Duration, RandomFactor) {  
    A_prime <- (Tau2^(Tau1/(Tau1-Tau2)))/Tau1
    
    t <- seq(from = 0, length.out = SamplingRate*Duration, by = 1/SamplingRate)
    
    SimulatedEPSC <- (A/A_prime)*(exp(-1/Tau1) + (-exp(-t/Tau2))) #positive-going events
    SimulatedEPSC <- c(0, SimulatedEPSC[-1])
    
    #noise <- runif(length(SimulatedEPSC), -3, 3)
    
    #noisyfiedEPSC <- SimulatedEPSC + noise   
}

sweepdata <- list()
position_array <- list()
for(n in 1:25) {
    sweep <- rep(0, 100000)
    sweepdata[[n]] <- sweep
}

for(i in 1:25) {
    temp <- sweepdata[[i]]
    position <- sort(sample(1:100000, 20))
    for(j in 1:20) {
        temp <- replace(temp, seq(from = position[j], to = position[j] + 99), unname(unlist(stackedEPSC[sample(1:200,1), ])))}
    sweepdata[[i]] <- temp
    EPSC <- as.data.frame(do.call(cbind, sweepdata))
    position_array[[i]] <- position}

position_df <- as.data.frame(do.call(rbind, position_array))

noise <- rnorm(dim(EPSC)[1], mean = 0, sd = 10)
noise2 <- sgolayfilt(noise, p=3, n=15)
noisyfiedEPSC <- (EPSC + noise2) - 200

plot(as.vector(unlist(noisyfiedEPSC[,24])), type = 'l', xlab = "Time(sec)")

write.csv(noisyfiedEPSC, file = "Simulated_EPSC_sd10.csv")
