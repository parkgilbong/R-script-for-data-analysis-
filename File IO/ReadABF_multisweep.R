library(readABF) #load 'readABF' library

wholefile <- readABF(file.choose()) # imprt ABF file 
wholefile 

SweepNum <- wholefile$header$lActualEpisodes
SamplingRate <- wholefile$samplingIntervalInSec
dataPtsPerChan <- wholefile$header$dataPtsPerChan

ExtractedSweep.df <- function (From, To) {
        Templist <- list()
        for (n in seq(From, To)) {
        df <- as.data.frame(wholefile, sweep = n)
        df$`Time [s]` <- df$`Time [s]` + (dataPtsPerChan*(n-1)*SamplingRate)
        df$Sweep <- c(n)
        Templist[[n]] <- df
        }
    Whole.df <- do.call(rbind, Templist)
    return(Whole.df)
    }

Sweep02.new <- ExtractedSweep.df(32,32)
plot(Sweep02.new$`Time [s]`, Sweep02.new$`IN 0 [pA]`, type = "l")
