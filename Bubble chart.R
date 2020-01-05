library(ggplot2)
pc <- read.delim("~/Your_csv_file_larger.txt", header=T)
library(reshape2)
head(pc)
str(pc)
pcm <- read.delim("Your_csv_file_larger2.txt", header=T) #melt(pc, id=c("Sample", "Time"))
pcm

pcm$Sample <-  factor(pcm$Sample, levels=unique(pcm$Sample))

xx <- ggplot(pcm, aes(x = Sample, y = variable)) + 
    geom_point(aes(size = value, fill = Time), alpha = 1, shape = 21) + 
    scale_size_continuous(limits = c(0.000001, 55), range = c(1,10), breaks = c(10,25,50)) + 
    labs( x= "", y = "", size = "", fill = "")  + 
    theme(legend.key=element_blank(), 
          axis.text.x = element_text(colour = "black", size = 15, face = "plain", angle = 90, vjust = 0.3, hjust = 1), 
          axis.text.y = element_text(colour = "black", face = "plain", size = 12, angle = 0), 
          # legend.text = element_text(size = 10, face ="bold", colour ="black"), 
          # legend.title = element_text(size = 11, face = "bold"), 
          panel.background = element_rect(fill="white", colour="black"),
          legend.position = "right", panel.grid.major.y = element_line(colour = "grey85"),
          panel.grid.major.x = element_line(colour = "grey85")) +  
    scale_fill_manual(values = c("red", "salmon", "steelblue1", "blue", "white"), guide = guide_legend(override.aes = list(size=5))) + 
    scale_y_discrete(limits = rev(levels(pcm$variable))) 
xx
