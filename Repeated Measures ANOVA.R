##########################################################################################
#                                 Wilcoxon-Mann-Whitney test                             #
##########################################################################################
require(psych)
df1 <- read.clipboard(header=F)
colnames(df1) <- c("X")
df2 <- read.clipboard(header=F)
colnames(df2) <- c("Y")

w.result <- wilcox.test(df1$X, df2$Y, paired = T)
w.result


##########################################################################################
#                                         T-test                                         #
##########################################################################################
require(psych) # in order to use the command "read.clipboard" in next line
df1 <- read.clipboard(header=F)
colnames(df1) <- c("X")
df2 <- read.clipboard(header=F)
colnames(df2) <- c("Y")

var.result <- var.test(df1$X, df2$Y) #to evaluate the sample variences of the two groups. If p-value is greater than 0.05, then two variances are homogeneous.

t.result <- if(var.result$p.value >= 0.05){
    t.test(df1$X, df2$Y, alternative = c("two.sided"), paired = F, var.equal = T)
    } else {
    t.test(df1$X, df2$Y, alternative = c("two.sided"), paired = F, var.equal = F)    
    }
    
t.result

##########################################################################################
#                                    Correlation test                                    #
##########################################################################################
require(psych) # in order to use the command "read.clipboard" in next line
df1 <- read.clipboard(header=F)
colnames(df1) <- c("X")
df2 <- read.clipboard(header=F)
colnames(df2) <- c("Y")
df2$Y <- as.integer(df2$Y)
nor.result <- shapiro.test(df1$X) #to evaluate the normality of samples. If p-value is greater than 0.05, then the sample passes normality test.

cor.result <- if(var.result$p.value >= 0.05){
    cor.test(df1$X, df2$Y, alternative = c("two.sided"), method = c("spearman"))
} else {
   cor.test(df1$X, df2$Y, alternative = c("two.sided"), method = c("pearson"))    
}

cor.result

##########################################################################################
#                                      One way ANOVA                                     #
##########################################################################################
df.aov <- read.delim(file= "C:/Users/KimYG/Desktop/Rheobase Current_aov.txt", 
                     sep = "\t", 
                     stringsAsFactors = T, 
                     header = T)
headtail(df.aov)
str(df.aov)
result.aov <- aov(Value ~ Group, data = df.aov)
summary(result.aov)
boxplot(Value ~ Group, data = df.aov,
        xlab = "Group", ylab = "Input Resistance",
        frame = T, col = c("#00AFBB", "#E7B800", "#FC4E07"))
# Multiple pairwise-comparison between the means of groups
TukeyHSD(result.aov) 
pairwise.t.test(df.aov$Value, df.aov$Group, p.adj="bonferroni", paired=F)

##########################################################################################
#                               Repeated Measures ANOVA                                  #
##########################################################################################
Data1 <- read.delim(file = "C:/Users/KimYG/Desktop/PFL_WT_depol.txt", sep = "\t", stringsAsFactors = T, header = T)
Data1$Group <- c("WT")
Data2 <- read.delim(file = "C:/Users/KimYG/Desktop/PFL_OL_depol.txt", sep = "\t", stringsAsFactors = T, header = T)
Data2$Group <- c("OL")
Data.df <- bind_rows(Data1, Data2)
Data.df$CellID <- as.factor(Data.df$CellID)
Data.df$Group <-  as.factor(Data.df$Group)
Data.df$Injected.current <- factor(Data.df$Protocol,levels(Data.df$Protocol), label= c(1:12)*100)
# colnames(Data.df) <- c("CellID", "Baseline", "PA", "Rin", "SS", "Sag", "RB", "Protocol", "Group","Injected.Current")
colnames(Data.df) <- c("CellID", "Baseline", "NOS", "MFR", "TD", "PBAHP", "Protocol", "Group", "Injected.Current")
Data.df.new <- filter(Data.df, !grepl("190719", CellID)) # filter dataframe to extract
Data.df.new$PBAHP[Data.df.new$PBAHP == 0] <- NA

str(Data.df.new)
require(Rmisc)
Summary.df <- summarySE(Data.df.new, measurevar = "MFR", groupvars = c("Injected.Current", "Group"), na.rm = T) #Summerize data Mean, SD, SEM 
Summary.df.Sag <- summarySE(Data.df.new, measurevar = "Sag", groupvars = c("Injected.Current", "Group"))
.replace.levels <- function (vec, replace.list) {
    # Checking if all levels to be replaced are in vec (and other way around)
    cur.levels <- unique(as.character(vec))
    not.in.cur.levels <- setdiff(cur.levels, names(replace.list))
    not.in.new.levels <- setdiff(names(replace.list), cur.levels)
    if (length(not.in.cur.levels) != 0 | length(not.in.new.levels) != 0) {
        stop("The following elements do not match:", paste0(not.in.cur.levels, not.in.new.levels, collapse = ","))
    }
    for (el in 1:length(replace.list)) {
        vec <- gsub(names(replace.list[el]), replace.list[[el]], vec)
    }
    vec
}

# Summary.df$Protocol <- .replace.levels(Summary.df$Protocol, list("Level_000"="+100 pA", "Level_001"="+200 pA", "Level_002"="+300 pA",
#                                           "Level_003"="+400 pA", "Level_004"="+500 pA", "Level_005"="+600 pA", "Level_006"="700 pA", 
#                                           "Level_007"="+800 pA", "Level_008"="+900 pA", "Level_009"="+1000 pA", "Level_010"="+1100 pA",
#                                           "Level_011"="+1200 pA"))


ggplot(Summary.df, aes(x=Injected.Current, y=MFR, colour=Group, group=Group)) + 
    geom_errorbar(aes(ymin=MFR-se, ymax=MFR+se), colour="black", width=.2) +
    geom_line() +
    geom_point(size=5, shape=21, fill="white") + # 21 is filled circle
    labs(x="Injected Current (pA)", y="Peak Amplitude (mV)", title="") + #Add axis titles
    scale_colour_hue(name= "",    # Legend label, use darker colors
                     breaks=c("WT", "OL"),
                     labels=c("Control", "OKR Learning"),
                     l=40) +                    # Use darker colors, lightness=40
    expand_limits(y=2) +                        # Expand y range
    scale_y_continuous(breaks=0:20*5) +         # Set tick every 2
    theme_classic(base_size = 11) +
    theme(legend.justification=c(1,0),
          legend.position="top",
          axis.text=element_text(size=10),
          axis.title=element_text(size=18, face="bold"))               # Position legend in bottom right

boxplot(MFR ~ Protocol*Group, data = Data.df,
        xlab = "Group", ylab = "Input Resistance",
        frame = T, col = c("#00AFBB", "#E7B800", "#FC4E07"))

require(ggplot2)
p <- ggplot(Data.df, aes(Protocol, MFR, colour=Group)) + geom_boxplot()
p
Data <- read.delim(file = "C:/Users/KimYG/Desktop/MFR_T+G.txt", 
                   sep = "\t", 
                   stringsAsFactors = T, 
                   header = T)
Data$Gain[is.na(Data$Gain)] <- 0 
#Data = read.table(textConnection(Input), header = T)
#Data$Instruction <- factor(Data$Instruction, levels = unique(Data$Instruction)) #Student is treated as a random variable in the model 
library(psych)
library(nlme)
library(car)
#------------------------code for analysis
model = lme(RB ~ Group + Protocol + Group*Protocol, 
            random = ~1|CellID,
            data=Data.df.new,
            method="REML",
            na.action = na.omit)
Anova(model)
