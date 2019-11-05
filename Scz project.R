rm(list = ls())

expression_mRNA_17_Aug_2014 <- read.delim (file.choose(), sep = "\t", stringsAsFactors = F, header = F);
# save(expression_mRNA_17_Aug_2014, file = "~/mRNA_Science_2015.rda")
dim(expression_mRNA_17_Aug_2014)
Expression_table <- expression_mRNA_17_Aug_2014[c(12:19983), ]
Information_table <- expression_mRNA_17_Aug_2014[c(1:10), ]

rownames(Expression_table) <- Expression_table[,1]
Expression_table <- Expression_table[,-c(1,2)]

rownames(Information_table) <- Information_table[,2]
Information_table <- Information_table[, -c(1,2)]
rownames(Information_table)

names(Expression_table) <- Information_table[8,]
names(Information_table) <- Information_table[8,]

#=============================================================================================================
#=============================================================================================================
#=============================================================================================================
#=============================================================================================================

temp.list <- read.table("clipboard", sep = "\t", stringsAsFactors = F)
names(temp.list) <- c("Gene_name")

Result.table <- Expression_table[match(temp.list$Gene_name, rownames(Expression_table)),]
Result.table.matrix <- as.numeric((as.matrix(unname(Result.table))))
Result.table.matrix <- matrix(Result.table.matrix, ncol=47, nrow= 3005, byrow = T)
Result.table.dataframe <- as.data.frame(Result.table.matrix, 
                                        row.names = names(Expression_table), 
                                        col.names = rownames(Result.table))
names(Result.table.dataframe) <- temp.list$Gene_name
str(Result.table.dataframe)

Result.table.dataframe$cell <- data.frame(type=matrix(Information_table[9,], nrow = 3005))
Result.table.dataframe$cell <- as.factor(unlist(Result.table.dataframe$cell))
str(Result.table.dataframe)

levels  <- levels(Result.table.dataframe$cell)
temp.matrix <- data.frame(matrix(NA, ncol=47, nrow = 7))
j <- 1
for (level in levels) {
    temp.frame <- Result.table.dataframe[Result.table.dataframe$cell == level,]
        temp.vector <- c()
    for (i in 1:47) {
            temp.vector[i] <- mean(temp.frame[,i])
    }
    print(temp.vector)
    temp.matrix[j,] <- temp.vector
    j <- j+1
}
names(temp.matrix) <- temp.list$Gene_name
temp.matrix <- cbind(cell.type = levels, temp.matrix)
str(temp.matrix)
library(reshape)
temp.matrix <- melt(temp.matrix, id = "cell.type")
temp.matrix$variable <- as.factor(temp.matrix$variable)


library(ggplot2)
f <- ggplot(data=temp.matrix, aes(x=cell.type, y= value, color = variable)) + 
    geom_point() 
f 
    
#f + scale_y_continuous(limits = c(-2.5, 2.5))
