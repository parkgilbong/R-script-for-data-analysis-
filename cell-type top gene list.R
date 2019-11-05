load("Normalized_table.rda")
Genelist <- read.table(file= "clipboard",sep = "\t", stringsAsFactors = F, header= F)
colnames(Genelist) <- c("Gene_name")
Genelist2 <- merge(Normalized.value, Genelist, by = "Gene_name")
Genelist3 <- Genelist2[order(Genelist2$UL_24H, decreasing = T),]
Genelist4 <- Genelist2[order(Genelist2$UL_24H, decreasing = F),]
Genelist5 <- rbind(Genelist3[1:3,], Genelist4[1:3,])
write(Genelist5, "clipboard", sep = "\t")
Genelist5
