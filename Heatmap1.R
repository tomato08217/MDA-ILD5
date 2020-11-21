####. heatmap of feature that <0.05
setwd("D:/renji/ILD")
library(readr)
library("reshape2")
library("pheatmap")
map <-read.xlsx("all_1198.xlsx",sheet=2)
map1 <-subset(map,select =c("CustomLabel","Survival","original_firstorder_RobustMeanAbsoluteDeviation","original_glszm_ZoneEntropy","original_shape_Flatness","original_shape_SphericalDisproportion","wavelet.HLL_glszm_LargeAreaHighGrayLevelEmphasis","wavelet.LLH_glcm_Idmn","wavelet.LLL_firstorder_90Percentile","wavelet.LLL_firstorder_Skewness","wavelet.LLL_glcm_MCC","wavelet.LLL_glszm_ZoneEntropy"))
map1_ordered <- map1[order(map1$CustomLabel),]

annotation_col = data.frame(Event = factor(map1_ordered$CustomLabel))

map2 <- map1_ordered[,2:12]#delete customlabel
map2 = t(map2)

colnames(map2) = paste("patient", 1:229, sep = "")
rownames(annotation_col) = colnames(map2)
ann_colors = list( Event = c("0" = "#CCFFFF", "1"="#006699"))
bk=unique(c(seq(-1,1,length=100)))#define range
pheatmap(map2,breaks=bk,scale = "row",color = colorRampPalette(c("#0C8569","#FEFE7A"))(100),cluster_rows = FALSE, cluster_cols = FALSE,annotation_col = annotation_col,main = "Features grouped by events",show_colnames = FALSE,show_rownames = FALSE,annotation_colors = ann_colors,border_color = "NA")
