setwd("D:/R/Methods")
rm(list=ls())

source("colData.R")
sp_index <- readRDS("sp_index.RDS")
col_patch <- c("MEL", "RO", "YEL", "BLG")
col_index <- readxl::read_xlsx("colour_sampling.xlsx", col_names = TRUE)

colData(sp_index=sp_index, col_patch=col_patch, img_folder = "images/", ext = ".jpg", target = col_index, out_folder = "testout/", resampleFactor = 3, stacks_folder = "testout/stacks/")

a1<-list.files("testout/", "_mean.RDS")
source("concat.R")
da1<-concat(a1, "testout/", save=T)

'%notin%' <- Negate('%in%')
tre1<-ape::read.tree("wg.tree")
gr1<-unique(FW_shape_gr)
sample.tree <- ape::drop.tip(tre1, which(tre1$tip.label %notin% unlist(c(gr1))))
ape::write.tree(sample.tree, file='sample_tiru.tree')

sample.tree<-ape::read.tree("sample_tiru.tree")
col_concat<-col_concat[,1:30000]
data1<-col_concat[, colSums(col_concat != 0) > 0]
rm(col_concat)
rm(xa)
source("dataPrep.R")
xa<-data.prep(sample.tree, FW_mshape_EFA, scale=T, method = "BM", mode = "cov")

saveRDS(xa, "Shape_PCA.RDS")

source("pcModels.R")
ak<-pc.aicc(sample.tree, xa[,1:4])
saveRDS(ak, "Col_PCmodel.RDS")

kojo<-scale(data1, scale = T, center = T)
xb<-phytools::phyl.pca(sample.tree, kojo,method = "BM", mode = "cov")
summary(xb)

#Rate

lk1<-list("Papl_cldm","Papl_cldf", "Pare_ceym","Pare_ceyf","Pare_hipm","Pare_hipf")
lk2<-list("Papl_cldf","Papl_cldm", "Pare_ceyf","Pare_ceym","Pare_hipf","Pare_hipm")
lk3<-unique(unlist(lk1))
lk4<-list(Shape_PCA, Col_PCA)
dd4<-matrix(nrow = 57, ncol = 1)
for(i in 1:length(lk4)){
  print(Sys.time())
  dd<-lk4[[i]]
  dd1<-RRphylo(tree = sample.tree, y = dd)
  dd1_1<-as.matrix(dd1$rates)
  
  for(j in 1:length(lk1)){
    tip1<-unlist(lk1[[j]])
    tip2<-unlist(lk2[[j]])
    wg.tree.w1 <- drop.tip(sample.tree, tip1)
    dd2<-RRphylo(tree = wg.tree.w1, y = dd)
    dd3<-as.matrix(dd2$rates)
    lk5<-dd3[grep(tip2, rownames(dd3)),]
    lk6<-grep(tip2, rownames(dd1_1))
    dd1_1[lk6]<-lk5
    print(i)
    print(j)
  }  
  dd1_2<-c(dd1_1[29:57,], dd1_1[1:28,])
  dd4<- cbind(dd4,as.data.frame(dd1_2))
}
dd4<-dd4[,-1]
colnames(dd4)<-c("Shape", "Colr")
saveRDS(dd4,"Rate_tiru.RDS")

dd3<-dd4$Colr
boxplot(dd3)

b<-min(boxplot(dff$colr_tot)$out)
for (i in 1:length(boxplot.stats(dd3)$out)){
  dd3[grep(boxplot.stats(dd3)$out[i], dd3)]<-b
} 
library(ggtree)
library(viridis)
ggtree::ggtree(sample.tree, aes(color=dd3)) +
  ggplot2::scale_colour_gradientn(colours = plasma(n = 10000, end=0.9,direction = -1)) +
  theme(legend.position="right")
plot(sample.tree)
