'%notin%' <- Negate('%in%')  
ring.tips <- c("Idea_mala", "Para_agle", "Para_nilg", "Tiru_limn", "Tiru_sept", "Pare_ceym", "Pare_hipm", "Pare_hipf", "Pare_ceyf", "Papl_cldf", "Papl_cldm")
  ring.tree <- ape::drop.tip(sample.tree, which(sample.tree$tip.label %notin% ring.tips))
dev.off()  
par(mfrow=c(1,2))  
  ring.df <- Shape_PCA[which(rownames(Shape_PCA) %in% ring.tips), 1:2]
  phytools::phylomorphospace(ring.tree, ring.df)
  
  ring.df <- Col_PCA[which(rownames(Col_PCA) %in% ring.tips), 1:2]
  phytools::phylomorphospace(ring.tree, ring.df)
