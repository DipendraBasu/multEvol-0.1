gr1<-c("Idea_mala", "Para_agle", "Para_nilg", "Tiru_limn", "Tiru_sept") 
gr2<-c("Pare_ceym", "Pare_hipm", "Pare_hipf", "Pare_ceyf", "Papl_cldf", "Papl_cldm")


if (length(gr1)>length(gr2)){dimn1<-gr2
dimn2<-gr1}else{dimn1<-gr1
dimn2<-gr2}

'%notin%' <- Negate('%in%')

dd2<-NULL
dd3<-NULL

dd1<- Momocs::MSHAPES(FW_efourier,as.factor(FW_shape_gr$V1))
dd2$shp<- dd1$shp[-which(names(dd1$shp) %notin% c(dimn1))]
dd3$shp<- dd1$shp[-which(names(dd1$shp) %notin% c(dimn2))]
dd2$Coe<- dd1$Coe[-which(names(dd1$Coe) %notin% c(dimn1))]
dd3$Coe<- dd1$Coe[-which(names(dd1$Coe) %notin% c(dimn2))]

par(mar=c(4.5,4.5,4.5,4.5))
layout(matrix(1:(length(dimn1)*length(dimn2)), ncol=length(dimn1), byrow=T))

for(i in 1: length(dimn2)){
  for(j in 1:length(dimn1)){
    # plot(NULL,  asp = 1, xlim=c(0.5,-0.5), ylim=c(0.5, -0.5),xlab='', ylab='', xaxt='n', yaxt='n', axes=F)
    Momocs::tps_iso(dd3$shp[[i]], dd2$shp[[j]], grid =F, outline = dd2$shp[[j]], iso.nb =500, legend = F, shp = F, iso.levels = 50, cont = F, xlim=c(0.5,-0.5), ylim=c(0.5, -0.5),poly = T,  maxArr = 0.019)
    polygon(dd3$shp[[i]], col=NULL, xlim=c(0.5,-0.5), ylim=c(0.5, -0.5),border='black',  asp=1)
    print(c(i,j))
    }
  }
dev.off()
xlim=c(1.5,-1.5), ylim=c(1.5, -1.5),
xlim=c(1.5,-1.5), ylim=c(1.5, -1.5),
xlim=c(1.5,-1.5), ylim=c(1.5, -1.5),