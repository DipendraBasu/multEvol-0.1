gr1<-c("Idea_mala", "Para_agle", "Para_nilg", "Tiru_limn", "Tiru_sept") 
gr2<-c("Pare_ceym", "Pare_hipm", "Pare_hipf", "Pare_ceyf", "Papl_cldf", "Papl_cldm")

if (length(gr1)>length(gr2)){dimn1<-gr2
dimn2<-gr1}else{dimn1<-gr1
dimn2<-gr2}

'%notin%' <- Negate('%in%')

data1<-MEL_RasterStacks[-which(names(MEL_RasterStacks) %notin% c(dimn1))]
ID1<-MEL_IDList[-which(names(MEL_IDList) %notin% c(dimn1))]
data2<-MEL_RasterStacks[-which(names(MEL_RasterStacks) %notin% c(dimn2))]
ID2<-MEL_IDList[-which(names(MEL_IDList) %notin% c(dimn2))]

ak<-list()
for(i in 1:length(dimn1)){
  ID3<-unlist(ID1[[i]])
  names(ID3)<-NULL
  ak[[i]] <- patternize::sumRaster(rList = data1[[i]], IDlist = ID3, type ='RGB' )
}
saveRDS(ak,"model_sumraster.RDS")
ak1<-list()
for(i in 1:length(dimn2)){
  ID3<-unlist(ID2[[i]])
  names(ID3)<-NULL
  ak1[[i]] <- patternize::sumRaster(rList = data2[[i]], IDlist = ID3, type ='RGB' )
}
saveRDS(ak1,"mimic_sumraster.RDS")
a1<-list.files(pattern="Outline")
target<-a1[[1]]
out.txt<-read.table(a1[[2]])

dev.off()
par(mar=c(1,1,1,1))
m <- matrix(c(0,1,2,3,4,5,6, 12,13, 14, 15,16,7,17,18,19,20,21,8,22,23,24,25,26,9,27,28,29,30,31,10,32,33,34,35,36,11,37,38,39,40,41), ncol=6, nrow=7, byrow = T)
layout(m)

colfunc <- viridis::plasma(100)
toto1<-list()
# plot(x=1:100, y=1:100,type="n", xlab='', ylab='', xaxt='n', yaxt='n', axes=F)
for (i in 1:length(dimn1)){
toto1[[i]]<-patternize::plotHeat(ak[[i]], as.vector(unlist(ID1[[i]])), plotCartoon = T, refShape ='target',outline = out.txt, refImage = data1[[i]][[1]], crop = c(0,0,0,0),flipRaster ='x', cartoonOrder = 'under',cartoonFill ='black',  colpalette = colfunc)}

toto2<-list()
for (i in 1:length(dimn2)){
  toto2[[i]]<-patternize::plotHeat(ak1[[i]], as.vector(unlist(ID2[[i]])), plotCartoon = T, refShape ='target',outline = out.txt, refImage = data2[[i]][[1]], crop = c(0,0,0,0),flipRaster ='x', cartoonOrder = 'under',cartoonFill ='black',  colpalette = colfunc)}


colfunc1<-colorspace::diverge_hcl(100)
toto3<-list()
for (i in 1:length(dimn2)){
  for(j in 1:length(dimn1)){
    psro<- (ak[[j]]/length(ID1[[j]]))-(ak1[[i]]/length(ID2[[i]]))
    toto4<-patternize::plotHeat(psro, normalized=T, plotCartoon = T, refShape ='target',outline = out.txt, refImage = data2[[i]][[1]], crop = c(0,0,0,0),flipRaster ='x', cartoonOrder = 'under',cartoonFill ='black',  colpalette =colfunc1)
    toto4<-append(toto4, toto3)
    }
}
source("C:/Users/dipendra/Desktop/Method/Codes and data/combinelist.R")
tot<-combine.lists(toto1, toto2)    
tot<-combine.lists(tot, toto3)    
