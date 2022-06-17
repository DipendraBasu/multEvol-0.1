c1.sim.colour <- readRDS("C:/Users/dipendra/Desktop/Col_C1.RDS")
c1.sim.shape <- readRDS("C:/Users/dipendra/Desktop/Shape_C1.RDS")

akul<-unlist(list(one=c("mim","mod"), two= c("modmim1", "modsis1"), three= c("modmim2", "modsis2")))

df1<-NULL
for (i in 1:length(c1.sim.colour)){
  ak<-as.matrix(c1.sim.colour[[i]][[3]][,1])
  ak<-cbind(ak,rep(names(c1.sim.colour[i]), 100))
  df1<-rbind(df1, ak)
}

df1<-as.data.frame(df1)
al5<-NULL
al6<-NULL
for(i in 1:length(df1$V2)){
  al1<-df1$V2[[i]]
  al2<-names(akul)[match(al1, akul)]
  al3<-substr(al2, 1, (nchar(al2)-1))
  al4<-substr(al2,nchar(al2), nchar(al2))
  al5<-rbind(al5,al3)
  al6<-rbind(al6, al4)
}
df1$comb1<-al5
df1$comb2<-al6

df2<-NULL
for (i in 1:length(c1.sim.colour)){
  ak<-as.matrix(c1.sim.colour[[i]][[1]][1])
  ak<-cbind(ak,names(c1.sim.colour[i]))
  df2<-rbind(df2, ak)
}
df2<-as.data.frame(df2)
al5<-NULL
al6<-NULL
for(i in 1:length(df2$V2)){
  al1<-df2$V2[[i]]
  al2<-names(akul)[match(al1, akul)]
  al3<-substr(al2, 1, (nchar(al2)-1))
  al4<-substr(al2,nchar(al2), nchar(al2))
  al5<-rbind(al5,al3)
  al6<-rbind(al6, al4)
}
df2$comb1<-al5
df2$comb2<-al6

df3<-split(df1, df1$V2)
df4<-NULL
for(i in 1:length(df3)){
  aj<-mean(as.numeric(df3[[i]][,1]))
  aj<-cbind(as.numeric(aj), unique(df3[[i]][,2]))
  df4<-rbind(df4,aj)
}
df4<-as.data.frame(df4)

al5<-NULL
al6<-NULL
for(i in 1:length(df4$V2)){
  al1<-df4$V2[[i]]
  al2<-names(akul)[match(al1, akul)]
  al3<-substr(al2, 1, (nchar(al2)-1))
  al4<-substr(al2,nchar(al2), nchar(al2))
  al5<-rbind(al5,al3)
  al6<-rbind(al6, al4)
}
df4$comb1<-al5
df4$comb2<-al6


require(ggplot2)
require(ggforce)
require(gghalves)

source("geomhalfviolin.R")
source("geomhalfboxplot.R")

p<-ggplot()+ geom_half_violin(data=df1,aes(x=as.factor(comb1), y=as.numeric(V1), split = as.factor(comb2), fill=V2),position = "identity")+theme(legend.position="none", aspect.ratio = 1, panel.background = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"),panel.border = element_rect(colour = "black", fill=NA, size=0.5))


p1<-p+geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, colour = "segment"), data = df)




source("splitviolin.R")
library(ggplot2)
par(mfrow=c(1,2))
# p<-ggplot(df1, aes(as.factor(comb1), V1, fill = as.factor(comb2))) +
#   geom_split_violin(trim = TRUE)  

p<-ggplot()+ geom_half_violin(data=df1,aes(x=V2, y=as.numeric(V1),  fill=V2),position = "identity")+theme(legend.position="none", aspect.ratio = 1, panel.background = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"),panel.border = element_rect(colour = "black", fill=NA, size=0.5))+ylim(0,1)

p1<-p+geom_half_boxplot(data=df2, aes(x=V2, y=as.numeric(V1),col="blue"))
p2<-p1+geom_half_boxplot(data=df4,aes(x=V2, y=as.numeric(V1)))
