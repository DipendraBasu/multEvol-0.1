cleq.efa<-function(img_path=NULL, gr, rslt_path=NULL){
  img_list<-list.files("FW_mod") #setting path
  setwd("FW_mod")
  imgs<-Momocs::import_jpg(jpg.path = img_list) #Reading outlines from binary images
  img_out<-Momocs::Out(imgs) # Converting to out object
  img1<- Momocs::coo_smooth(img_out, 5)
  img1<-Momocs::coo_sample(img1,min(Momocs::coo_nb(img1)))
  img1<-Momocs::coo_center(img1)
  img1<-Momocs::fgProcrustes(img1, tol = 1e-4)
  img1<-Momocs::coo_scale(img1)
  #showing stacked outlines
  repeat
  { 
    stack(img1)
    cont <- readline(prompt = "continue (y/n): ") #option to continue with the displayed outline stack
    if(cont != "y")
      {
		    img1<-Momocs::coo_slidedirection(img1,"down")
    }
    {
      dev.print(x11)
      dev.copy2pdf ( paste0("ContourStack.pdf"), width = 6, height = 6)
      dev.off()
    }
    osc<-Momocs::coo_oscillo(img1[1], method = "efourier")
    dev.copy2pdf()
    trgt<-readline(prompt = "target image name(without extension): ")
    trgt<-as.character(trgt)
    pto<-Momocs::Ptolemy(img1[20],nb.h = 7)
    pto<-Momocs::Ptolemy(img1[grep(trgt,img1)])
    nb_harmonics<-readline(prompt = "number of harmonics for callibration: ")
    nb_harmonics<-as.numeric(nb_harmonics)
    chp<-Momocs::calibrate_harmonicpower_efourier(img1, nb.h = 20)
    chp<-calibrate_harmonicpower_efourier(img1, nb.h = nb_harmonics)
    chp$minh
    tp<-chp$minh[3]
    dk1<-chp$gg$data[1:((tp+2)*length(img1)),2:3]
    dk1[, 1] <- factor(dk1[, 1], levels = unique(dk1[, 1]))
    ggplot2::ggplot(dk1, ggplot2::aes(harm, hp))+ggplot2::geom_boxplot()+ggplot2::theme_classic()
    
    nb_harmonics1<-readline(prompt = "number of harmonics for Fourier: ")
    nb_harmonics1<-as.numeric(nb_harmonics1)
    calibrate_reconstructions_efourier(d2, id= "CallerebiaOrixaF_NCBS-AJ416_NCBS_2018-01-08_VaishaliBhaumik", range = 1:18)
    h<-Momocs::efourier(img1, nb.h = 7, norm=F)
    saveRDS(h, "FW_efourier.RDS")
    break
  }
}