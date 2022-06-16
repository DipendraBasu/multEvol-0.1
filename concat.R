concat<-function(file_names, file_path=NULL, save=FALSE, save_path=NULL){
  data1<-NULL
  for(i in 1:length(file_names)){
    col_data<-readRDS(paste0(file_path, file_names[i]))
    data1<-cbind(data1, t(as.data.frame(col_data)))
  }
  data1<-as.data.frame(data1)
  if(save==TRUE){
    saveRDS(data1, paste0(save_path, "col_concat.RDS"))
  }
  return(data1)
}