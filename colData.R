colData <- function(sp_index, col_patch, img_folder, ext, target, sampletype= "point" resampleFactor=1, out_folder, stacks_folder=NULL)
{
  col_seq <- paste(paste0(1:length(col_patch), "=",col_patch, "; "), collapse="")
  col.input <- as.numeric(readline(prompt = paste0("select colour patch (", col_seq, "): ")))
  # numcol <- length(col_patch)
  col_patch <- col_patch[col.input]
  sp_list <- sp_index[[which(names(sp_index) == col_patch)]]
  list_images <- list.files(img_folder, pattern = paste0(sp_list, collapse = "|"))
  ind_list <- as.vector(sapply(list_images, function(x) {substr(x, 1, 12)}))
  ind_species <- as.vector(sapply(list_images, function(x) {substr(x, 1, 9)}))
  image_list <- ID_list <- lapply(split(ind_list, ind_species), function(x) {as.list(unlist(split(x, 1), recursive = TRUE))})
  for(i in seq_along(ID_list))
  {
    for(j in seq_along(ID_list)[[i]])
    {
      image_list[[i]] <- patternize::makeList(unlist(ID_list[[i]]), 'image', img_folder, ext)
    }
  }
  saveRDS(ID_list, paste0(out_folder, col_patch, "_IDList.RDS"))
  
  targets <- as.data.frame(target[which(target$sp %in% sp_list), ])
  targets[, c('red', 'green', 'blue', 'offset')] <- NA
  stack_list <- out.mean <- image_list
  col.offset <- c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3, 0.35, 0.4)

  for(i in 1:length(image_list))
  {
    sp.name <- names(image_list)[i]
    print(paste0(i, " ", col_patch, "_", sp.name))
    sample_id <- as.numeric(targets[which(targets$sp == sp.name), 2])
    sample_ind <- target_ind <- image_list[[i]][[sample_id]]
    repeat
    {
      par(mar = c(rep(0, 4)))
      offset.val <- col.offset[as.numeric(readline(prompt = "choose colour offset (1=0.05; 2=0.1; 3=0.15; 4=0.2; 5=0.25; 6= 0.3; 7=0.35; 8= 0.4): "))]
      rgb <- patternize::sampleRGB(sample_ind, type = sampletype,resampleFactor = 2)
      stack_list[[i]] <- patternize::patRegRGB(image_list[[i]], target = target_ind, RGB = rgb, plot = "stack", colOffset = offset.val, resampleFactor = resampleFactor)
      cont <- readline(prompt = "continue (press 'y' for yes): ")
      if(cont == "y")
      {
        if(!is.null(stacks_folder))
        {
          dev.print(x11)
          numdim <- as.numeric(dim(stack_list[[i]][[1]])[1])
          dev.print (device=png, paste0(stacks_folder, col_patch, "_", sp.name, ".png"), width = numdim, height = numdim)
          dev.off()
        }
        targets$red[i] <- rgb[1]
        targets$green[i] <- rgb[2]
        targets$blue[i] <- rgb[3]
        targets$offset[i] <- offset.val
        break
      }
    }
    out.temp <- rbind.data.frame(lapply(stack_list[[i]], function(x) {raster::getValues(x)}))
    out.temp[is.na(out.temp)] <- 0
    out.mean[[i]] <- rowMeans(out.temp)
  }

  mean_matr<-matrix(0L,ncol=length(unique(unlist(sp_index))), nrow= (numdim^2))
  colnames(mean_matr)<-unique(unlist(sp_index))
  for (i in 1: length(out.mean)){
    sp_nm<-names(out.mean)[i]
    colnumb<-grep(sp_nm,colnames(mean_matr))
    mean_matr[,colnumb]<-out.mean[[i]]
  }
  mean_matr[1:ncol(mean_matr)]<-sapply(X = mean_matr[1:ncol(mean_matr)], as.numeric)
  
  saveRDS(targets, paste0(stacks_folder, col_patch, "_allRGB.RDS"))
  saveRDS(stack_list, paste0(out_folder, col_patch, "_RasterStacks.RDS"))
  saveRDS(mean_matr, paste0(out_folder, col_patch, "_mean.RDS"))
}
