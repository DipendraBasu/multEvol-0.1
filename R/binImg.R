#' Creating square binary images for outline analysis.
#'
#' @name binImg
#'
#' @description Transforming raw images into binary images (black foreground, white background) for the analysis of shape
#' outlines.
#'
#' @details A robust function that thresholds .jpg images of specimens photographed against a lighter background (close to
#'  white) and transforms them into a square blank-and-white image that is resized to user-defined dimensions. This
#'  transformed images are directly saved to a user-defined folder.
#'
#' @usage  binImg(img_path, thresh, dimn, out_path)
#'
#' @param img_path The path of the raw image files.
#' @param thresh The thresholding cutoff (ranges from 0 to 1). defaults to 0.9. Images with a darker foreground require
#' lower threshold values to efficiently segregate the background and foreground.
#' @param dimn Number of pixels (length and width) for image resizing. Defaults to 100.
#' @param out_path Folder path for saving the binarized images.
#'
#' @return None. Transformed and resized binary images are directly saved in the user-defined folder.
#'
#' @examples
#' \dontrun{
#' binImg(img_path = "img_folder/", thresh = 0.97, dimn = 100, out_path = "test/")
#' }
#'
#' @rdname binImg
#'
#' @export
#' @import magick

binImg <- function(img_path = NULL,
                   thresh = 0.9,
                   dimn = 100,
                   out_path = NULL){


  list_img<- list.files(img_path, pattern = ".jpg")

  for(i in 1:length(list_img)){
  x = image_read(paste0(img_path,list_img[i]))
  x1 = image_convert(x, colorspace = 'gray')
  uca<-image_threshold(x1, type = "black", threshold = paste0((thresh*100), "%"))
  uca1<-image_resize(uca, geometry_size_pixels(dimn, dimn, preserve_aspect = T))
  uca2<-image_extent(uca1, paste0((dimn*1.2),"x",(dimn*1.2)), color = 'white', gravity = "Center")
  uca2<-image_resize(uca2, geometry_size_pixels(dimn, dimn, preserve_aspect = T))
  uca3<-image_despeckle(uca2, times = 15)
  uca3<-image_threshold(uca3, type = "black", threshold = "90%")
  uca3<-image_despeckle(uca3, times = 15)
  uca3<-image_threshold(uca3, type = "black", threshold = "90%")
  uca3<-image_despeckle(uca3, times = 15)
  uca3<-image_threshold(uca3, type = "black", threshold = "90%")
  uca3<-image_despeckle(uca3, times = 5)
  print(list_img[i])
  image_write(uca3, path = paste0(out_path, list_img[i]), format = "jpeg")
  }
}

