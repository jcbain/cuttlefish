#' Find Unique Colors in an Image
#'
#' @param image The image to load.
extract_colors <- function(image){
  img = magick::image_read(image)
  bitmap = img[[1]]
  hexcomps = apply(bitmap, MARGIN=1, FUN=function(x) x)
  hexes = unique(paste0("#", hexcomps[,1], hexcomps[,2], hexcomps[,3]))

  hexes

}


