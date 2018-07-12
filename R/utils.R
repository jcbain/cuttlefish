#' Find Unique RGB Colors in an Image
#'
#' `extract_colors` takes in an image and returns all of the
#' unique colors in the image in RGB color-space.
#' 
#' @param image The image to load.
#' @return Returns a tibble with three columns refering to
#'     red, green and blue states of the pixel.
extract_colors <- function(image){
  img = magick::image_read(image)
  bitmap = img[[1]]
  hexcomps = apply(bitmap, MARGIN=1, FUN=function(x) x)
  hexes = unique(paste0("#", hexcomps[,1], hexcomps[,2], hexcomps[,3]))

  dplyr::as_tibble(t(col2rgb(hexes)))
}


