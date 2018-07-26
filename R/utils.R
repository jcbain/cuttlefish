#' Calculate the Distance between a Point and a Vector
#'
#' Takes a point `x` and calculates the euclidean distance from a
#' vector `y`.
#'
#' @param x A numerical value.
#' @param y A numerical value or vector.
#' @return A vector of distances
euclid_distance <- function(x, y){
  sqrt((y-x)^2)
}

#' Find Unique RGB Colors in an Image
#'
#' `extract_colors` takes in an image and returns all of the
#' unique colors in the image in RGB color-space.
#'
#' @param image The image to load.
#' @return Returns a matrix with three columns refering to
#'     red, green and blue states of the pixel.
extract_colors <- function(image){
  img = magick::image_read(image)
  bitmap = img[[1]]
  hexcomps = apply(bitmap, MARGIN=1, FUN=function(x) x)
  hexes = unique(paste0("#", hexcomps[,1], hexcomps[,2], hexcomps[,3]))

  rgb <- t(col2rgb(hexes))
  apply(X = rgb, MARGIN = 2, FUN = as.numeric)
}


