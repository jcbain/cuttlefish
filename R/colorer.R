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

  paste0("#", hexcomps[,1], hexcomps[,2], hexcomps[,3])
}

#' Find the n Most Distinct Colors
#'
#' `find_segmented` selects the most distinct set of colors from a set of colors.
#' @param x Matrix of RGB colors.
#' @param n `n` distinct colors to be output.
#' @return Matrix of distinct hsv colors.
find_segmented <- function(hexes, n){
  uniq_hexes = unique(hexes)
  tmp_rgb = t(col2rgb(uniq_hexes))
  tmp_rgb = apply(X = tmp_rgb, MARGIN = 2, FUN = as.numeric)

  # convert to matrix to RBG then to HSV
  rgbmat = colorspace::RGB(tmp_rgb)
  tmp_hsv = colorspace::coords(as(rgbmat, "HSV"))

  # find max angle in HSV matrix and divide
  max_angle = max(tmp_hsv[,1])
  angle = max_angle/n

  # find a random starting index
  start_ind = sample(1:nrow(tmp_hsv), 1)
  selection = matrix(nrow = n, ncol = 3)
  selection[1,] = tmp_hsv[start_ind, ]

  for(i in 2:n){
    # create new angle
    current_angle = selection[(i - 1), 1] + angle
    if(current_angle > max_angle){
      current_angle = current_angle - max_angle
    }

    # find all values that are closest to new angle and that aren't the previous angle
    min_ind = which.min(euclid_distance(current_angle, tmp_hsv[tmp_hsv[,1]!=tmp_hsv[i-1,1],][,1]))
    closest_angle = tmp_hsv[min_ind, 1]
    options = tmp_hsv[tmp_hsv[,1]==closest_angle,]

    if(is.null(dim(options))){
      selection[i,] = t(matrix(options))
    } else {
      # select the one that is the farthest distance
      farthest_ind = which.max(as.matrix(pdist::pdist(selection[i-1,2:3], options[,1:2])))

      # assign that row to slection[i, ]
      selection[i,] = options[farthest_ind,]
    }
    # remove color from possible selections
    removal_ind = which(tmp_hsv[,1] == selection[i,1] & tmp_hsv[,2] == selection[i,2] & tmp_hsv[,3] == selection[i,3])
    tmp_hsv = tmp_hsv[-removal_ind,]
    }

  tmp_hsv = colorspace::HSV(selection)
  tmp_rgb = colorspace::coords(as(tmp_hsv, "RGB"))
  rgb(tmp_rgb, maxColorValue = 255)

}

#' Create a Palette of the Most Prominent Colors of an Image
#'
#' Creates a color palette from the most prominent `n` colors of a
#'     provided image.
#' @param img Path to image file.
#' @param n top `n` most prominent colors.
#' @return Vector of the most prominent hex colors.
#' @export
find_prominent <- function(hexes, n){
  names(sort(table(hexes), decreasing = T))[1:n]
}

