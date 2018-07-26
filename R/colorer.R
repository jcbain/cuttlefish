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

  rgb = t(col2rgb(hexes))
  apply(X = rgb, MARGIN = 2, FUN = as.numeric)
}

#' Find the n Most Distinct Colors
#'
#' `distint_hsv` selects the most distinct set of colors from a set of colors.
#' @param x Matrix of RGB colors.
#' @param n `n` distinct colors to be output.
#' @return Matrix of distinct hsv colors.
distinct_hsv <- function(x, n){

  # convert to matrix to RBG then to HSV
  rgbmat = colorspace::RGB(x)
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

    # find all values that are closest to new angle
    min_ind = which.min(euclid_distance(current_angle, tmp_hsv[,1]))
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
    }
  selection
}

#' Create a Color Palette of n Colors from an Image
#'
#' Creates a color palette of n distinct colors from a provided image.
#' @param img Path to image file.
#' @param n `n` distinct colors.
#' @return Vector of distinct hex colors.
#' @export
create_palette <- function(img, n){
  img_colors = extract_colors(img)
  hsv_mat = distinct_hsv(img_colors, n)
  tmp_hsv = colorspace::HSV(hsv_mat)

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
prominent_palette <- function(img, n){
  img = magick::image_read(img)
  bitmap = img[[1]]
  hexcomps = apply(bitmap, MARGIN=1, FUN=function(x) x)
  hexes = paste0("#", hexcomps[,1], hexcomps[,2], hexcomps[,3])
  
  names(sort(table(hexes), decreasing = T))[1:n]
}

