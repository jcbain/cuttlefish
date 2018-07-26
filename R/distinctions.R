#' Find the n Most Distinct Colors
#'
#' `distint_hsv` selects the most distinct set of colors from a set of colors.
#' @param x Matrix of RGB colors.
#' @param n `n` distinct colors to be output.
#' @export
distinct_hsv <- function(x, n){

  # convert to matrix to RBG then to HSV
  rgbmat = colorspace::RGB(x)
  hsv = colorspace::coords(as(rgbmat, "HSV"))

  # find max angle in HSV matrix and divide
  max_angle = max(hsv[,3])
  angle = max_angle/n

  # find a random starting index
  start_ind = sample(1:nrow(hsv), 1)
  selection = matrix(, nrow = n, ncol = 3)
  selection[1,] = hsv[start_ind, ]

  for(i in 2:n){
    # create new angle
    current_angle = selection[(i - 1), 3] + angle
    if(current_angle > max_angle){
      current_angle = current_angle - max_angle
    }

    # find all values that are closest to new angle
    min_ind = which.min(euclid_distance(current_angle, hsv[,3]))
    closest_angle = hsv[min_ind, 3]
    options = hsv[hsv[,3]==closest_angle,]

    # select the one that is the farthest distance
    farthest_ind = which.max(as.matrix(pdist::pdist(selection[i-1,1:2], options[,1:2])))

    # assign that row to slection[i, ]
    selection[i,] = options[farthest_ind,]
  }

  selection
}
