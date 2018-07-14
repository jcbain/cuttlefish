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

#' Find the n Most Distinct Colors
#'
#' `distint_hsv` selects the most distinct set of colors from a set of colors.
#' @param x Matrix of RGB colors.
#' @param n `n` distinct colors to be output.
distinct_hsv <- function(x, n){
  rgbmat = colorspace::RGB(x)
  hsv = colorspace::coords(as(rgbmat, "HSV"))

  max_angle = max(hsv[,3])
  angle =   max_angle/n

  start_ind = sample(1:nrow(hsv), 1)

  selection = matrix(, nrow = n, ncol = 3)
  selection[1,] = hsv[start_ind, ]

  for(i in 2:n){
    current_angle = selection[(i - 1), 3] + angle
    if(current_angle > max_angle){
      current_angle = current_angle - max_angle
    }

    min_ind = which.min(euclid_distance(current_angle, hsv[,3]))
    closest_angle = hsv[min_ind, 3]
    options = hsv[hsv[,3]==closest_angle,]

    farthest_ind = which.max(as.matrix(pdist::pdist(selection[i-1,1:2], options[,1:2])))
    selection[i,] = options[farthest_ind,]
    # find all values that are closest to new angle
    # select the one that is the farthest distance
    # assign that row to slection[i, ]
    # repeat n times

  }

  selection
}
