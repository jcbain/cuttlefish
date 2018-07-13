#' Find the n Most Distinct Colors
#'
#' `distint_hsv` selects the most distinct set of colors from a set of colors.
#' @param x Matrix of RGB colors.
#' @param n `n` distinct colors to be output.
distinct_hsv <- function(x, n){
  angle =   360/n

  rbg = colorspace::RGB(x)
  hsv = colorspace::coords(as(rgb, "HSV"))

  start_ind = sample(1:nrow(hsv), 1)

  selection = matrix(, nrow = n, ncol = 3)
  selection[1,] = hsv[start_ind, ]

  for(i in 2:n){
    current_angle = selection[i - 1, 3] + angle
    # find all values that are closest to new angle
    # select the one that is the farthest distance
    # assign that row to slection[i, ]
    # repeat n times

  }
}
