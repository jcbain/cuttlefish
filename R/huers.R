'%ni%' <- Negate('%in%')

hues <- list('red' = c(0, 60), 'yellow' = c(60, 120), 'green' = c(120, 180),
             'cyan' = c(180, 240), 'blue' = c(240, 300), 'magenta' = c(300, 360))

#' Subset hex colors by specific hues
#'
#' @export
select_hue <- function(hexes, hue){
  if (hue %ni% c('red', 'yellow', 'green', 'cyan', 'blue', 'magenta')){
    stop(paste(hue, "is not a selectable hue."))
  }
  degrees = get(hue, hues)

  tmp_rgb = t(col2rgb(hexes))
  tmp_rgb = apply(X = tmp_rgb, MARGIN = 2, FUN = as.numeric)

  # convert to matrix to RBG then to HSV
  rgbmat = colorspace::RGB(tmp_rgb)
  tmp_hsv = colorspace::coords(as(rgbmat, "HSV"))

  tmp_hsv = tmp_hsv[(tmp_hsv[,1] >= degrees[1] & tmp_hsv[,1] <= degrees[2]),]

  tmp_rgb = colorspace::coords(as(colorspace::HSV(as.matrix(tmp_hsv)), "RGB"))
  rgb(tmp_rgb, maxColorValue = 255)
}
