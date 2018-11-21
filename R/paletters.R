#' Create a Color Palette of n Colors from an Image
#'
#' Creates a color palette of n distinct colors from a provided image.
#' @param img Path to image file.
#' @param n `n` distinct colors.
#' @param prominent.ord Option to choose prominent colors instead of segmenting
#'     the colors from a random starting color.
#' @param ... Additional arguments passed to \link[cuttlefish]{find_segmented}.
#' @return Vector of distinct hex colors.
#' @examples
#'   # return 20 semi-evenly distributed colors from some image
#'   create_palette('path/to/image.png', n = 20)
#'
#'   # return the 10 most prominent colors from some image
#'   create_palette('path/to/image.jpg', n = 10, prominent.order = T)
#' @export
create_palette <- function(img, n, prominent.ord=FALSE, hue=NA, ...){
  img_colors <- extract_colors(img)
  if (!is.na(hue)){
    img_colors <- select_hue(img_colors, hue)
  }
  if(prominent.ord){
    colors <- cuttlefish::find_prominent(img_colors, n)
  } else {
    colors <- cuttlefish::find_segmented(img_colors, n, ...)
  }
  colors
}
