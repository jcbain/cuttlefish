#' Create a Color Palette of n Colors from an Image
#'
#' Creates a color palette of n distinct colors from a provided image.
#' @param img Path to image file.
#' @param n `n` distinct colors.
#' @param prominent.ord Option to choose prominent colors instead of segmenting
#'     the colors from a random starting color.
#' @param ... Additional arguments passed to `cuttlefish::find_segmented`.
#' @return Vector of distinct hex colors.
#' @export
create_palette <- function(img, n, prominent.ord=FALSE, ...){
  img_colors = extract_colors(img)
  if(prominent.ord){
    colors <- cuttlefish::find_prominent(img_colors, n)
  } else {
    colors <- cuttlefish::find_segmented(img_colors, n, ...)
  }
  colors
}
