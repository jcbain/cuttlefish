'%ni%' <- Negate('%in%')

select_hue <- function(hexes, hue){
  if (hue %ni% c('red', 'blue')){
    stop(paste(hue, "is not a selectable hue."))
  }
}
