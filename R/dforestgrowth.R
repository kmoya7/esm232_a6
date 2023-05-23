#'  Forest growth model
#'  Forest size is measured in units of carbon (C)
#' @param time time since start
#' @param C forest size in carbon
#' @param parms - as list with three values, r, K, closure
#' @param r growth rate
#' @param K carrying capacity (kg C)
#' @param g linear growth rate after closure
#' @param closure canopy closure (kg C)
#' @return derivative of population with time

dforestgrowth= function(time, C, parms) {

  dC = ifelse(C < parms$closure, parms$r*C, parms$g*(1- (C/parms$K)))
  # if size of forest is under canopy closure, then r*C, if not g * (1 - C/K)
  dC = ifelse(C >= parms$K, 0, dC)

  return(list(dC))
}
