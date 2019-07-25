# mvkde.r
# written by JuG
# July 25 2019


#' Compute KDE at x given data
#' @author matt.shotwell
#' @description Published by  <matt.shotwell@vanderbilt.edu> https://biostatmatt.com/archives/2745
#' @examples 
#' @param 
#' @details 
#'
#'
#' @return 
#' @export


mvkde <- function(x, data, bandwidth=Scott, kernel=RadSym) {
  # bandwidth may be a function or matrix
  if(is.function(bandwidth))
    bandwidth <- bandwidth(data)
  u <- t(solve(bandwidth, t(data) - x))
  return(mean(kernel(u)))
}