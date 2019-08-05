# RadSym.r
# written by JuG
# July 25 2019


#' Radially symmetric kernel (Gaussian kernel)
#' @author <jgodet@unistra.fr> (adapted from matt.shotwell)
#' @description Published by  <matt.shotwell@vanderbilt.edu> https://biostatmatt.com/archives/2745
#' @details 
#' @examples 
#'
#' @return 
#' @export

RadSym <- function(u){
  return(exp(-rowSums(u^2)/2) / (2*pi)^(ncol(u)/2))
}
  