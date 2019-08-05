# Scott.r
# written by JuG
# July 25 2019


#' Multivariate extension of Scott's bandwidth rule
#' @author matt.shotwell
#' @description Published by  <matt.shotwell@vanderbilt.edu> https://biostatmatt.com/archives/2745
#' @examples 
#' @param 
#' @details 
#'
#'
#' @return 
#' @export

Scott <- function(data){
  return(  t(chol(cov(data))) * nrow(data) ^ (-1/(ncol(data)+4)))
}
 
