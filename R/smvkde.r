# smvkde.r
# written by JuG
# July 25 2019


#' Compute KDE at (matrix) x given data
#' @author matt.shotwell
#' @description Published by  <matt.shotwell@vanderbilt.edu> https://biostatmatt.com/archives/2745
#' @param 
#' @details 
#' @examples 
#'
#'
#' @return 
#' @export


## 
smvkde <- function(x, ...)
  apply(x, 1, mvkde, ...)