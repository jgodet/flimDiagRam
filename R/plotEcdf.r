# plotEcdf.r
# written by JuG
# April 23 2020


#' Plot empirical cumulative distribution functions
#' @author JuG
#' @description Plot empirical cumulative distribution functions
#' @param AA dataset a
#' @param BB dataset b
#' @details 
#' 
#' @examples 
#'
#'
#' @return 
#' @export


plotEcdf <- function(AA,BB,col1, col2,...){
  if(missing(col1)){
    col1 = rgb(.153,.392,.482)
  }
  if(missing(col2)){
    col2 = rgb(.792,.208,.196)
  }
  xy <- ecdf(AA)
  xx <- get("x", envir=environment(xy))# = sort(x)
  yy <- get("y", envir=environment(xy))
  plot(yy,xx,col=col1,lwd=2,type='l',xaxp=c(0,1,2),...)
  xy <- ecdf(BB)
  xx <- get("x", envir=environment(xy))# = sort(x)
  yy <- get("y", envir=environment(xy))
  lines(yy,xx,col=col2,lwd=2,...)
}