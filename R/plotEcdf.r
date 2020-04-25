# plotEcdf.r
# written by JuG
# April 23 2020


#' Plot empirical cumulative distribution functions
#' @author JuG
#' @description Plot empirical cumulative distribution functions
#' @param AA dataset a
#' @param BB dataset b
#' @param col1 color 1
#' @param col2 color 2
#' @param CI boolean plot confidence interval
#' @details 
#' Dvoretzky–Kiefer–Wolfowitz inequality:
#' P ( sup|F_n - F| > epsilon  ) \leq 2*exp(-2n*epsilon^2)
#' set alpha to 0.05 and alpha=2*exp(-2n*epsilon^2):
#' --> epsilon_n = \sqrt(-log(0.5*0.05)/(2*n))
#' @examples 
#'
#'
#' @return 
#' @export


plotEcdf <- function(AA,BB,col1, col2,CI=FALSE, ...){
  if(missing(col1)){
    col1 = rgb(.153,.392,.482)
  }
  if(missing(col2)){
    col2 = rgb(.792,.208,.196)
  }
  xy <- ecdf(AA)
  xx <- get("x", envir=environment(xy))# = sort(x)
  yy <- get("y", envir=environment(xy))
  n = length(yy)
  plot(xx,yy,col=col1,lwd=2,type='l',...)
  xy1 <- ecdf(BB)
  n1 = length(xy1)
  xx1 <- get("x", envir=environment(xy1))# = sort(x)
  yy1 <- get("y", envir=environment(xy1))
  lines(xx1,yy1,col=col2,lwd=2,...)

  if(CI==TRUE){
      #lower and upper bands:
      epsilon_i = sqrt(log(2/0.05)/(2*n))
      L=pmax(1:n/n-epsilon_i, 0)
      U=pmin(1:n/n+epsilon_i, 1)
      polygon(x = c(xx,rev(xx)), y = c(U, rev(L)),col = paste(col1,"80", sep=''), border = NA)

      epsilon_i = sqrt(log(2/0.05)/(2*n1))
      L1=pmax(1:n1/n1-epsilon_i, 0)
      U1=pmin(1:n1/n1+epsilon_i, 1)
      polygon(x = c(xx1,rev(xx1)), y = c(U1, rev(L1)),col = paste(col2,"80", sep=''), border = NA)
  }
}
