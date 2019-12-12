# getData.r
# written by JuG
# December 12 2019


#' Import data
#' @author JuG
#' @description 
#' @param pathEGFP path to folder containing _a1[\%].asc, _photons.asc and _t1.asc files
#' @param label labdel string 
#' @details 
#' @examples 
#'
#'
#' @return 
#' @export


getDataHist <- function(pathEGFP,label){
  alpha1d <- read.table(paste(pathEGFP,"_a1[%].asc",sep=''),h=FALSE)
  alpha1d <- as.vector(as.matrix(alpha1d))
  photons <- read.table(paste(pathEGFP,"_photons.asc",sep=''),h=FALSE)
  photons <- as.vector(as.matrix(photons))
  tau1d <- read.table(paste(pathEGFP,"_t1.asc",sep=''),h=FALSE)
  tau1d <- as.vector(as.matrix(tau1d))
  
  dataD <- data.frame(tau1d,alpha1d,photons)
  dataD <- dataD[ alpha1d==100,]
  dataD$ind <- label
  
  return(dataD)
}