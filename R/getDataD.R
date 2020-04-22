# getData.r
# written by JuG
# December 12 2019


#' Do something
#' @author JuG
#' @description 
#' @param 
#' @details 
#' @examples 
#'
#'
#' @return 
#' @export


getDataD <- function(pathEGFP,label, ref= 2200){
  alpha1d <- read.table(paste(pathEGFP,"_a1[%].asc",sep=''),h=FALSE)
  alpha1d <- as.vector(as.matrix(alpha1d))
  alpha2d <- read.table(paste(pathEGFP,"_a2[%].asc",sep=''),h=FALSE)
  alpha2d <- as.vector(as.matrix(alpha2d))
  tau1d <- read.table(paste(pathEGFP,"_t1.asc",sep=''),h=FALSE)
  tau1d <- as.vector(as.matrix(tau1d))
  #photons <- read.table(paste(pathEGFP,"_photons.asc",sep=''),h=FALSE)
  #photons <- as.vector(as.matrix(photons))
  
  dataD <- data.frame(tau1d,alpha1d,alpha2d)#, photons)
  dataD <- dataD[ tau1d!=0,]
  dataD <- dataD[dataD$tau1d!=20.,] 
  dataD <- dataD[(dataD$alpha>=2. & dataD$alpha<=99.),] 
  
  dataD$tauMean <- ifelse(dataD$tau1d==20, ref, dataD$tau1d * dataD$alpha1d/100 + ref * dataD$alpha2d/100)
  dataD$alpha <- dataD$alpha1d
  dataD$alpha[dataD$alpha1d<5] <-  rnorm(sum(dataD$alpha1d<5),0,3)
  dataD$tau <- dataD$tau1d
  dataD$tau[dataD$alpha1d<5] <-  rnorm(sum(dataD$alpha1d<5),ref, 20)
  dataD$tau <- dataD$tau/1000
  dataD$tauMeanForm <- (ref - dataD$alpha1d * (ref - ifelse(dataD$tau1d==20, ref, dataD$tau1d))/100)/1000
  dataD$ind <- label
  return(dataD)
}