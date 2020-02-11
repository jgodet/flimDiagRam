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


getDataF <- function(pathFRET,label){
  alpha1f <- read.table(paste(pathFRET,"_a1[%].asc",sep=''),h=FALSE)
  alpha1f <- as.vector(as.matrix(alpha1f))
  alpha2f <- read.table(paste(pathFRET,"_a2[%].asc",sep=''),h=FALSE)
  alpha2f <- as.vector(as.matrix(alpha2f))
  tau1f <- read.table(paste(pathFRET,"_t1.asc",sep=''),h=FALSE)
  tau1f <- as.vector(as.matrix(tau1f))
#  photons <- read.table(paste(pathFRET,"_photons.asc",sep=''),h=FALSE)
#  photons <- as.vector(as.matrix(photons))
  
  dataF <- data.frame(tau1f,alpha1f,alpha2f)#, photons)
  dataF <- dataF[ tau1f!=0,]
  dataF$tauMean <- ifelse(dataF$tau1f==20, 2300, dataF$tau1f * dataF$alpha1f/100 + 2300 * dataF$alpha2f/100)
  dataF$alpha <- dataF$alpha1f
  dataF$alpha[dataF$alpha1f<5] <-  rnorm(sum(dataF$alpha1f<5),0,3)
  dataF$tau <- dataF$tau1f
  dataF$tau[dataF$alpha1f<5] <-  rnorm(sum(dataF$alpha1f<5),2300, 20 )
  dataF$tau <- dataF$tau/1000
  dataF$ind <- label
  return(dataF)
}