# getData.r
# written by JuG
# December 12 2019


#' Read single exponential decay data file
#' @author JuG
#' @description Read single exponential decay data file
#' @param pathTau path to the lifetime data file (*_t1.asc)
#' @param pathTau path to the lifetime data file (*_photons.asc)
#' @param label
#' @details 
#' @examples 
#'
#'
#' @return data frame 
#' @export


getData <- function(pathTau, pathPhoton, label, minPhotons){

    tau1d <- read.table(pathTau,h=FALSE)
    tauMat <- tau1d
    tau1d <- as.vector(as.matrix(tau1d))
    photons <- read.table(pathPhoton,h=FALSE)
    photMat <- photons
    photons <- as.vector(as.matrix(photons))

    
    dataD <- data.frame(tau1d, photons)
    dataD <- dataD[dataD$tau1d > 0 & dataD$photons>= minPhotons,]
    dataD <- cbind(dataD, which(tauMat>0 & photMat>= minPhotons, arr.ind = T))
    dataD$ind <- label

        
    return(dataD)
  }
