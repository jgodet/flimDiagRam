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


getData <- function(pathTau, pathPhoton, label){

    tau1d <- read.table(pathTau,h=FALSE)
    tau1d <- as.vector(as.matrix(tau1d))
    photons <- read.table(pathPhoton,h=FALSE)
    photons <- as.vector(as.matrix(photons))
    
    dataD <- data.frame(tau1d, photons)
    dataD$ind <- label
    return(dataD)
  }
