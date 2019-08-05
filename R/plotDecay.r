# plotDecay.r
# written by JuG
# August 05 2019


#' Graphical plot of an individual decay
#' @author JuG
#' @description 
#' @param decay photon counts of the decay
#' @param channels channels vector
#' @param timePerChannel time per channels (by default 0.0125ns = 12.5ps)
#' @param addPoints boolean add decay on plotted graph
#' @details 
#' @examples 
#' decay <- simMatDecay(nAcceptor = 1,nAcceptorFix=TRUE, fraction = "progressive", 
#' distAcc = 35.383045, sdDistAcc = 0,bindingProbs = 1)
#' plotDecay(decay = decay[1,80,],col='blue')
#' plotDecay(decay = decay[1,10,], add=TRUE)
#'
#' @return 
#' @export


plotDecay <- function(decay, channels, timePerChannel = .0125, fit,addPoints=FALSE,...){
  if(missing(channels)){
    nchannels <- length(decay)
  }
  time <- (1:nchannels)*timePerChannel
  if(!addPoints){
    plot(time, decay, log = 'y', xlab = "Time, ns", ylab= "Counts",...)
  }else{
    points(time, decay,...)
  }
  
}


