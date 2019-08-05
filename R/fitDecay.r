# fitDecay.r
# written by JuG
# July 25 2019


#' Do something
#' @author JuG
#' @description 
#' @param matDecayArray matrix of the decays
#' @param Na = 50, 
#' @param start channel at which the decay starts 
#' @param A amplitude of the decay 
#' @param nchannels Number of channels (default = 1024)

#' @details 
#' @examples 
#'
#'fitDecay(matDecayArray = decay)
#' @return 
#' @export


fitDecay<- function(matDecayArray, Na = 50, start = 100, A = 1000, nchannels = 1024){
  fitRes <- array(data = NA,dim = c(Na,100,3))
  r2<- numeric()
  
  for (i in 1:dim(fitRes)[1]){
    for(k in 1:dim(fitRes)[2]){
      #declin <- apply(X = matDecay[,sample(x = 1:1000, size = 10,replace = T)],MARGIN = 1,FUN = mean)[105:1024]
      # offsetEst <- median(matDecay[,i][10:80])
      # declin <- matDecay[,i][100:1024]-offsetEst#sample(1:1000, size = 1)][105:1024] 
      offsetEst <- median(matDecayArray[i,k,][10:80])
      declin <- matDecayArray[i,k,][start:nchannels]-offsetEst#sample(1:1000, size = 1)][105:1024]   
      #declin <- (noise2 + counts2)[105:1024]
      x <- (1:(nchannels-start+1))*.0125
      dtf <- data.frame(x,declin)
      mod <- NULL
      try(mod <- nls( declin ~  (A-B) * exp(-x/tau) + B * exp(-x/b) ,
                      #  nls( declin ~  (1000-B) * exp(-x/2.4) + B * exp(-x/b),
                      start = list(B=700, b=1.9), data=dtf,
                      algorithm = "port",
                      lower = list(B=5, b = .2),
                      upper = list(B=A*.99, b = 2.4)),silent=T)
      if(!is.null(mod)){
        fitRes[i,k,1:2] <- coefficients(mod) ######################HERERERERERE
        fitRes[i,k,3] <- with(dtf[5:600,], 1 - sum(resid(mod)^2)/sum((declin - mean(declin))^2))
      }
    }
  }
  
  return(fitRes)
}
