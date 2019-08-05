# figs1panelA.r
# written by JuG
# August 05 2019


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


figs1panelA<- function(){
  
  nchannels = 1024
  #65.383045
  print("Generate decays...")
  decay <- simMatDecay(nAcceptor = 1,nAcceptorFix=TRUE, fraction = "progressive", 
                       distAcc = 65.383045, sdDistAcc = 0,bindingProbs = 1,Na = 200)
  print("Fit decays...")
  fittedDecay <- fitDecay(matDecayArray = decay,Na = 200)
  
  #fitRes <- matrix(NA, ncol=3, nrow=100*200)

  groundTruth <-  array(data = NA,dim = c(dim(fittedDecay)[1],dim(fittedDecay)[2]))
  
  for (i in 1:dim(fittedDecay)[1]){
    for(k in 1:dim(fittedDecay)[2]){
      groundTruth[i,k] <- k*10
    }
  }
  
  
  fitResLOCF <- fittedDecay[,,1]
  for (i in 1:dim(fitResLOCF)[1]){
    for (j in 1:dim(fitResLOCF)[2]){
      
      if(is.na(fitResLOCF[i,j] )){
        fitResLOCF[i,j] <- median(fittedDecay[,j,1],na.rm=T)
      }
      if(fitResLOCF[i,j] >= 990){
        fitResLOCF[i,j] <- median(fittedDecay[,j,1],na.rm=T)
      }
    }
  }
  
  par(mfrow=c(1,2), mar=c(2,4,2,1))
  image(groundTruth[,], col = heat.colors(60),axes=F,useRaster = TRUE); box()
  axis(2,las=1,cex.axis=1.5)

  image(fitResLOCF[,], col = heat.colors(60),axes=F,useRaster = TRUE); box()
  axis(2,las=1,cex.axis=1.5)

  return()
}