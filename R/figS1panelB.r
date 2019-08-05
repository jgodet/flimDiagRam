# figS1panelB.r
# written by JuG
# August 05 2019


#' Generate graph of figure S1 lower panel  
#' @author JuG
#' @description 
#' @param 
#' @details 
#' @examples 
#'figS1panelB()
#'
#' @return 
#' @export


figS1panelB<- function(){
  nchannels = 1024
  #65.383045
  print("Generate decays...")
  decay <- simMatDecay(nAcceptor = 1,nAcceptorFix=TRUE, fraction = "progressive", 
                       distAcc = 65.383045, sdDistAcc = 0,bindingProbs = 1)
  print("Fit decays...")
  fittedDecay <- fitDecay(matDecayArray = decay)
  if(!require(devtools)){install.packages('devtools')}
  library(devtools)
  if(!require(utilitR)){install_github("jgodet/utilitR")}
  require(utilitR)
  if(!require(colorRamps)){install.packages('colorRamps')}
  require(colorRamps)

  par(mar=c(6,6,6,4), mfrow=c(1,2))
  prop <- apply(fittedDecay[,,1],MARGIN = 2,FUN=median,na.rm=T)/10
  plot(1:100,prop, col=contColor(prop,palette = heat.colors(60)),cex.axis = 1.25,pch=21,
       bg='lightgrey',xlab="True alpha (%)", ylab="Fitted alpha (%)",las=1, cex.lab=1.25,cex=1.2)
  abline(a=c(0,1), col=rgb(.25,.25,.25,.5))
  box()
  
  
  plot(1:100,apply(fittedDecay[,,2],MARGIN = 2,FUN=median, na.rm=T),cex.axis = 1.25,
       ylim=c(0,2.4),ylab='Lifetime, ns',col=contColor(prop,palette = heat.colors(60)),
       pch=21,bg='lightgrey',xlab="True alpha (%)", las=1,cex=1.2,cex.lab=1.25)
  abline(h=2, col=rgb(.25,.25,.25,.5))
  box()
  
  return()
}
