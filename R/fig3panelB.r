# fig3panelB.r
# written by JuG
# August 05 2019


#' Do something
#' @author JuG
#' @description 
#' @param 
#' @details 
#' @examples 
#'fig3panelB()
#'
#' @return 
#' @export


fig3panelB<- function(){
  print("Generate decays...")
  matDecayArray <- simMatDecay(nAcceptor = 3, distAcc = c(45,55,70), 
                               sdDistAcc = c(1,2,5), 
                               bindingProbs = c(.1,.1,.80))
  
  print("Fit decays...")
  #fitRes <- matrix(NA, ncol=3, nrow=100*200)
  fitRes <- fitDecay(matDecayArray = matDecayArray)
  

  qsF <- data.frame(alpha = as.vector(fitRes[,,1]/10), 
                    tau = as.vector(fitRes[,,2]))
  
  qsF <- qsF %>% filter(qsF$alpha>1 &  qsF$alpha<99) %>%  na.omit()

  plotDiagram(qsF)
  
  return()
}
