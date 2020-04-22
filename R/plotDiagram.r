# plotDiagram.r
# written by JuG
# August 05 2019


#' Plot FLIM diagram
#' @author JuG
#' @description 
#' @param data dataframe of alpha and tau values
#' @details 
#' @examples 
#'
#'
#' @return 
#' @export


plotDiagram<- function(data, silence=FALSE, ...){
  if(!silence){print("Prepare plot...")}
  data <- data[data$tau1d!=20.,] 
  data <- data[(data$alpha>=2. & data$alpha<=99.),] 
  ## compute density on a grid 
  dens.alpha <- seq(-5,105,length.out=105)
  dens.tau1 <- seq(-.1,2.4,length.out=100)
  dens.grid <- expand.grid( alpha=dens.alpha, tau=dens.tau1)
  #dens.vals  <- smvkde(dens.grid, data=qsDup[,c("alpha","tau")])
  dens.val <- smvkde(dens.grid, data=data[,c("alpha","tau")])
  
  
  
  ## arrange density values into matrix for easy plotting
  #dens.mtrx <- matrix(dens.vals, 105, 100)
  dens.mtrx <- matrix(dens.val, 105, 100)
  
  par(mar=c(8,8,4,4))
  contour(x=dens.alpha, y=dens.tau1,nlevels=10, z=dens.mtrx, 
          axes=T, las=1, cex.lab=3, cex.axis=2.5, mgp=c(4,1.5,0),
          col=rgb(255/255,3/255,0/255,10:1/10),#red
          drawlabels = FALSE,
          xlab=expression(paste(alpha [1],"  %")),
          ylab=expression(tau [1]),lwd=2,
          ylim=c(0,2.5), xlim=c(0,115),...)
  points(data[,c("alpha","tau")], pch='.', col=rgb(.2,.2,.2,.5),...)
  
  for (i in c(.5,.8,1.1,1.4,1.7,2,2.2,2.3,2.4,2.5)){
    alphax <- 0:100
    tauMean = i
    tau1y <- 2.400 - (2.400 - tauMean) / (alphax/100)
    lines(alphax, tau1y, lty=2, col='blue')
    alphax = 102
    tau1y <- 2.400 - (2.400 - tauMean) / (alphax/100)
    if(i%in% c(.5,.8,1.1,1.4,1.7,2,2.2,2.4)){
      text(x = alphax, y = tau1y, paste(tauMean, "ns", sep=" "), col="blue", cex= 1.5,adj = 0)
    }
  }
  if(!silence){print("Job done...")}
  return()
}
