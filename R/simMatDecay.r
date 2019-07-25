# simMatDecay.r
# written by JuG
# July 25 2019


#' Do something
#' @author JuG
#' @description 
#' @param 
#' @details 
#' @examples 
#' decay <- simMatDecay(nAcceptor = 2,distAcc = c(20, 50))
#' dims <- dim(decay)
#' time <- ((1:dims[3])- 100 ) * .0125 
#' counts <- apply(decay[1,,],2,median)
#' plot(time, counts,ylim=c(1,1000),xlim=c(0,12), type='l', log="y", las=1 )
#' @return 
#' @export


simMatDecay<- function(nAcceptor = 3, distAcc = c(40, 50, 60),sdDistAcc = c(3,1.5,1.5), bindingProbs = c(.15,.3,.65),R0 = 50, Na = 50, start = 100, A = 1000, nchannels = 1024, tau = 2.4, lambdaNoise = 2){

  channels = 1:nchannels
  
  
  
  matDecayArray <- array(data = NA,dim = c(Na,100,1024))
  matDecay <- matrix(NA, nrow = 1024,ncol = 200)
  nFRET <- numeric()
  nAcc <- numeric()
  alpha <- numeric()
  tauda <- numeric()
  
  for (i in 1:Na){
    for (k in 1:100){
      #1#nAcc[i] <- nacc <- round(runif(n=1, min = 0, max= 15))
      nAcc[i] <- nacc <- round(runif(n=1, min = 0, max= nAcceptor))
      
      #R <- runif(n = nacc,min = 30,max = 150)
      #1# R <- sample(c(rnorm(1,40,1),rnorm(1,50,1),120,120,rnorm(1,60,1),seq(90,200,length.out = 19)),size = 24,replace = F, prob  = c(rep(0.004,4),.9,rep(0.004,19)))
      
      
      R <- sample(c(rnorm(3,mean = distAcc, sd = sdDistAcc )),size = nacc,replace = T, prob  = bindingProbs)
      
      
      if(nacc==0){
        R <- 1000
      }else{
        R <- R[1:nacc] 
      }
      #R <- sample(c(50,60,62,65,70),size = nacc,replace = F, prob  = c(.20,.20,.20,.20,.20))
      #R <- 65.383045
      #if(nacc==0){R <- 1000}
      #nFRET[i] <- sum(R<100)
      ki = 1/tau * (R0/R)**6
      tauDA = (1/tau + sum(ki))**-1
      #tauDA
      tauda[i] <- tauDA
      counts3 <- NA
      counts3[1:start] <- 0
      #B <- rbeta(n = 1,shape1 = round(11-(nacc)/5),shape2 = round(2+nacc/5)) * 1000
      B <- (1- .5*rbeta(n = 1,shape1 = max(0,(4*nAcceptor-4*(2.5-tauDA))/2), shape2 =max(0,(4*(2.5-tauDA))/2)))* 1000
      #B <- runif(n = 1, min = 200, max = 800)
      #B <- k*10 
      # B <- round(rnorm(n = 1,mean = nacc / 5, sd=.005)*600)+200
      B <- ifelse(B<A*.98, B, A * .98)
      B <- ifelse(B<A * .02, A * .02,B)
      counts3[start:1024] <-rpois(925, (A-B) * exp(-channels[1:925]*.0125/tau) + B * exp(-channels[1:925]*.0125/tauDA))
      #counts3[start:1024] <-(A-B) * exp(-channels[1:925]*.0125/tau) + B * exp(-channels[1:925]*.0125/tauDA)
      #counts3[(start-5): (start+5)] <- rpois(11,A * exp(-(seq(8,0,length=11))))
      noise3 <- rpois(n = 1024, lambda = lambdaNoise)
      decay <- ( noise3 + counts3 )
      #matDecay[,i] <- ( noise3 + counts3 )
      #alpha[i] <- B/A
      # if(i%%10 == 0) {lines(((channels-100)*.0125), noise3 + counts3, col=rgb(1,0,0,.2))}
      matDecayArray[i,k,] <- decay
    }
  }
  
  return(matDecayArray)
}
