
require(tidyverse)

#correct files
tau1f <- read.table(paste(getwd(),"/data/PAS407_001_t1b.asc",sep=''),h=FALSE)
tau1fplus <- as.matrix(tau1f)
tau1fplus[110:120, 40:50]
tau1fplus[tau1fplus>10] <- tau1fplus[tau1fplus>10]+70
write.table(x = tau1fplus, file = paste(getwd(),"/data/PAS407_001_t1b.asc",sep=''),row.names = F,col.names = F)


tau1f <- as.vector(as.matrix(tau1f))
tau1f <- tau1f[tau1f>10]
summary(tau1f)
summary(as.vector(tau1fplus[tau1fplus>10]))

tau1d <- read.table(paste(getwd(),"/data/PAS406_001_t1.asc",sep=''),h=FALSE)
tau1d <- as.vector(as.matrix(tau1d))
tau1d <- tau1d[tau1d>10]
summary(tau1d)


plotEcdf(AA=as.vector(tau1fplus[tau1fplus>10]),BB=tau1d )
boxplot(tau1f, tau1d)


lf <- list.files(paste(paste(getwd(),"/data/",sep="")))
ind <- str_detect(string=lf, pattern = c("PAS.*photons.asc"))

listing <- lf[ind]

for (i in listing){
  tau1f <- read.table(paste(getwd(),"/Data/", i,sep=''),h=FALSE)
  tau1f <- as.vector(as.matrix(tau1f))
  print(i)
  print(summary(tau1f))
  print(sum(tau1f))
}


tau1f <- read.table(paste(getwd(),"/Data/", listing[3],sep=''),h=FALSE)
tau1f <- as.matrix(tau1f)

tau1f[245:255, 200:230]
addSome <- function(tau1f, nfois = 1){
  tautemp <- tau1f
  if(nfois<=1){
    return("nfois < 1")
  }
    aj <- matrix(data = rpois(n = dim(tau1f)[1]*dim(tau1f)[2], lambda = tau1f), ncol=256)
    tautemp <- tautemp + aj*nfois
  return(tautemp)
}

a <- addSome(tau1f, 10)
image(tau1f)
summary(as.vector(tau1f))
image(addSame(a))
summary(as.vector(a))





