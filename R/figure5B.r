#code Figure 5F PvdA/NRPS
# Jug
# mis au propore le "Mon Oct 01 17:27:50 2018"


 # from http://biostatmatt.com/archives/2745

# radially symmetric kernel (Gussian kernel)
RadSym <- function(u)
  exp(-rowSums(u^2)/2) / (2*pi)^(ncol(u)/2)

## multivariate extension of Scott's bandwidth rule
Scott <- function(data)
  t(chol(cov(data))) * nrow(data) ^ (-1/(ncol(data)+4))

## compute KDE at x given data
mvkde <- function(x, data, bandwidth=Scott, kernel=RadSym) {
  # bandwidth may be a function or matrix
  if(is.function(bandwidth))
    bandwidth <- bandwidth(data)
  u <- t(solve(bandwidth, t(data) - x))
  mean(kernel(u))
}

## compute KDE at (matrix) x given data
smvkde <- function(x, ...)
  apply(x, 1, mvkde, ...)


getallPath <- function(folder){
  flist <- list.files(folder)
  pathEGFPList <- flist[grep(flist, pattern=".img")]
  pathEGFPList <- gsub(pathEGFPList,pattern=".img",replacement="")
  return(pathEGFPList)
  }




###
getDataD <- function(pathEGFP,label){
  alpha1d <- read.table(paste(pathEGFP,"_a1[%].asc",sep=''),h=FALSE)
  alpha1d <- as.vector(as.matrix(alpha1d))
  alpha2d <- read.table(paste(pathEGFP,"_a2[%].asc",sep=''),h=FALSE)
  alpha2d <- as.vector(as.matrix(alpha2d))
  tau1d <- read.table(paste(pathEGFP,"_t1.asc",sep=''),h=FALSE)
  tau1d <- as.vector(as.matrix(tau1d))

  dataD <- data.frame(tau1d,alpha1d,alpha2d)
  dataD <- dataD[ tau1d!=0,]
  dataD$tauMean <- ifelse(dataD$tau1d==20, 2300, dataD$tau1d * dataD$alpha1d/100 + 2300 * dataD$alpha2d/100)
  dataD$alpha <- dataD$alpha1d
  dataD$alpha[dataD$alpha1d<5] <-  rnorm(sum(dataD$alpha1d<5),0,3)
  dataD$tau <- dataD$tau1d
  dataD$tau[dataD$alpha1d<5] <-  rnorm(sum(dataD$alpha1d<5),2300, 20)
  dataD$tau <- dataD$tau/1000
  dataD$ind <- label

  return(dataD)
}


getDataF <- function(pathFRET,label){
  alpha1f <- read.table(paste(pathFRET,"_a1[%].asc",sep=''),h=FALSE)
  alpha1f <- as.vector(as.matrix(alpha1f))
  alpha2f <- read.table(paste(pathFRET,"_a2[%].asc",sep=''),h=FALSE)
  alpha2f <- as.vector(as.matrix(alpha2f))
  tau1f <- read.table(paste(pathFRET,"_t1.asc",sep=''),h=FALSE)
  tau1f <- as.vector(as.matrix(tau1f))

  dataF <- data.frame(tau1f,alpha1f,alpha2f)
  dataF <- dataF[ tau1f!=0,]
  dataF$tauMean <- ifelse(dataF$tau1f==20, 2300, dataF$tau1f * dataF$alpha1f/100 + 2300 * dataF$alpha2f/100)
  dataF$alpha <- dataF$alpha1f
  dataF$alpha[dataF$alpha1f<5] <-  rnorm(sum(dataF$alpha1f<5),0,3)
  dataF$tau <- dataF$tau1f
  dataF$tau[dataF$alpha1f<5] <-  rnorm(sum(dataF$alpha1f<5),2300, 20 )
  dataF$tau <- dataF$tau/1000
  dataF$ind <- label
  return(dataF)
}

#406 vs 407

qsD <- numeric()
qsF <- numeric()
j <- k <- 0
for (i in 2:5){
  j = j+1
  aa <- getDataD(paste("C:/FlimAllRaw/406/20170302_00",i,sep=""),label=j)
  bb <- getDataF(paste("C:/FlimAllRaw/407/20170302_00",i,sep=""),label=j)
  qsD <- rbind(qsD,aa)
  qsF <- rbind(qsF,bb)
}
for (i in 1:5){
  j = j+1
  aa <- getDataD(paste("C:/FlimAllRaw/406/20170316_00",i,sep=""),label=j)
  bb <- getDataF(paste("C:/FlimAllRaw/407/20170316_00",i,sep=""),label=j)
  qsD <- rbind(qsD,aa)
  qsF <- rbind(qsF,bb)
}
for (i in 1:6){
  j = j+1
  aa <- getDataD(paste("C:/FlimAllRaw/406/20170324_00",i,sep=""),label=j)
  bb <- getDataF(paste("C:/FlimAllRaw/407/20170324_00",i,sep=""),label=j)
  qsD <- rbind(qsD,aa)
  qsF <- rbind(qsF,bb)
}

#for (i in 2:6){
#  j = j+1
#  aa <- getDataD(paste("C:/FlimAllRaw/406/20170421_00",i,sep=""),label=j)
#  bb <- getDataF(paste("C:/FlimAllRaw/407/20170421_00",i,sep=""),label=j)
#  qsD <- rbind(qsD,aa)
#  qsF <- rbind(qsF,bb)
#}


dim(qsD)
 dim(qsF)
 par(mfrow=c(2,1))
 boxplot(tau~ind, data=qsD, ylim=c(2,2.5)) ; abline(h=2.3) ; abline(h=2.2)
 boxplot(tau~ind, data=qsF, ylim=c(2,2.5)); abline(h=2.3)  ; abline(h=2.2)

#tau1 <- read.table("F:/Morgane/FlimAllRaw/407/20170316_001_t1.asc",h=FALSE)

x11()


#qsD <- qsD[qsD$alpha > 1 & qsD$alpha < 99 & qsD$tau >0.5 ,]
#qsF <- qsF[qsF$alpha > 1 & qsF$alpha < 99 & qsF$tau >0.5 ,]

qsDup <- qsD[qsD$alpha > 1 & qsD$alpha < 99 & qsD$tau1d != 20.00 ,]
qsFup <- qsF[qsF$alpha > 1 & qsF$alpha < 99 & qsF$tau1f != 20.00 ,]


## compute density on a grid of Ozone and Solar.R values
dens.alpha <- seq(-5,100,length.out=105)
dens.tau1 <- seq(0.5,2.4,length.out=100)
dens.grid <- expand.grid( alpha=dens.alpha, tau=dens.tau1)
dens.vals  <- smvkde(dens.grid, data=qsDup[,c("alpha","tau")])
dens.vals2 <- smvkde(dens.grid, data=qsFup[,c("alpha","tau")])

## arrange density values into matrix for easy plotting
dens.mtrx <- matrix(dens.vals, 105, 100)
dens.mtrx2 <- matrix(dens.vals2, 105, 100)

setwd("E:/Seafile/Ma bibliothèque/Papiers/FlimNRPS/draftFigures/draftFigure5")
getwd()
pdf("PvdL406407.pdf")
par(mar=c(6,6,4,4))
contour(x=dens.alpha, y=dens.tau1,nlevels=3, z=dens.mtrx, axes=T, las=1, cex.lab=2, col=rgb(66/255,69/255,244/255,1:10/10),drawlabels = FALSE,
        xlab=expression(paste(alpha [1],"  %")), ylab=expression(tau [1]),lwd=2, ylim=c(1,2.4), xlim=c(-5,120))

 for (i in c(seq(1.700, 2.400, by=.1), 2.250, 2.350)){
   alphax <- 0:100
   tauMean = i
   tau1y <- 2.300 - (2.300 - tauMean) / (alphax/100)
   lines(alphax, tau1y, lty=2, col='blue')
   alphax = 105
   tau1y <- 2.300 - (2.300 - tauMean) / (alphax/100)
   text(x = alphax, y = tau1y, paste(tauMean, "ns", sep=" "), col="blue", cex= .65,adj = 0)
 }

with(qsDup[sample(size =2000,1:dim(qsD)[1],replace=T),],points(alpha, tau, pch=20, cex=.25,col=rgb(66/255,69/255,244/255,.3)))
with(qsFup[sample(size =2000,1:dim(qsD)[1],replace=T),],points(alpha, tau, pch=20, cex=.25,col=rgb(255/255,165/255,0/255,1)))
#points(qsF$alpha, qsF$tau, pch=20, cex=.25, col=rgb(1,0,0,.2))
contour(x=dens.alpha, y=dens.tau1, z=dens.mtrx, nlevels=4, col=rgb(66/255,69/255,244/255,1:10/10),drawlabels = FALSE,add=T, lwd=2)
contour(x=dens.alpha, y=dens.tau1, z=dens.mtrx2, nlevels=5, add=TRUE, drawlabels = FALSE,col=rgb(255/255,165/255,0/255,seq(.5,1,by=.15)),lwd=2)
dev.off()

####################
# inset boxplot
unDeuxDonly <- as.factor(qsD$tau1d==20.00 | qsD$tau1d >= 2050)
aggregate(unDeuxDonly ~as.factor(qsD$ind), FUN=table)
aggregate(unDeuxDonly ~as.factor(qsD$ind), FUN=length)
pctDo <- aggregate(unDeuxDonly ~as.factor(qsD$ind), FUN=table)[,2][,2]/aggregate(unDeuxDonly ~qsD$ind, FUN=length)[,2]

unDeuxDA <- qsF$tau1f==20.00 | qsF$tau1f >= 2200
aggregate(unDeuxDA ~as.factor(qsF$ind), FUN=table)
aggregate(unDeuxDA ~as.factor(qsF$ind), FUN=length)
pctDA <- aggregate(unDeuxDA ~as.factor(qsF$ind), FUN=table)[,2][,2]/aggregate(unDeuxDA ~qsF$ind, FUN=length)[,2]

fr <- data.frame(fraction = c(pctDo,pctDA), condition = c(rep("D",15), rep("D-A",15)))
fr$condition <- relevel(fr$condition, ref="D")
par(mar=c(10,10,6,6))
boxplot(fraction~condition,data=fr,notch = F,ylim=c(0,1),
col=c(rgb(66/255,69/255,244/255),rgb(255/255,165/255,0)),
las=1,cex.lab=2,cex.axis=2, ylab="")





####################
# marginal dist
df <- data.frame(valtau1 =c(qsD$tau1d[qsD$tau1d!=20.00],qsF$tau1f[qsF$tau1f!=20.00]),
                 condition= c(rep("D-only",length(qsD$tau1d[qsD$tau1d!=20.00])),
                  rep("D-A",length(qsF$tau1f[qsF$tau1f!=20.00]))))

hd <- hist(df$valtau1[df$condition=="D-only"],breaks = seq(0,2300,by=50),
col=rgb(.153,.392,.482,.5), probability=T,
xlim=c(1200, 2300))
hda <- hist(df$valtau1[df$condition=="D-A"],breaks = seq(0,2300,by=50),add=T,
probability=T,
col=rgb(1,0,0,.5)
)

barplot(hda$counts[20:46],horiz=T, axes=F, col=rgb(255/255,165/255,0,.5),xlim=c(0,20000),space=0)
barplot(hd$counts[20:46],horiz=T, axes=F,add=T,col=rgb(66/255,69/255,244/255,.5),space=0)
box()


