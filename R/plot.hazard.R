`plot.hazard` <-
function(x, group = TRUE, xlim = NA, ylim = NA, xlab = "Survival Time", ylab = "Hazard Function", 
         main = NA, leglab = NA, type = "l", lty = 1, lwd = 1, col = NA, legpos = "topleft", ...)

{

#x...object of class "mws"
#maxtime...maximum dwell time on x-axis
#plotg...plot hazards for different groups
#plotp...plot hazards for different variables (pages)
#performs plots of the hazard rates and of the survival functions


scale <- 1/x$scale						       #different parameterization
K     <- dim(scale)[1]
pages <- dim(scale)[2]

if (any(is.na(col))) col <- 1:max(pages)
if (any(is.na(xlim))) xlim <- c(0,mean(x$clmean))

maxtime <- round(xlim[2],0)
Ti <- maxtime                        #upper limit for sojourn time on x-axis
hazgroup <- array(NA,c(Ti,pages,K))  #init plot array
#if (is.na(legx)) legx <- Ti/10
if ((is.na(leglab)) && (group)) leglab <- "Variable"
if ((is.na(leglab)) && (!group)) leglab <- "Group"
if ((is.na(main)) && (group)) main <- "Hazard Function for Group"
if ((is.na(main)) && (!group)) main <- "Hazard Function for Variable"

if (group) {                           #1 plot for each group
  for (k in 1:K) {                     #Compute hazards for each page in group k
    for (j in 1:pages) {
      hazgroup[,j,k] <- x$shape[k,j]*scale[k,j]*((scale[k,j]*(1:Ti))^(x$shape[k,j]-1)) 	
   }}
} else {                               #1 plot for each page
for (j in 1:pages) {
  for (k in 1:K) {
    hazgroup[,j,k] <- scale[k,j]*x$shape[k,j]*((scale[k,j]*(1:Ti))^(x$shape[k,j]-1))	#Hazardfunktionen für alle Gruppen auf Seite j
   }}
}

if (any(is.na(ylim))) ylim <- c(0,max(hazgroup))
#if (is.na(legy)) legy <- ylim[2]

if (group) {
  for (k in 1:K) {
    if (((k-1) %% 3) == 0) {                #open new device (3 plots on each device)
      get(getOption("device"))()
      par(mfrow=c(1,3))
    }
    matplot((1:Ti),hazgroup[,,k],type=type,xlab=xlab,ylab=ylab,main=paste(c(main),k),lty=lty,lwd=lwd,ylim=ylim,...)
    legend(legpos, legend = paste(c(leglab),1:pages), col=col,lty=lty,lwd=lwd,...)
    }
} else { 
   for (j in 1:pages) {
    if (((j-1) %% 3) == 0) {                #open new device (3 plots on each device)
      get(getOption("device"))()
      par(mfrow=c(1,3))
    } 
    matplot((1:Ti),hazgroup[,j,],type=type,xlab=xlab,ylab=ylab,main=paste(c(main),j),lty=lty,lwd=lwd,ylim=ylim,...)
    legend(legpos, legend = paste(c(leglab),1:K), col=col,lty=lty,lwd=lwd,...)
  } 
}

}
