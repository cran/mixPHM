`Eclass` <-
function(x, old, K, method, Sdist,p)

{

shape <- matrix(NA, K, p)	           # K x p Matrix with Shape-Parameter
scale <- matrix(NA, K, p)            # K x p Matrix with Scale-Parameter

#check whether one-element cluster
if (any(as.vector(table(old))==1)) stop("Cluster contains only one observation! Cannot proceed with estimation! Please choose another starting solution or less components K!")


priorl <- by(x,old,function(y) {                                              #list of prior probabilities
                    y <- as.matrix(y)
                    nses <- length(y[,1])                                     #number of sessions in group
                    apply(y,2,function(z){ lz <- length(z[z>0])/nses})        #vector with prior probabilities
                    })
prior <- matrix(unlist(priorl),ncol=p,byrow=TRUE)                             #matrix of prior probabilities

#tapply(1:dim(x)[1],old, function(ind) {
#         y <- as.matrix(x[ind,])
#         ptot <- colSums(y)
#         if (any(ptot==0)) {
#           indp <- (1:length(ptot))[ptot==0] 
#           stop("Complete-0 dwell times in cluster during iteration in variable ",indp,". Estimation abandoned!")
#         }})

if (method=="separate") {  
   parlist <- tapply(1:dim(x)[1],old, function(ind) {
                         y <- as.matrix(x[ind,])
                         apply(y,2,function(z) {
                            wphm <- survreg(Surv(z[z>0])~1,dist=Sdist)        #wphm for each page within group
                            shapep <- 1/wphm$scale
                            scalep <- exp(wphm$coefficients[1])
                            list(scalep,shapep)
                            }) })
                            
   shsclist <- tapply(unlist(parlist),rep(1:2,length(unlist(parlist))/2),function(y){
                   matrix(y,nrow=K,byrow=TRUE)})                             #reorganizing parlist
   shape <- shsclist[[2]]                                                      #shape matrix K,p
   scale <- shsclist[[1]]                                                      #scale matrix K,p
   anzpar <- 2*K*p
}


if (method=="main.g") {
    for (i in 1:p) {
       datreg  <- as.vector(x[, i])			#VD-vektor für i-te Seite
       datreg  <- datreg[x[, i]>0]
       xold    <- old[x[, i]>0]				#Gruppenvektor für i-te Seite

       wphm <- survreg(Surv(datreg)~factor(xold),dist=Sdist)
       scalebase <- as.vector(wphm$coefficients[1])	#scale parameter group 1 (reference group)
       scalevec1 <- as.vector(exp(wphm$coefficients[2:K]+scalebase)) #scale parameter of the remaining groups
       scale [,i] <- c(exp(scalebase),scalevec1)
       shape [,i] <- 1/wphm$scale			#shape über gruppen konstant
   }
anzpar <- K*p+p
}


if (method=="main.p") {
    for (j in 1:K)  {
       datregmat <- as.matrix(x[old == j,])
       nsess <- dim(datregmat)[1]				#sessionanzahl in gruppe j
       pagevek <- rep(1:p,rep(nsess,p))				#Seitenvektor für sessions in gruppe j
       datreg <- as.vector(datregmat)
       xold <- pagevek[datreg > 0]				#VD > 0
       datreg <- datreg[datreg > 0]

       wphm <- survreg(Surv(datreg)~factor(xold),dist=Sdist)  		#xold bezieht sich auf seiten
       scalebase <- as.vector(wphm$coefficients[1])
       scalevec1 <- as.vector(exp(wphm$coefficients[2:p]+scalebase))
       scale[j,] <- c(exp(scalebase),scalevec1)
       shape[j,] <- 1/wphm$scale				#shape bleibt über seiten konstant
     }
anzpar <- K*p+K
}

if (method=="int.gp") {
    datreg <- as.vector(x)
    nsess <- dim(x)[1]
    pagevek <- rep(1:p,rep(nsess,p))
    oldall <- rep(old,p)
    xoldg <- oldall[datreg > 0]				#Gruppencontrast
    xoldp <- pagevek[datreg > 0]			#Seitencontrast
    datreg <- datreg[datreg > 0]

    wphm <- survreg(Surv(datreg)~factor(xoldg)*factor(xoldp),dist=Sdist)
    scalebase <- as.vector(exp(wphm$coefficients[1]))
    scaleg <- exp(c(0,wphm$coefficient[2:K]))		#group contrast
    scalep <- exp(c(0,wphm$coefficient[(K+1):(K+p-1)])) #page contrast
    scaleimat <- matrix(exp(wphm$coefficient[(K+p):(K*p)]),(K-1),(p-1)) #interaction effects
    scaleimat <- rbind(rep(1,p),cbind(rep(1,K-1),scaleimat))
    scaletemp <- outer(scaleg,scalep)*scalebase
    scale <- scaletemp*scaleimat
    shape <- matrix((1/wphm$scale),K,p)
    anzpar <- K*p+1
}

if (method=="main.gp") {
    datreg <- as.vector(x)
    nsess <- dim(x)[1]
    pagevek <- rep(1:p,rep(nsess,p))
    oldall <- rep(old,p)
    xoldg <- oldall[datreg > 0]				#Gruppencontrast
    xoldp <- pagevek[datreg > 0]			#Seitencontrast
    datreg <- datreg[datreg > 0]

    wphm <- survreg(Surv(datreg)~factor(xoldg)+factor(xoldp),dist=Sdist)
    scalebase <- as.vector(exp(wphm$coefficients[1]))
    scaleg <- exp(c(0,wphm$coefficient[2:K]))		#group contrast
    scalep <- exp(c(0,wphm$coefficient[(K+1):(K+p-1)]))	#page contrast
    scale <- outer(scaleg,scalep)*scalebase
    shape <- matrix((1/wphm$scale),K,p)
    anzpar <- K+p
}

list (scale=scale,shape=shape,prior=prior,anzpar=anzpar)
}

#returns matrices with shape and scale parameters as well as prior matrix
