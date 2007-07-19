`plot.profile` <-
function (x, method = "mean", type = "b", pch = 19, lty = 1, lwd = 1, col = NA, 
          xlab = "Variables", axlab = NA, leglab = NA, ylab = NA, main = NA, legpos = "topright",...)
{
# profile-plot: x-Achse -> Seiten; y-Achse -> Median/Mean der Verweildauern
# method: "mean" or "median"


K <- dim(x$scale)[1]
p <- dim(x$scale)[2]

if (is.na(col)) col <- 1:K

if (substitute(method) == "mean") {
  if (is.na(ylab)) ylab <- "Mean of Survival Time"
  if (is.na(main)) main <- "Cluster-Profile \n Mean Survival Times"
  mat <- x$clmean
} else {
  if (is.na(ylab)) ylab <- "Median of Survival Time"
  if (is.na(main)) main <- "Cluster-Profile \n Median Survial Times"
  mat <- x$clmed
}

if (any(is.na(axlab))) axlab <- paste("V",1:p,sep="")
if (is.na(leglab)) leglab <- "Group"

matplot(1:p,t(mat),type=type,pch=pch,lty=lty,lwd=lwd,col=col, xaxt="n", xlab=xlab,
        ylab=ylab,main=main,...)
axis(1,at=1:p,labels=axlab)
legend(legpos,paste(c(leglab),1:K),col=col,lty=lty,lwd=lwd,...)
}

