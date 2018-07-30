# Function for plotting means with 95% Confidence Interval for within-subjects comparisons, offset by off on the x-axis
# data must have the structure array(0, dim=c(UV1, UV2, N)), so the last dimension = subjects, 1st and 2nd dimensions = UVs
# The data must NOT include the id number or any other not-used numbers!

lineplot.ci <- function(xaxis, data, aggfun=mean, off=0, upper=T, lower=T, xdim=1, type="b", ylim=NULL, 
                        xlim=NULL, pt=15:25, col=rep("black",11), 
                        ptcol=rep("white",11), lty=rep(1,11), xaxt="s", yaxt="s", na.rm=T, ...)

{
library(Hmisc)
dimensions <- dim(data)
if (is.null(xlim)) xlim=c(xaxis[1]-0.5*(xaxis[2]-xaxis[1]), xaxis[length(xaxis)]+0.5*(xaxis[2]-xaxis[1]))
if (is.null(ylim)) ylim=c(0,max(data))
if (length(dimensions)<3) loop2 = 1 else loop2 = dimensions[2]
N <- dimensions[length(dimensions)]
if (xdim==1) nlines <- loop2
if (xdim==2) nlines <- dimensions[1]
if (length(lty)<nlines) lty <- rep(lty, nlines)
if (length(pt)<nlines) pt <- rep(pt, nlines)
if (length(col)<nlines) col <- rep(col, nlines)
if (length(ptcol)<nlines) ptcol <- rep(ptcol, nlines)


# compute means
m <- matrix(0, dimensions[1], loop2)
for (i in 1:dimensions[1])  {
  for (j in 1:loop2) {
    if (length(dimensions)<3) m[i,j] <- aggfun(data[i,], na.rm=na.rm) else m[i,j] <- aggfun(data[i,j,], na.rm=na.rm)
  }
}
    
#plot means

if (xdim==1)  {
  for (j in 1:loop2)  {
    plot(xaxis + (j-1)*off, m[,j], xlim=xlim, ylim=ylim, type=type, pch=pt[j], 
         col=col[j], bg=ptcol[j], lty=lty[j], xaxt=xaxt, yaxt=yaxt, ...)  
    par(new=T)
  }
}
if (xdim==2)  {
  for (i in 1:dimensions[1])  {
    plot(xaxis + (i-1)*off, m[i,], xlim=xlim, ylim=ylim, type=type, pch=pt[i], 
         col=col[i], bg=ptcol[i], lty=lty[i], xaxt=xaxt, yaxt=yaxt,...)  
    par(new=T)
  }
}

#compute and plot confidence intervals (Bakeman & McArthur)

grandmean <- mean(data, na.rm=na.rm)
corrdata <- data
for (id in 1:N) {
  if (length(dimensions) < 3) {
    idmean <- mean(data[,id], na.rm=na.rm)
    corrdata[,id] <- data[,id] - idmean + grandmean    
  } else {
    idmean <- mean(data[,,id], na.rm=na.rm)
    corrdata[,,id] <- data[,,id] - idmean + grandmean    
  }
}
std <- matrix(0, dimensions[1], loop2)
for (i in 1:dimensions[1])  {
  for (j in 1:loop2) {
    if (length(dimensions) < 3) std[i,j] <- sd(corrdata[i,], na.rm=na.rm) else std[i,j] <- sd(corrdata[i,j,], na.rm=na.rm)
  }
}


CI <- array(0, dim=c(dimensions[1], loop2, 2))
for (i in 1:dimensions[1])  {
  for (j in 1:loop2) {  
    par(new=T)
    if (xdim == 1) {x <- xaxis[i] + (j-1)*off} else {x <- xaxis[j] + (i-1)*off}
    if (xdim == 1) colidx <- j else colidx = i
    CI[i,j,1] = m[i,j] + 1.96*std[i,j]/sqrt(N)
    CI[i,j,2] = m[i,j] - 1.96*std[i,j]/sqrt(N)
    errbar(x, m[i,j], yplus=CI[i,j,1], yminus=CI[i,j,2], xlim=xlim, ylim=ylim,add=T,type="l", errbar.col=col[colidx], ...)
  }
}

par(new=F)
return(CI)

}
  



