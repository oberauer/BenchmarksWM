# Function for plotting means with 2-dimensional 95% Confidence Interval for within-subjects comparisons, 
# data must be raw data in long format 
# dv = column containing the 2 DVs (numbers or names)
# iv = vector of columns of the IV (numbers or names): a maximum of 2!
# id = column containing id (number or name; default: 1)
# x = number of column within dv that is used as the x-axis (by default the first column in dv)
# param = number of column within iv that is used as parameter for distinguishing point types

lineplot.ci4 <- function(data, dv, iv, id=1, x=1, param=2, upper=T, lower=T, ylim=c(0,1), xlim=NULL, main="", 
                         pt=15:25, col=rep("black", 11), ptcol=rep("white",11),na.rm=T, ...)  # default: remove NAs, so function runs over valid cases, without returning NA!
  
{
  library(Hmisc)
  library(psych)
  
  if (is.character(iv) & is.numeric(id)) id = names(data)[id]  # prevent mis-match of character and numeric indices into columns
  idvar <- data[,id]
  ids <- unique(idvar)
  
  # computing Bakeman & McArthur correction (for long data): id = column with subject id, dv = column with dependent variable
  
  corrdata <- data
  for (dx in 1:length(dv)) {
    subjMeans <- aggregate(x=data[,dv[dx]], by=list(data[,id]), FUN=mean, na.rm=na.rm)
    for (ii in 1:length(ids)) {
      corrdata[data[,id]==ids[ii],dv[dx]] <- corrdata[data[,id]==ids[ii],dv[dx]] - subjMeans[ii,2] + mean(subjMeans[,2], na.rm=na.rm)
    }
  }
  
  dimensions <- length(iv)
  if (dimensions==1) {
    loop = 1   
  } 
  else {
    iv2vals <- sort(unique(as.numeric(data[,iv[param]])))
    loop <- length(iv2vals)
  }
  
  for (j in 1:loop)  {
    dd <- corrdata[corrdata[,iv[param]]==iv2vals[j],]
    dX <- aggregate(dd[,dv[x]], by=list(dd[,iv[3-param]]), FUN=mean)
    dY <- aggregate(dd[,dv[3-x]], by=list(dd[,iv[3-param]]), FUN=mean)
    plot(dX$x, dY$x, xlim=xlim, ylim=ylim, type="b", pch=pt[j], col=col[j], bg=ptcol[j], ...)        
    errorCircles(dv[x], dv[3-x], data=dd, group=iv[3-param], paired = F, labels = "", 
                 main = main, xlim = xlim, ylim = ylim, add=T, 
                 arrow.len = 0.05, alpha = 0.05, sd = FALSE, bars = TRUE, circles = F, 
                 pch=pt[j], ...)
    par(new=T)
  }
  
}




