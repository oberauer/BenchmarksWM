# Function for plotting means with 95% Confidence Interval for within-subjects comparisons, offset by off on the x-axis
# data must be raw data in long format 
# dv = column containing the DV (number or name)
# iv = vector of columns of the IV (numbers or names): a maximum of 2!
# id = column containing id (number or name; default: 1)
# group = column containing between-subjects variable (number of name)
# x = number of column within iv that is used as the x-axis (by default the first column in iv)

lineplot.ci3 <- function(data, dv, iv, id=1, group=NULL, x=1, off=0, Bakeman=T, upper=T, lower=T, ylim=c(0,1), xlim=NULL, 
                         pt=15:25, col=rep("black", 11), ptcol=rep("white",11),na.rm=T, cex=1, ...)  # default: remove NAs, so function runs over valid cases, without returning NA!
  
{
  library(Hmisc)
  
  if (is.character(iv) & is.numeric(id)) id = names(data)[id]  # prevent mis-match of character and numeric indices into columns
  
  # computing Bakeman & McArthur correction (for long data): id = column with subject id, dv = column with dependent variable
  
  idvar <- data[,id]
  subjMeans <- aggregate(x=data[,dv], by=list(data[,id]), FUN=mean, na.rm=na.rm)
  names(subjMeans) <- c(id, dv)
  ids <- unique(idvar)
  dimensions <- length(iv)
  
  corrdata <- data
  if (Bakeman) {
    if (!is.null(group)) {
      groupMeans <- aggregate(x=data[,dv], by=list(data[,group]), FUN=mean, na.rm=na.rm)
      grandMean <- groupMeans[,"x"]
      groupVar <- aggregate(x=data[,group], by=list(data[,id]), FUN=mean)
      names(groupVar)[1:2] <- c("ids", "grouplevel")
      groupVar$groupidx <- 0
      glevels <- sort(unique(groupVar$grouplevel))
      for (g in 1:length(glevels)) {
        groupVar[groupVar$grouplevel==glevels[g], "groupidx"] <- g  # assign integer indices to the group levels
      }
    } else {
      grandMean <- mean(subjMeans[,2], na.rm=na.rm)
      groupidx <- rep(1, length(ids))
      groupVar <- data.frame(ids, groupidx)
    }
    for (ii in 1:length(ids)) {
      corrdata[data[,id]==ids[ii],dv] <- corrdata[data[,id]==ids[ii],dv] - subjMeans[subjMeans[,id]==ids[ii],2] + 
        grandMean[groupVar[groupVar$ids==ids[ii], "groupidx"]]
    }
  }
  aggdata <- aggregate(x=corrdata[,dv], by=corrdata[,c(id,iv)], FUN=mean, na.rm=na.rm)  # first step: individual subject's mean per condition
  if (dimensions==1) byVar = list(aggdata[,2:(1+length(iv))])
  if (dimensions> 1) byVar = aggdata[,2:(1+length(iv))]
  Mdata <- aggregate(x=aggdata[,"x"], by=byVar, FUN=mean, na.rm=na.rm)  # second step: average over subjects (IVs start in column 2)
  SD <- aggregate(x=aggdata[,"x"], by=byVar, FUN=sd, na.rm=na.rm)                   # compute SD over subjects
  Mdata$ci <- 1.96*SD[,"x"]/sqrt(length(ids)) 
  if (dimensions == 1) {
    names(SD)[1] <- iv
    names(Mdata)[1] <- iv
  }
 
  xaxis <- sort(unique(as.numeric(Mdata[,iv[x]])))    
  if (is.null(xlim)) xlim=c(xaxis[1]-0.5*(xaxis[2]-xaxis[1]), xaxis[length(xaxis)]+0.5*(xaxis[2]-xaxis[1]))
  xaxt <- "s"
  if ( is.factor(Mdata[,iv[x]]) ) {
    xticks <- levels(Mdata[,iv[x]])
    xaxt <- "n"    
  }
  
  if (dimensions==1) {
    loop = 1   
  } 
  else {
    iv2vals <- sort(unique(as.numeric(Mdata[,iv[3-x]])))
    loop <- length(iv2vals)
  }
  
  for (j in 1:loop)  {
    if (dimensions==1) {
      y <- Mdata$x
      dci <- Mdata$ci
      xx <- sort(unique(as.numeric(Mdata[,iv[x]])))
    } else {
      subdata <- Mdata[as.numeric(Mdata[,iv[3-x]])==iv2vals[j], ]
      y <- subdata[,"x"]   
      dci <- subdata[,"ci"]  
      xx <- sort(unique(as.numeric(subdata[,iv[x]])))  
    }
    CIhigh <- y + dci
    CIlow <- y - dci
    xx <- xx + (j-1)*off
    plot(xx, y, xlim=xlim, ylim=ylim, type="b", pch=pt[j], col=col[j], bg=ptcol[j], xaxt=xaxt, cex=cex, ...)  
    errbar(xx, y, yplus=CIhigh, yminus=CIlow, xlim=xlim, ylim=ylim,add=T,type="l", xaxt=xaxt, ...)
    if (xaxt == "n") axis(side=1, at=xaxis, labels=xticks)  # if x-axis is a factor: add the factor levels as tick labels 
    if (j < loop) par(new=T)
  }
  
}




