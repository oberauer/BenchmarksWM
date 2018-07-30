#Function for plotting means with 95% CI, offset by off on the x-axis
#data = dataset
#varvector = vector of the numbers of the variables to be plotted
#xvector = vector of categories on the x axis to which the variables are assigned
#off = offset of data points relative to category values in xvector

plot.confint <- function(data, varvector, xvector=1:length(varvector), off=0, upper=T, lower=T, add=F,
         xlim=c(xvector[1]-0.5, xvector[length(xvector)]+0.5), ylim=NULL, type="b", pch=c(21:25), bg="white", ...) 
{
#library(gplots)
library(Hmisc)
data <- as.matrix(data[,varvector])
if (is.null(ylim)) ylim=c(0,max(data))
m <- colMeans(data, na.rm=TRUE)
std <- apply(data, 2, sd, na.rm=TRUE)
ci <- 1.96*std/sqrt(length(t(data[,1])))    #t = transpose
if (upper==T) uiw <-ci else uiw <- 0
if (lower==T) liw <-ci else liw <- 0
x <- xvector+off
#plot(x,m,xlim=xlim, type=type, pch=pch, bg=bg, ...)
#plotCI(x,m,uiw=uiw,liw=liw,xlim=xlim, gap=0.3, add=TRUE, ...) 
errbar(x, m, yplus=m+uiw, yminus=m-liw, xlim=xlim, ylim=ylim, add=add, type=type, pch=pch, bg=bg, ...)


}