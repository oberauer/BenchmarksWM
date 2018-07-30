# Bakeman & McArthur correction (for long data): id = column with subject id, dv = column with dependent variable
BakemanL <- function (data, id=1, dv=2) {
  idvar <- data[,id]
  subjMeans <- aggregate(x=data[,dv], by=list(data[,id]), FUN=mean)
  names(subjMeans) <- c(id, dv)
  ids <- unique(idvar)
  corrdata <- data
  for (ii in 1:length(ids)) {
    corrdata[data[,id]==ids[ii],dv] <- corrdata[data[,id]==ids[ii],dv] - subjMeans[subjMeans[,id]==ids[ii],2] + mean(subjMeans[,2])
  }
  return(corrdata)
}