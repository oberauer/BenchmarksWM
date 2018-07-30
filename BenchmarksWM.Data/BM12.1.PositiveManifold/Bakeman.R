#Bakeman & McArthur correction (for wide data)
Bakeman <- function (data) {
  mean = rowMeans(data)
  corrdata = data - mean + mean(mean)
}
