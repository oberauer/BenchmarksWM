#Compute Means and Confidence Intervals (from wide data)
Confint <- function(data) {
  means <- colMeans(data)
  ci <- 1.96*apply(data, MARGIN=2, sd)/sqrt(dim(data)[1])
  upperci <- means+ci
  lowerci <- means-ci
  return(rbind(means, upperci, lowerci))
}
