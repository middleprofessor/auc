# script to compute various area under the curves (AUC) using trapezoidal method

auc <- function(x, y, method="auc"){
  # method = "auc", auc computed using trapezoidal calc
  # method = "iauc" is an incremental AUC of Le Floch
  # method = "pos.iauc" is a "positive" incremental AUC of Le Floch but not Wolever
  # method = "pb.auc" is AUC of post-time0 values
  if(method=="iauc"){y <- y - y[1]}
  if(method=="pos.iauc"){y[y < 0] <- 0}
  if(method=="pb.auc"){
    x <- x[-1]
    y <- y[-1]
  }
  n <- length(x)
  area <- 0
  for(i in 2:n){
    area <- area + (x[i] - x[i-1])*(y[i-1] + y[i])
  }
  area/2
}

auc2 <- function(x, y, baseline=FALSE){
  # Wilding et al. 2012
  # same as above
  if(baseline==TRUE){y <- y - y[1]}
  p <- length(x)
  ap <- x[3:p]
  am <- x[1:(p-2)]
  a <- numeric(p)
  a[1] <- (x[2] - x[1])/2
  a[2:(p-1)] <- (ap - am)/2
  a[p] <- (x[p] - x[p-1])/2
  auc2 <- a%*%y
}
