### Small helper functions mainly used in the
### "extract" function

# calculate medians of the samples of predicted values

calc.predict.medians<-function(yrep){
  m<-matrix(yrep[[1]], ncol=100, nrow=1000)
  medians<-apply(m, MARGIN = 2, FUN = median)
  return(medians)
}

#' Calculate R-squared (inner function)
#'
#' @param y A numeric vector of outcomes.
#' @param mu.rep A numberic vector of fitted values.
#' @return An r-squared statistic calculated using \code{y} and \code{mu.rep}.
#' @examples
#' calc.r.sq(c(3,2,4), c(1,2,0))
calc.r.sq<-function(y, mu.rep){
  y.mean<-mean(y)
  tss<- sum((y-y.mean)^2)
  rss<- sum((y-mu.rep)^2)
  rsq = round(1-(rss/tss),3)
  return (rsq)
}

#' Calculate maximum absolute value of Geweke diagnostic across all chains (inner function)
#' @param samples coda::mcmc.list object
#' @return A numeric value of Geweke diagnostic (as a rule, values less than 1.64 considered acceptable if the chains runs long enough)
calc.geweke<-function(samples){
  return (max(abs(coda::geweke.diag(samples)[[1]][1]$z)))
}
