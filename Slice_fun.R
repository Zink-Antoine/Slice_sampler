##### various internal function #################

######
#' Fn1 (**internal function**)
#'
#' Cumulative distribution function for a function foo
#'
#' @param x [numeric] (**required**) a variable
#' @param foo [function] (**required**) the function to be applied to the variable x
#'
#' @return the cumulative distribution value of the function foo for the variable x
#'
Fn1<-function(x,foo){
  CDF<- CDF0<-0
  for (i in x) {
    CDF0<-CDF0+foo(i)
    CDF<-c(CDF,CDF0)
  }
  return(CDF/CDF0)
}

###
#' Fn2 (**internal function**)
#'
#'probability for x of density foo
#'
#' @param p [numeric] (**required**) probability
#' @inheritParams Fn1
#'
#' @return a probability
#'
Fn2<-function(p,x,foo){
  CDF<-CDF0<-pCDF<-0
  for (i in x) {
    CDF0<-CDF0+foo(i)
    CDF<-c(CDF,CDF0)
    if (i<p) pCDF<-CDF0
  }
  return(pCDF/CDF0)
}


####
#' Fn3 (**internal function**)
#'
#' quantile q for the foo distribution
#'
#' @param q [numeric] (**required**) quantile
#' @inheritParams Fn1
#'
#' @return a quantile
#'
Fn3<-function(q,x,foo){
  CDF0<-0
  i<-min(x)
  norm<-sum(foo(x))
  step<-(max(x)-min(x))/length(x)
  while (i<max(x) && CDF0<q) {
    CDF0<-CDF0+foo(i)/norm
    i<-i+step
  }
  return(i)
}


