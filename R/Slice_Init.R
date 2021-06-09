#' Slice_Init
#'
#'  function displaying the measured data (x,y),
#'  and establishing the corresponding functions y(x) and p(y)
#'
#' @param x [numeric] (**required**) a variable
#' @param y [numeric] (**required**) the measured value for x
#'
#' @return a list with the following elements
#' @return $hist_y a histogram of y density
#' @return $foo_y a function p(y)
#' @return $foo_x a function y(x)
#'
#' @importFrom graphics points plot lines hist
#' @importFrom grDevices dev.cur
#'
#' @export
#'
#' @examples
#' x<-50:550
#' y<-dnorm(x,300,75)
#' if (dev.cur()!=1)dev.off()
#' Slice_Init(x,y)
#'
Slice_Init<-function(x,y){

  if (dev.cur()==1) plot(x,y) else points(x,y)#display data (x,y)
  foo_x<-approxfun(x,y) #transformation of the data into a function y(x)
  lines(x,foo_x(x),col=3)  #display of the function y(x)

  hist_y<-hist(y,breaks=100,plot=FALSE) #drawing a curve (histogram) of y density
  foo_y<-approxfun(hist_y$mids,hist_y$density)  #transformation into function p(y)

  return(list(hist_y=hist_y,foo_y=foo_y,foo_x=foo_x))
}

