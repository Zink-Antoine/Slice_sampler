#' Slice_Run
#'
#' function determining the slice and the new point x1
#'
#' @inheritParams Stepout
#' @param foo_x a function y(x)
#' @param foo_y a function p(y)
#' @param hist_y a histogram of y density
#' @param x need?
#'
#' @return a list with the following elements
#' @return **Elements Type  Description
#' @return $x1[numeric] the new point
#' @return $L [numeric] the new left boundary of the slice
#' @return $R [numeric] the new right boundary of the slice
#' @return $y0[numeric] the vertical level defining the slice
#'
#' @importFrom graphics lines
#'
#' @export
#'
Slice_Run<-function(x0,foo_x,foo_y,hist_y,x,w=20,m=20){

	repeat{
	y0<-Fn3(runif(1),hist_y$mids,foo_y)#random drawing of a y0 of the distribution p(y)
	if(y0<foo_x(x0)) break
	}

	Sol<-Stepout(foo_x,x0,y0,w,m) #stepping-out
	lines(Sol[1:2],c(y0,y0),col=2)
	Sol_hat<-Shrink(foo_x,x0,y0,Sol[1],Sol[2]) #shrinkage
	lines(Sol_hat[2:3],c(y0,y0),col=6)

	return(c(Sol_hat,y0))
}
