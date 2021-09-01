##############shrinkage########################
#' Shrink
#'
#' Slice sampler: shrinkage procedure
#' Neal(2003) fig.5
#'
#' @param foo [function] (**required**) function proportional to the density
#' @param x0 [numeric] (**required**) the current point
#' @param y0 [numeric] (**required**) the vertical level defining the slice
#' @param L [numeric] (**required**) left boundary of the interval to sample from
#' @param R [numeric] (**required**) right boundary of the interval to sample from
#' @param m [numeric] (**with default**)
#' @param Rmx [numeric] (**with default**)
#' @param Lmin [numeric] (**with default**)
#'
#' @return a list with the following elements
#' @return **Elements Type  Description
#' @return $x1[numeric] the new point
#' @return $L [numeric] the new left boundary of the slice
#' @return $R [numeric] the new right boundary of the slice
#' @export
#'
Shrink<-function(foo,x0,y0,L,R,m=100,Rmx=475,Lmin=20){

	L_hat<-L
	R_hat<-R

	if (x0>Rmx) {x0<-Rmx}#control line added to limit edge effects
	if (x0<Lmin) {x0<-Lmin}#control line added to limit edge effects


	repeat{
		U<-runif(1,0,1)
		x1<-L_hat+U*(R_hat - L_hat)
		if (y0<foo(x1)) break
		if (x1<x0) {L_hat<-x1}
		else {R_hat<-x1}
	#	print(R_hat)
	#	print(L_hat)
	#	print(round(L_hat,2)==round(R_hat,2))
    if(round(L_hat,2)==round(R_hat,2)) break
	}

return(list(x1=x1,L=L_hat,R=R_hat))
}


