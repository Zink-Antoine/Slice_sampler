############Stepping out################
#' Stepout
#'
#' Slice sampler: stepping out procedure
#' Neal(2003) fig.3
#'
#' @param foo [function] (**required**) function proportional to the density
#' @param x0 [numeric] (**required**) the current point
#' @param y [numeric] (**required**) the vertical level defining the slice
#' @param w [numeric] (**required**) estimate of the typical size of a slice
#' @param m [numeric] (**required**) integer limiting the size of a slice to mw
#' @param Rmx [numeric] (**with default**) bound value to limit edge effects (added)
#'
#' @import stats
#'
#' @return a list value
#' @return **Element  Type  Description**
#' @return $L numeric left boundary of the interval
#' @return $R numeric right boundary of the interval
#' @return $J numeric J index
#' @return $K numeric K index
#'
#' @export
#'
#'
Stepout<-function(foo,x0,y,w,m,Rmx=w*m){

	if(y>foo(x0)) return(print("x0 invalid"))

	U<-runif(1,0,1)
	L<-x0-w*U
	R<-L+w
	V<-runif(1,0,1)
	J<-floor(m*V)
	K<-(m-1)-J
	if (L<30){L<-30}
	if (R>Rmx) {R<-Rmx}

	while(J>0 && y<foo(L)){
		L<-L-w
		J<-J-1
		if (L<30){L<-30}
		}

	while(K>0 && y<foo(R)&& R<Rmx){
		R<-R+w
		K<-K-1
		if (R>Rmx) {R<-Rmx} #control line added to limit edge effects
		}

	return(list(L=L,R=R,J=J,K=K))
}

