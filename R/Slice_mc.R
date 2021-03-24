#' Slice_mc
#'
#' Monte-Carlo procedure
#'
#' @inheritParams Slice_Init
#' @param n_iter [numeric] (**with default value**) number of total iterations (including burn in; default: 1000)
#' @param n_burnin [numeric] (**with default**) length of burn in, i.e. number of iterations to discard at the beginning.
#' Default is n_iter/2, that is, discarding the first half of the simulations.
#'
#' @return a list with the following elements
#' @return **Elements Type  Description**
#' @return $x1 [numeric]  the new point
#' @return $L  [numeric] left boundary of the slice
#' @return $R  [numeric] right boundary of the slice
#'
#' @export
#'
Slice_mc<-function(x,y,n_iter=1000,n_burnin=n_iter/2){
	mcInit<-Slice_Init(x,y)
	foo_x<-mcInit$foo_x
	foo_y<-mcInit$foo_y
	hist_y<-mcInit$hist_y
	mcSlice<-alist(x1=0,L=0,R=0)
	for (i in 1:n_iter){
	run<-Slice_Run(foo_x,foo_y,hist_y,x)
	mcSlice$x1[i]<-run[1]
	mcSlice$L[i]<-run[2]
	mcSlice$R[i]<-run[3]
	}
	return(mcSlice)
}

