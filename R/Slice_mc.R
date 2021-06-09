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
#'  \tabular{lll}{
#'  **Element** \tab **Type** \tab **Description**\cr
#'  `$x1` \tab `numeric` \tab the new point \cr
#'  `$L` \tab `numeric` \tab the left boundary of the slice \cr
#'  `$R` \tab `numeric` \tab the right boundary of the slice \cr
#' }
#' @export
#'
#' @examples
#' #TL example from RLumModel
#' require(RLumModel)
#' data("ExampleData.ModelOutput", envir = environment())
#' TL_curve <- get_RLum(model.output, recordType = "TL$", drop = FALSE)
#' x<-TL_curve$data[,1]
#' y<-TL_curve$data[,2]
#' if (dev.cur()!=1) dev.off()
#' Slice_mc(x,y,n_iter=1000,n_burnin=500)
#'
#'
Slice_mc<-function(x,y,n_iter=1000,n_burnin=n_iter/2){
	mcInit<-Slice_Init(x,y)
	foo_x<-mcInit$foo_x
	foo_y<-mcInit$foo_y
	hist_y<-mcInit$hist_y

	repeat{
	  x0<-floor(runif(1,30,max(x)))
	  if(foo_x(x0)>hist_y$breaks[2]) break
	}

	mcSlice<-list(x1=x0,L=0,R=0)
	for (i in 1:n_iter){
	run<-Slice_Run(mcSlice$x1,foo_x,foo_y,hist_y,Rmx=max(x))
	mcSlice$x1[i]<-run$x1
	mcSlice$L[i]<-run$L
	mcSlice$R[i]<-run$R
	}

	return(mcSlice)
}

