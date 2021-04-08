######### mc procedure plusieures courbes ###################

#' Slice_Cycle
#'
#' @inheritParams Slice_mc
#'
#' @return a list with the following elements
#'  \tabular{lll}{
#'  **Element** \tab **Type** \tab **Description**\cr
#'  `$x1` \tab `numeric` \tab the new point \cr
#'  `$L` \tab `numeric` \tab the new left boundary of the slice \cr
#'  `$R` \tab `numeric` \tab the new right boundary of the slice \cr
#'  `$iter` \tab `numeric` \tab the number of iteration\cr
#' }
#'
#' @export
#'
Slice_Cycle<-function(x,y,n_iter=1000,n_burnin=n_iter/2){

	mcInit<-list()
	for (j in 1:ncol(y)){
		mcInit[[j]]<-Slice_Init(x[,j],y[,j])
	}

	foo_x<-mcInit[[1]]$foo_x
	foo_y<-mcInit[[1]]$foo_y
	hist_y<-mcInit[[1]]$hist_y
	x1<-0
	mcSlice<-alist(x1=x1,L=0,R=0)
	for (i in 1:n_iter){
		run<-Slice_Run(x1,foo_x,foo_y,hist_y)
		x1<-run$x1
		L<-run$L
		R<-run$R
		y0<-run$y0
		for (j in 2:ncol(y)){
		  foo_x<-mcInit[[j]]$foo_x
		  foo_y<-mcInit[[j]]$foo_y
		  hist_y<-mcInit[[j]]$hist_y
			if (y0>foo_x(x1)){
				Sol_hat<-Shrink(foo_x,x1,y0,L,R) #shrinkage
				x1<-Sol_hat$x1
				L<-Sol_hat$L
				R<-Sol_hat$R
				if (y0>foo_x(x1)){
					run<-Slice_Run(x1,foo_x,foo_y,hist_y)
					x1<-run$x1
					L<-run$L
					R<-run$R
					y0<-run$y0
					}
			}
		}

		mcSlice$x1[i]<-x1
		mcSlice$L[i]<-L
		mcSlice$R[i]<-R
		mcSlice$iter[i]<-i
	}

	return(mcSlice)
}

