######### mc procedure plusieures courbes ###################

#' Slice_Cycle
#'
#' @inheritParams Slice_mc
#'
#' @return a list with the following elements
#' @return **Elements Type  Description
#' @return $x1[numeric] the new point
#' @return $L [numeric] left boundary of the slice
#' @return $R [numeric] right boundary of the slice
#' @return $iter [numeric]the number of iteration
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
	mcSlice<-alist(x1=0,L=0,R=0)
	for (i in 1:n_iter){
		run<-Slice_Run(foo_x,foo_y,hist_y,x)
		x1<-run[1]
		L<-run[2]
		R<-run[3]
		y0<-run[4]
		for (j in 2:ncol(y)){
		  foo_x<-mcInit[[j]]$foo_x
		  foo_y<-mcInit[[j]]$foo_y
		  hist_y<-mcInit[[j]]$hist_y
			if (y0>foo_x(x1)){
				Sol_hat<-Shrink(foo_x,x1,y0,L,R) #shrinkage
				x1<-Sol_hat[1]
				L<-Sol_hat[2]
				R<-Sol_hat[3]
				if (y0>foo_x(x1)){
					run<-Slice_Run(foo_x,foo_y,hist_y,x)
					x1<-run[1]
					L<-run[2]
					R<-run[3]
					y0<-run[4]
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

