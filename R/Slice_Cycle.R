#' Slice_Cycle
#'
#' Monte-Carlo procedure for multiple curves
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
#' @examples
#' #multiples TL calculated with RLumModel##########
#' #call function "model_LuminescenceSignals", model = "Bailey2001"
#' # the irradiation dose is varied and then compared.
#' require(RLumModel)
#' irradiation_dose <- seq(from = 0,to = 100,by = 20)
#' model.output <- lapply(irradiation_dose,
#'       function(x){
#'            sequence <- list(IRR = c(20, x, 1),
#'               #PH = c(220, 10, 5),
#'               TL=c(20,400,5))
#'            data <- model_LuminescenceSignals(
#'               sequence = sequence,
#'               model = "Bailey2001",
#'               plot = FALSE,
#'               verbose = FALSE)
#'            return(get_RLum(data, recordType = "TL$", drop = FALSE))
#' })
#' ##combine output curves
#' TL_curve.merged <- merge_RLum(model.output)
#' ##plot
#' plot_RLum(
#'  object = TL_curve.merged,
#'  xlab = "Temperature [°C]",
#'  ylab = "TL signal [a.u.]",
#'  main = "TL signal with various dose",
#'  legend.text = paste("dose", irradiation_dose, "Gy"),
#'  combine = TRUE)
#' ##
#' n.pt<-length(TL_curve.merged[1]$data[,1])
#' n.irr<-length(irradiation_dose)
#' y<-x<-array(dim=c(n.pt,n.irr))
#' for (i in 1:n.irr){
#'  x[,i]<-TL_curve.merged[i]$data[,1]
#'  y[,i]<-TL_curve.merged[i]$data[,2]
#' }
#'
#'
#' if (dev.cur()!=1) dev.off()
#' Slice_mc(x,y,n_iter=1000,n_burnin=500)
#'
#'
Slice_Cycle<-function(x,y,n_iter=1000,n_burnin=n_iter/2){

	mcInit<-list()
	for (j in 1:ncol(y)){
		mcInit[[j]]<-Slice_Init(x[,j],y[,j])
	}
	x0<-mcInit[[1]]$x0
	foo_x<-mcInit[[1]]$foo_x
	foo_y<-mcInit[[1]]$foo_y
	hist_y<-mcInit[[1]]$hist_y

	mcSlice<-list(x1=x0,L=0,R=0)
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

