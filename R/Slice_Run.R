#' Slice_Run
#'
#' function determining the slice and the new point x1
#'
#' @inheritParams Stepout
#' @param foo_x a function y(x)
#' @param foo_y a function p(y)
#' @param hist_y a histogram of y density
#'
#' @return a list with the following elements
#'  \tabular{lll}{
#'  **Element** \tab **Type** \tab **Description**\cr
#'  `$x1` \tab `numeric` \tab the new point \cr
#'  `$L` \tab `numeric` \tab the new left boundary of the slice \cr
#'  `$R` \tab `numeric` \tab the new right boundary of the slice \cr
#'  `$y0` \tab `numeric` \tab the vertical level defining the slice\cr
#' }
#'
#'
#'
#' @importFrom graphics lines
#'
#' @export
#'
#' @examples
#' #TL example from RLumModel
#' require(RLumModel)
#' data("ExampleData.ModelOutput", envir = environment())
#' TL_curve <- get_RLum(model.output, recordType = "TL$", drop = FALSE)
#' x<-TL_curve$data[,1]
#' y<-TL_curve$data[,2]
#' dev.off()
#' run0<-Slice_Init(x,y)
#' Slice_Run(run0$x0,run0$foo_x,run0$foo_y,run0$hist_y,Rmx=max(x))
#'
#' #multiples TL calculées avec RLumModel##########
#' #call function "model_LuminescenceSignals", model = "Bailey2001"
#' # the irradiation dose is varied and then compared.
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
#' run0<-Slice_Init(x[,1],y[,1])
#' Slice_Run(run0$x0,run0$foo_x,run0$foo_y,run0$hist_y,Rmx=max(x))
#'


Slice_Run<-function(x0,foo_x,foo_y,hist_y,w=40,m=10,Rmx=m*w){

	repeat{
	y0<-Fn3(runif(1),hist_y$mids,foo_y)#random drawing of a y0 of the distribution p(y)
  #print(c(x0,foo_x(x0),y0,y0<foo_x(x0)))
	if(y0<foo_x(x0)) break
	}

	Sol<-Stepout(foo_x,x0,y0,w,m,Rmx) #stepping-out
	lines(c(Sol$L,Sol$R),c(y0,y0),col=2)

	Sol_hat<-Shrink(foo_x,x0,y0,Sol$L,Sol$R,Rmx) #shrinkage
	lines(c(Sol_hat$L,Sol_hat$R),c(y0,y0),col=6)
	points(Sol_hat$x1,y0,pch=4)

	return(c(Sol_hat,y0=y0))
}
