##Forward Price

FP <- function(PY,i,time=1,C1=0,C1_time=0,C2=0,C2_time=0,c=0,y=0){

	if(C1_time<time){
		C1_pv<-C1*exp(-i*C1_time)} 
	else{C1_pv<-0}

	if(C2_time<time){
		C2_pv<-C2*exp(-i*C2_time)}
	else{C2_pv<-0}
	rate<-i+c-y
	ForwardPrice<-(PY-C1_pv-C2_pv)*exp(rate*time)
	return(ForwardPrice)
}
##FP(PY,i,time=1,C1=0,C1_time=0,C2=0,C2_time=0,c=0,y=0)
##  Input variable:
##  PY: Present value (require)
##  i: Continually compounded interest rate (require)
##  time: Forward contract time period (year), default:1
##  C1: Amount of first coupon, default: 0
##  C1_time: Time of first coupon (year), default: 0
##  C1: Amount of first coupon, default: 0
##  C1_time: Time of first coupon (year), default: 0
##  c: Continually compounded cost yield, default: 0
##  y: Continually compounded convenient yield, default: 0
