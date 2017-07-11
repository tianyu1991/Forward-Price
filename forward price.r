
FP <- function(PY,i,time=1,C1=0,C1_time=0,C2=0,C2_time=0){
	C1_pv<-0
	C2_pv<-0
	if(C1_time<time){
		C1_pv<-C1*exp(-i*C1_time)}
	if(C2_time<time){
		C2_pv<-C2*exp(-i*C2_time)}
	ForwardPrice<-(PY-C1_pv-C2_pv)*exp(time*i)
	return(ForwardPrice)
}
