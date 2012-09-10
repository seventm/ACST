#p_val
computePValue<-function(x, id){ 
	result<-sum(x>=x[id])/length(x)
	return(result)
}
