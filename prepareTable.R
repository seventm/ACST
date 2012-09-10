prepareTable<- function(orginalResults, permutatedResults){
	tmp <- cbind(orginalResults, sapply(permutatedResults, function(x) return(x)))
	colnames(tmp) <- c("ORGINAL", paste("Permutation", 1:length(permutatedResults), sep="_" ) )
	return(tmp)
}
