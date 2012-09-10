#losuj_perm
randomPermutations<-function(groupNumber1, groupNumber2, permutations){
	samplesNumber <- groupNumber1+groupNumber2
	groupNumber <- groupNumber2
	results<-matrix(sample(samplesNumber, groupNumber), nrow=1)
	while(nrow(results) < permutations){
		tmp <- sample(samplesNumber, groupNumber)
		test <- sapply(1:nrow(results), function(i){
							return(length(intersect(results[i,], tmp))==groupNumber)
						})
		if (sum(test)==0)
			results<-rbind(results, tmp)
	}
	rownames(results)<-NULL
	return(results)
}
