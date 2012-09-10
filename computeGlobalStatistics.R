computeGlobalStatistics <- function(consitentPathways, localStatistics, distances){
	squaredLocalStatistics <- localStatistics^2
	standarizedSquaredLocalStatistics <- (squaredLocalStatistics - mean(squaredLocalStatistics)) / sd(squaredLocalStatistics)
	
	transformedDistances <- distances[,1]
	transformedDistances <- 1/(transformedDistances + 1)
	names(transformedDistances) <- rownames(distances)
	
	if(length(consistentPathways) > 0){
		consistentPathwaysNodes <- lapply(consistentPathways, function(x) unique(unlist(strsplit(x, "~"))) )
	}
	if(length(consistentPathwaysNodes) > 0){
		globalStatistics <- sapply(1:length(consistentPathwaysNodes), function(i){
			id <- consistentPathwaysNodes[[i]]
			results <- sum(standarizedSquaredLocalStatistics[id]) * max(transformedDistances[id])
			return(results)
		})
	}
	return(globalStatistics)
}
