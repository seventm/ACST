choosingConsistentSons <- function(node, localStatistics, pathwaySubGraphs, relations, relationScores){
	consistentSons <- NULL
	sons <- edges(pathwaySubGraphs)[[node]]
	if(length(sons) > 0){
		for(son in sons){
			relationName <- paste(node, "~", son, sep="")
			if(relationSocres[relations[relationName]] * localStatistics[node] * localStatistics[son] > 0){
				consitentSons <- c(consistentSons, relationName)
			}
		}
	}
	return(consistentSons)
}
