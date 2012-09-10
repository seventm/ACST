#reV - localStatistics 
#GG - pathwaySubGraphs
#rel - relations
#relacje - relationScores // relationscore

findConsistentPathways <- function(localStatistics, pathwaySubGraphs, relations = relations, relationscores){
	pathways <- NULL
	
	pathwayNodes <- nodes(pathwaySubGraphs)[(degree(pathwaySubGraphs)[[2]]!=0)]
	
	for (node in pathwayNodes){
		consitentSons <- choosingConsistentSons(node, localStatistics, pathwaySubGraphs, relations, relationscores)
		
		if (length(pathways) > 0) {
 			control <- FALSE
			pathways <-lapply(pathways, function(pathway) {
 						if(any(consitentSons %in% pathway)) {
 							control <<- TRUE
 							uniqueSons<-unique(c(consitentSons, pathway))
 							return(uniqueSons)
 						} else {
							uniqueSons <- pathway
							return(uniqueSons)
 						}
 					});
 			if (!control) 
				pathways[[length(pathways)+1]] <- c(unique(consitentSons))
			} else {
				pathways[[1]] <- consitentSons
			}
		
	}
	
	if(length(pathways) > 0) 
		pathways<-mergingSingleConsistentRelations(pathways)
    
    return(pathways)
}
