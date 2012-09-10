acst<-function(groupIDs1, groupIDs2, preparedExpressionData, pathwaySubGraphs, relations, pathwayNames, distances, relationScores){
	localStatistics <- computeLocalStatistics(preparedExpressionData, groupIDs1, groupIDs2)
	
	results <- sapply(1:length(pathwaySubGraphs), function(i){
		if(numNodes(pathwaySubGraphs[[i]])!=0){
			consistentPathways <- findConsistentPathways(localStatistics, pathwaySubGraphs[[i]], relations[[i]], relationScores)
			globalStatistics <- computeGlobalStatistics(consitentPathways, localStatistics, distances[[i]])
			return(globalStatitics)
		} else return(0)
	})
	results <- t(results)
	rownames(results) <- pathwayNames
	return(results)
}

saveACST<-function(results, path = "./Results/", org, analysisID="", addstring = "", stamp = TRUE, permID<-NULL){
	datestamp <- ""
	if (stamp){
		datestamp <- format(Sys.time(), "%d%m%Y%H%M%S%p")
		datestamp <- substr(datestamp, 1, nchar(datestamp)-1)
	}
	separator <- "_"
	dir.create(path, showWarnings=FALSE)	

	if(is.null(permID))
		extension <- ".resACST"
	extension <- paste(".per", permID,sep= "")
	filename<-paste(org, separator, analysisID, separator, datestamp, separator, addstring, extension,sep="")
	write.table(results, file = filename, col.names = FALSE, sep ="\t", quote = FALSE)
	return(filename)
}
