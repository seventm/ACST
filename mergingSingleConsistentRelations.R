
mergingSingleConsistentRelations<-function(pathwayList){
	uniquePathwayList <- lapply( pathwayList, function(x) unique(unlist(strsplit(x, "~"))) )

	listSum <- sapply( uniquePathwayList, function(x) sum(sapply(uniquePathwayList, function(y) any(y%in%x))) )
	IDmore <- which(listSum > 1)
	choosenPathwayListEq1 <- pathwayList[listSum == 1]
	
	if(length(IDmore) > 0){
		choosenUniquePathway <- uniquePathwayList[IDmore]
		choosenPathway <- pathwayList[IDmore]
	
		choosenUniquePathwayOne	<- list(choosenUniquePathway[[1]])
		choosenPathwayOne	<- list(choosenPathway[[1]])
		
		while(length(choosenUniquePathway) > 0){
			uniquePathways <- choosenUniquePathway[[1]]
			pathways <- choosenPathway[[1]]
						 
			IDduplicates <- which(sapply(choosenUniquePathway, function(x){ return(any(uniquePathways %in% x))}))
			uniquePathways <- unique(unlist(choosenUniquePathway[IDduplicates]))
			pathways <- unique(unlist(choosenPathway))
			IDchoosen <- which(sapply(choosenUniquePathwayOne, function(x){ return(any(uniquePathways %in% x)) }) )
			  
			if(length(IDchoosen) != 0){
				uniquePathways <- unique( c(uniquePathways, unlist(choosenUniquePathwayOne[IDchoosen]) ) )
				pathways <- unique( c(pathways, unlist(choosenPathwayOne[IDchoosen]) ) )
				choosenUniquePathwayOne <- choosenUniquePathwayOne[-IDchoosen]
				choosenPathwayOne <- choosenPathwayOne[-IDchoosen]
			}	
							
			choosenUniquePathwayOne[[length(choosenUniquePathwayOne) + 1]] <- uniquePathway
			choosenPathwayOne[[length(choosenPathwayOne) + 1]] <- pathways
			
			choosenPathway <- choosenPathway[-IDduplicates]
			choosenUniquePathway <- choosenUniquePathway[-IDduplicates]
		}
		
		results <- c(choosenPathwayOne,choosenPathwayListEq1)
	} else {
		results <- choosenPathwayListEq1
	}
	return(results)
}
