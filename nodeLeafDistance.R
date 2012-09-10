#Compute Distances between each node and leaf
#dpf <- distance pathway file
#require(RBGL)

prepareDistanceFiles<-function(pathwaysPath = "./KEGGxml_PosCons/", distancePath = "./distance/", genesOnly = TRUE, silent = FALSE){
	fileNames<-dir(pathwaysPath)
	dir.create(distancePath, showWarnings = FALSE)
	tmp<-lapply(fileNames, function(filename, pathwaysPath, distancePath, genesOnly, silent){
		outfile <- paste(distancePath, "path_",gsub(".xml", ".dpf", filename), sep="")
		distance <- nodeLeafDistance(filename, pathwaysPath = pathwaysPath, genesOnly = genesOnly)
		write.table(distance, file=outfile, quote=FALSE, sep="\t")
		if(silent==FALSE)
			cat(paste(filename, "\t\t[OK]\n", sep=""))
	}, pathwaysPath, distancePath, genesOnly, silent)
	invisible(tmp)
}


nodeLeafDistance<-function(filename, pathwaysPath,  genesOnly=TRUE){

	pathway	  	<- parseKGML(paste(pathwaysPath,filename,sep=""))
	pathwayGraph  	<- KEGGpathway2Graph(pathway, genesOnly = genesOnly)

	notConnectedNodes <- nodes(pathwayGraph)[(degree(pathwayGraph)[[1]]==0) & (degree(pathwayGraph)[[2]]==0)]
	leafs <- nodes(pathwayGraph)[(degree(pathwayGraph)[[1]]!=0) & (degree(pathwayGraph)[[2]]==0)]

	pathwayDistance <- sapply(nodes(pathwayGraph), countNodeDistance, notConnectedNodes, leafs, pathwayGraph)
	return(pathwayDistance)
}

countNodeDistance<-function(node, notConnectedNodes, leafs, pathwayGraph){
	if(sum(node%in%notConnectedNodes)){
		return(Inf)
	} else if (sum(node%in%leafs)) {
		return(0)
	} else {
		distance <- dijkstra.sp(ugraph(pathwayGraph), node)$distance
		distance <- distance[leafs]
		distance <- distance[!is.na(distance)]
		return(min(distance))
	}
	
}	

