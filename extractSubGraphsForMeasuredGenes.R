extractSubGraphsForMeasuredGenes <- function(path = "./KEGGxml_PosCons/", measuredGeneID){
	filenames<- dir(path)
	pathwaySubGraphs <- lapply(filenames, function(file, measuredGeneID){
		pathway <- parseKGML(paste(path, file, sep=""))
		pathwayGraph <- KEGGpathway2Graph(pathway, genesOnly = TRUE)
		detectedGenes <- nodes(pathwayGraph) %in% measuredGeneID
		detectedNodes <- nodes(pathwayGraph)[detectedGenes]
		pathwaySubGraphs <- subKEGGgraph(detectedNodes, pathwayGraph)
	}, measuredGeneID)
	return(pathwaySubGraphs)
}
