extractAllPathwaysGeneNames <- function(path="./KEGGxml/", orgSymbol = NULL, pathKEGG="./"){
	if(is.null(orgSymbol)){
		pathKEGGfile <- paste(pathKEGG, "KEGGorgID", sep="")
		if(!file.exists(pathKEGGfile))
			stop("missing orgSymbol")
		KEGGorgID<-unlist(read.table(pathKEGGfile, stringsAsFactors=FALSE))
		orgSymbol <- paste("^", KEGGorgID, collapse=":|", sep="")
	}
	orgSymbol <- paste(orgSymbol, ":", sep="")
	filenames<- dir(path)
	geneNames<-unlist(lapply(filenames, function(file, orgSymbol) {
		pathway <- parseKGML(paste(path, file, sep=""))
		nodeNames <- unlist(sapply(nodes(pathway), function(x) x@name))
		geneNames <- nodeNames[grep(orgSymbol, nodeNames)]
		return(geneNames)
	}, orgSymbol))
	geneNames<-unique(geneNames)
	return(geneNames)
}
 
