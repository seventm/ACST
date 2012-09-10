#Extract info about types of relations
#rnf <- relation node file
#relations - variable is required

extractRelationTypes<-function(pathwaysPath = "./KEGGxml_PosCons/", infoPath = "./nodesRel/", relationscores = relationScores, genesOnly = TRUE, silent = FALSE){
	filenames<-dir(pathwaysPath)
	dir.create(infoPath, showWarnings = FALSE)
	tmp<-lapply(filenames, function(filename, pathwaysPath, infoPath, genesOnly, silent){
		outfile <- paste(infoPath, "desc_rel_" ,gsub(".xml", ".rnf", filename), sep="")
		relationInfo <- extractRelationInfo(filename, pathwaysPath = pathwaysPath, relationScores=relationscores, genesOnly = genesOnly)
		write.table(relationInfo, file=outfile, quote=FALSE, sep="\t")
		if(silent==FALSE)
			cat(paste(filename, "\t\t[OK]\n", sep=""))
	}, pathwaysPath, infoPath, genesOnly, silent)
	invisible(tmp)
}

extractRelationInfo<-function(filename, pathwaysPath, relationScores=relationScores, genesOnly = TRUE){
	pathway 	<- parseKGML(paste(pathwaysPath, filename, sep=""))
	pathwayGraph 	<- KEGGpathway2Graph(pathway, genesOnly = genesOnly)
	if(length(getSubtype(pathwayGraph))>0){
		singleRelations <- lapply(getSubtype(pathwayGraph), getSingleRelation)
		relationInfo<-as.data.frame(t(sapply(singleRelations, getRelationInfo, relationScores)))
		colnames(relationInfo)<-c("name", "mark", "value")
		
		return(relationInfo)
	}
}

getSingleRelation<-function(subtype){
	name	<- ifelse(length(subtype)==0, "undef", getName(subtype[[1]])[[1]])
	value	<- ifelse(length(subtype)==0, "undef", getValue(subtype[[1]])[[1]])
	return(c(name, value))
}

getRelationInfo<-function(singleRelation, relationScores){
	return(c(singleRelation, relationScores[singleRelation[1]]))
}
