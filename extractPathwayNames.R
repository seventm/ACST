extractPathwayNames<-function(path = "./KEGGxml_PosCons/"){
	filenames<- dir(path)
	pathwayNames <- unlist(lapply(filenames, function(file){
		pathway <- parseKGML(paste(path, file, sep=""))
		return(gsub(" ", "_", getTitle(pathway)))
	}))
	names(pathwayNames) <- filenames
	return(pathwayNames)
}
