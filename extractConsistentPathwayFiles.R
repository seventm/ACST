extractConsistentPathwayFiles<-function(KEGGpath="./KEGGxml/", consistentPath="./KEGGxml_PosCons/", relationType="GErel", silent=FALSE){
	filenames<-dir(KEGGpath)
	dir.create(consistentPath, showWarnings = FALSE)
	tmp<-lapply(filenames, function(filename){
		parsedXML <- parseKGML(paste(KEGGpath, filename,sep=""))
		pathwayGraph <- KEGGpathway2Graph(parsedXML)
		if(any(relationType%in%unique(unlist(lapply(getKEGGedgeData(pathwayGraph), getType))))){
			file.copy(from=paste(KEGGpath, filename, sep=""), to=paste(consistentPath, filename, sep=""), overwrite=TRUE)
			if(silent==FALSE)
				cat(filename, "[Possible Consistent]\n")
		} else {
		if(silent==FALSE)
			cat(filename, "[checked]\n")
		}
		return(NULL)
	})
	cat("Possible Consistent KEGG pathways extracted for", relationType, "\n")
	invisible(TRUE)
}
