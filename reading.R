readDistances<-function(distancePath = "./distance/"){
	filenames<-dir(distancePath)
	distances<-lapply(filenames, function(file, distancePath) read.delim(file=paste(distancePath, file,sep=""), header=TRUE, sep="\t"), distancePath)
	return(distances)
}

readRelationTypes<-function(infoPath = "./nodesRel/"){
	filenames <- dir(infoPath)
	relationTypes <- lapply(filenames, readSingleRelationTypeFile, infoPath)
	return(relationTypes)
}
readSingleRelationTypeFile<-function(filename, infoPath){
	tmp <- read.delim(file=paste(infoPath, filename,sep=""), header=TRUE)
	singleRelations <- as.vector(tmp[,1])
	names(singleRelations) <- rownames(tmp)
	return(singleRelations)
}


readACSTresults <- function(resultsPath = "./Results/", permutated){
	filenames <- dir(resultsPath)
	if(permutated){
		filenames <- filenames[grep(pattern=".resACST$"), filenames, invert = TRUE]
		Data <- lapply( filenames, function(filename, resultsPath){
			return(read.table(file=paste(resultsPath, filename, sep=""), sep="\t", header = FALSE, row.names=1))
		}, resultsPath)
	} else { 
		filename <- filenames[grep(pattern=".resACST$"), filenames]
		Data <- read.table(file=paste(resultsPath, filename, sep=""), sep="\t", header = FALSE, row.names=1)
	}
	return(Data)
}
