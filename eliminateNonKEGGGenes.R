eliminateNonKEGGGenes <-function(ExprsData, KEGGgenes){
		ExprsData <- ExprsData[rownames(ExprsData) %in% KEGGgenes]
		return(ExprsData)
}
