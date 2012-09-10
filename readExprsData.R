readExprData <- function(ExprFile){
	ExprsData <- read.delim(ExprFile, header = TRUE)
	rownames(ExprsData) <- translateGeneID2KEGGID(rownames(ExprsData))
	return(ExprsData)
}
