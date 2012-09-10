dataTable <- prepareTable(orginalResults, permutatedResults)

singlePvalue <- function(x, id){ #p_val
	y <- x[id]
	return( sum(x >= y) / length(x) )
}

computePvalue <- function(dataTable){ #results_pv
	pValues <- sapply( 1:nrow(dataTable), function(i, dataTable){
		return( singlePvalue(dataTable[i,], 1) )
	}, dataTable)
	names(pValues) <- rownames(dataTable)
	return(pValues)
}

sumP <- function(pVal, pValues){
	return(sum(pValues <= pVal))
}

adjustPVal <- function(pVal, pValues){
	orgPv <- sumP(pVal, pValues[,1])
	tmp <- mean(sapply(2:ncol(pValues), function(i){
		sumP(pVal, pValues[ , i])
	}
	return(orgPv - tmp)
}


computeFDR <- function(pVal, pValues){
	pv <- pValues[pValues[ , 1] >= pVal , 1]
	fdr <- sapply(pv, function(pVal){
			adjPV <- max(adjustPVal(pVal, pValues), 0)
			result <- mean( sapply(2:ncol(pValues), function(i){
				tmp <- sumP(pVal, pValues[ , i])
				result<- ifelse( tmp != 0 | S != 0, (tmp)/(tmp+adjPV), 0)
				return(result)
			} ) )
			return(result)
		})
	
	return(min(fdr))
}


FDR <- function(dataTable, pathwayNames, threshold = 0.25){
		
		pValues <- sapply( 1:ncol(dataTable), function(i, dataTable){
			sapply( 1:nrow(dataTable), function(j, dataTable) {
				singlePvalue(dataTable[j, ], i)
			}, dataTable)
		}, dataTable)
		
		fdrValues <- rep(7, nrow(pValues))
		uniquePval <- sort(unique(pValues[,1]))
		
		i <- 1
		pCtrl <- 0
		
		while(pCtrl < threshold){
			ids <- which(pValues[ , 1] == uniquePval[i])
			tmp <- sapply(ids, function(i, pValues){
						return(computeFDR(pValues[i, 1], pValues))
					}, pValues) # is it possible to make it once ?
			fdrValues[ids] <- tmp
			pCtrl <- tmp[1]
			i <- i + 1
		}
		
		
		results <- data.frame(pathwayNames, gsub(".xml", names(pathwayNames)), pValues[,1], fdrValues)
		colnames(results) <- c("PathwayName", "PathwayID", "p-value", "adj.p-value")
		return(results)
		
}


