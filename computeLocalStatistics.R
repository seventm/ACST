#re_MEN
#x - expression data matrix /rows - genes , columns - samples/
#groupIDs1/2 - columns ID for compared groups
computeLocalStatistics<-function(x,groupIDs1,groupIDs2){ #computing local statistic
	results <- apply(x, 1, function(x, groupIDs1, groupIDs2){
			 return(t.test(x[groupIDs1], x[groupIDs2])$stat)
		})
  	return(results)
}
