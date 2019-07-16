# based on featureFilter and findLargest functions from genefilter package
#
# modifications: Julie Lorent


featureFilterJL <- function (eset, geneIdentifier = "symbol"){
    gN <- featureNames(eset)
    testStat <- genefilter:::rowIQRs(eset)
    if (length(testStat) != length(gN)) 
        stop("testStat and gN must be the same length")
    if (is.null(names(testStat))) 
        names(testStat) = gN
    tSsp = split.default(testStat, fData(eset)[, geneIdentifier])
    uniqGenes <- sapply(tSsp, function(x) names(which.max(x)))
    eset <- eset[uniqGenes, ]
    eset
}