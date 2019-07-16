## Author(s):
## John Lövrot <john.lovrot@ki.se>
## 
## Licence: GPL-3

averageExprsByFDataVar <- function(eset, fDataVarNam) {
    fun1 <- function(x, fDataVar, fun2) {
        y <- unlist(lapply(split(x, fDataVar), fun2))
        return(y)
    } 
    tmpExprs <- with(fData(eset), 
            apply(exprs(eset), 2, fun1, 
                    fDataVar=get(fDataVarNam), 
                    fun2=mean))
    eset2 <- ExpressionSet(
            assayData=tmpExprs, 
            phenoData=phenoData(eset), 
            experimentData=experimentData(eset), 
            protocolData=protocolData(eset))
    fData(eset2)[, fDataVarNam] <- featureNames(eset2)
    validObject(eset2)
    return(eset2)
}

