## Compendium of published gene signatures as implemented in Ignatiadis12JCO
## http://www.ncbi.nlm.nih.gov/pubmed/22508827
##
## Author(s):
## John Lövrot <john.lovrot@ki.se>
## 
## Licence: CC BY 4.0
## https://creativecommons.org/licenses/by/4.0/

Ignatiadis12JCOSuppl2 <- within(Ignatiadis12JCOSuppl2, {
        entrezid <- as.character(EntrezGene.ID)
    })

allentrezids <- unique(Ignatiadis12JCOSuppl2$entrezid)

## Extract eset with probes annotated with the genes in the Ignatiadis12JCOSuppl2 compendium
eset <- mbEset[fData(mbEset)$entrezid %in% allentrezids, ]

## Collapse to genes
if (annotation(eset) %in% c("hgu133a", "HuRSTA2a520709")) {
    eset <- featureFilter(eset)
} else {
    eset <- averageExprsByFDataVar(eset, "entrezid")
    fData(eset)$entrezid <- featureNames(eset)
}
featureNames(eset) <- fData(eset)$entrezid

## Derive module scores
tmpPData <- pData(eset)
for (signam in unique(Ignatiadis12JCOSuppl2$signature)) {
    
    sigtab <- subset(Ignatiadis12JCOSuppl2, 
        signature == signam & entrezid %in% fData(eset)$entrezid)
    
    eset2 <- eset[sigtab$entrezid, ]
    featureNames(eset2) <- rownames(sigtab)
    fData(eset2)[rownames(sigtab), "coef"] <- sigtab[, "coefficient"]
    
    ## Derive relative expression
    exprs(eset2) <- sweep(exprs(eset2), 1, apply(exprs(eset2), 1, "median"))
    
    if (!validObject(eset2)) 
        stop("eset2 not assembled correctly")
    
    tmpPData[, paste("Ignatiadis12JCOmodule", signam, sep="")] <- apply(exprs(eset2), 2, function(x, w) x %*% w /sum(abs(w)), w=fData(eset2)$coef)
}

eset3 <- mbEset
pData(eset3) <- tmpPData

## Check
if (!validObject(eset3))
    stop("eset3 not assembled correctly")

mbEset <- eset3


## Clean up
remove(list=c("allentrezids", "eset", "eset2", "eset3"))

