## Gene modules, Desmedt08CCR: 
## http://www.ncbi.nlm.nih.gov/pubmed/18698033
##
## * ESR1 (estrogen receptor pathway)
## * ERBB2 (her2/neu receptor pathway) 
## * AURKA (proliferation)
## * STAT1 (immune response) 
## * PLAU (tumor invasion)
## * VEGF (angogenesis) 
## * CASP3 (apoptosis)
##
## Author(s):
## John Lövrot <john.lovrot@ki.se>
## 
## Licence: CC BY 4.0
## https://creativecommons.org/licenses/by/4.0/

data("mod1", package="genefu")

allentrezids <- unique(do.call("rbind", mod1)$EntrezGene.ID)

## Extract eset with probes annotated with the genes in the Desmedt08CCR modules
eset <- mbEset[fData(mbEset)$entrezid %in% allentrezids, ]

## Collapse to genes
if (annotation(eset) %in% c("hgu133a", "HuRSTA2a520709")) {
    eset <- featureFilter(eset)
} else {
    eset <- averageExprsByFDataVar(eset, "entrezid")
    fData(eset)$entrezid <- featureNames(eset)
}
fData(eset)$EntrezGene.ID <- fData(eset)$entrezid

## Derive relative expression
exprs(eset) <- sweep(exprs(eset), 1, apply(exprs(eset), 1, "median"))

## Derive module scores
tmpPData <- pData(eset)
for (modnam in names(mod1)) {
    tmp <- sig.score(
        x = mod1[[modnam]], 
        data = t(exprs(eset)), 
        annot = fData(eset), 
        do.mapping = TRUE, 
        signed = TRUE)
    modnam2 <- paste("Desmedt08CCRmodule", modnam, sep="")
    tmpPData[, modnam2] <- tmp$score[rownames(tmpPData)]
}

eset2 <- mbEset
pData(eset2) <- tmpPData

## Check
if (!validObject(eset2))
    stop("eset2 not assembled correctly")

mbEset <- eset2

## Clean up
remove(list=c("allentrezids", "eset", "eset2"))
