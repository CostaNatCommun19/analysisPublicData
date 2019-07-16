
eset <- mbEset[fData(mbEset)$symbol %in% as.character(LereboursTab1$Gene.symbols), ]

eset <- featureFilterJL(eset)

featureNames(eset) <- fData(eset)$symbol

eset2 <- mbEset
eset2$NFkBsig  <- colMeans(exprs(eset))

validObject(eset2)
mbEset <- eset2





    