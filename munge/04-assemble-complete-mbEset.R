
mbEset <- mbExprEset

ProjectTemplate::cache("mbExprEset")
rm(list=c("mbExprEset"))
gc()

phenoData <- new("AnnotatedDataFrame", 
    data = mbPData[sampleNames(mbEset),], 
    varMetadata = metadata[names(mbPData),])

mbEset@phenoData <- phenoData

validObject(mbEset)


