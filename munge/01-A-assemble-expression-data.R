
experimentData <- new("MIAME",
     name = "Marion Karniely",
     lab = "METABRIC (Molecular Taxonomy of Breast Cancer International Consortium)",
     contact = "ega-helpdesk@ebi.ac.uk",
     title = "The genomic and transcriptomic architecture of 2,000 breast tumours reveals novel subgroups",
     abstract = "The elucidation of breast cancer subgroups and their molecular drivers requires integrated views of the genome and transcriptome from representative numbers of patients. We present an integrated analysis of copy number and gene expression in a discovery and validation set of 997 and 995 primary breast tumours, respectively, with long-term clinical follow-up. Inherited variants (copy number variants and single nucleotide polymorphisms) and acquired somatic copy number aberrations (CNAs) were associated with expression in 40% of genes, with the landscape dominated by cisand trans-acting CNAs. By delineating expression outlier genes driven in cis by CNAs, we identified putative cancer genes, including deletions in PPP2R2A, MTAPand MAP2K4. Unsupervised analysis of pairedDNA–RNA profiles revealed novel subgroups with distinct clinical outcomes, which reproduced in the validation cohort. These include a high-risk, oestrogen-receptor-positive 11q13/14 cis-acting subgroup and a favourable prognosis subgroup devoid of CNAs. Trans-acting aberration hotspots were found to modulate subgroup-specific gene networks, including a TCR deletion-mediated adaptive immune response in the ‘CNA-devoid’ subgroup and a basal-specific chromosome 5 deletion-associated mitotic network. Our results provide a novel molecular stratification of the breast cancer population, derived from the impact of somatic CNAs on the transcriptome.", 
     url="https://www.ebi.ac.uk/ega/studies/EGAS00000000083")

mbExprEset <- ExpressionSet(
    assayData = as.matrix(exprs),
    experimentData = experimentData,
    annotation = "Humanv3BeadID")
 
## assemble fdata
fData(mbExprEset)$probequality <- unlist(mget(featureNames(mbExprEset), 
        illuminaHumanv3PROBEQUALITY, ifnotfound = NA))
fData(mbExprEset)$ArrayAddressID <- unlist(mget(featureNames(mbExprEset), 
        illuminaHumanv3ARRAYADDRESS, ifnotfound = NA))
fData(mbExprEset)$IlluminaID <- featureNames(mbExprEset)
fData(mbExprEset)$entrezid <- unlist(mget(featureNames(mbExprEset), 
        illuminaHumanv3ENTREZID, ifnotfound = NA))
fData(mbExprEset)$symbol <- unlist(mget(featureNames(mbExprEset), 
        illuminaHumanv3SYMBOL, ifnotfound = NA))

sampleNames(mbExprEset) <- colnames(exprs(mbExprEset))
validObject(mbExprEset)

rm(list=c("exprs", "experimentData"))
