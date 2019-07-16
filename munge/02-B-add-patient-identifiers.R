
pairs <- tumour.normal.pair
pairs$subjid <- dplyr::id(pairs, drop = TRUE)

##### Sample pairs in the METABRIC data
# * For 12 patients in the METABRIC cohort, several samples are present as 
#several rows in the data (and only one of those rows should be selected for 
#survival analyses for instance). It is clarified in the supplementary methods 
#of the Curtis et al. Nature 2012 paper (see below)  
# * "Genotype analysis for the full set of cases subsequently revealed that 
#eight individuals were represented both in the discovery and validation set 
#(MB: 0667/0025, 0546/0326, 0327/0547, 0549/0329, 0559/0335, 0573/0355, 0408/0407,
#0432/0433), and four were represented twice in the validation set 
#(MB: 0110/0196, 0552/0330, 6213/6206, 2820/2720), but were supplied as unique 
#accessions by the tumour bank. These sample pairs represent multiple primary 
#tumours from the same individual and different sections of the same tumour."

pairs[pairs$METABRIC_ID %in% "MB-0667",]$subjid <- pairs[pairs$METABRIC_ID %in% "MB-0025",]$subjid
pairs[pairs$METABRIC_ID %in% "MB-0546",]$subjid <- pairs[pairs$METABRIC_ID %in% "MB-0326",]$subjid
pairs[pairs$METABRIC_ID %in% "MB-0327",]$subjid <- pairs[pairs$METABRIC_ID %in% "MB-0547",]$subjid
pairs[pairs$METABRIC_ID %in% "MB-0549",]$subjid <- pairs[pairs$METABRIC_ID %in% "MB-0329",]$subjid
pairs[pairs$METABRIC_ID %in% "MB-0559",]$subjid <- pairs[pairs$METABRIC_ID %in% "MB-0335",]$subjid
pairs[pairs$METABRIC_ID %in% "MB-0573",]$subjid <- pairs[pairs$METABRIC_ID %in% "MB-0355",]$subjid
pairs[pairs$METABRIC_ID %in% "MB-0408",]$subjid <- pairs[pairs$METABRIC_ID %in% "MB-0407",]$subjid
pairs[pairs$METABRIC_ID %in% "MB-0432",]$subjid <- pairs[pairs$METABRIC_ID %in% "MB-0433",]$subjid
pairs[pairs$METABRIC_ID %in% "MB-0110",]$subjid <- pairs[pairs$METABRIC_ID %in% "MB-0196",]$subjid
pairs[pairs$METABRIC_ID %in% "MB-0552",]$subjid <- pairs[pairs$METABRIC_ID %in% "MB-0330",]$subjid
pairs[pairs$METABRIC_ID %in% "MB-6213",]$subjid <- pairs[pairs$METABRIC_ID %in% "MB-6206",]$subjid
pairs[pairs$METABRIC_ID %in% "MB-2820",]$subjid <- pairs[pairs$METABRIC_ID %in% "MB-2720",]$subjid

tmpPData <- base::merge(rbind(
        data.frame(METABRIC_ID = as.character(pairs$METABRIC_ID), 
            subjid = pairs$subjid, stringsAsFactors = FALSE),
        data.frame(METABRIC_ID = as.character(pairs$MatchNormal_METABRIC_ID), 
            subjid = pairs$subjid, stringsAsFactors = FALSE)), 
    mbPData, 
    by = "METABRIC_ID", 
    all.x = FALSE, all.y = TRUE)

# tumors that have not been given a patient identifier yet (normals are all 
# paired but for some of them, the paired tumor is unkonwn)
tmpPData$subjid <- ifelse(!is.na(tmpPData$subjid) | tmpPData$tumnorm %in% "Normal", 
    tmpPData$subjid, 
    dplyr::id(tmpPData, drop = TRUE) + max(tmpPData$subjid, na.rm = TRUE))

tmpPData[tmpPData$METABRIC_ID %in% "MB-0667",]$subjid <- tmpPData[tmpPData$METABRIC_ID %in% "MB-0025",]$subjid
tmpPData[tmpPData$METABRIC_ID %in% "MB-0546",]$subjid <- tmpPData[tmpPData$METABRIC_ID %in% "MB-0326",]$subjid
tmpPData[tmpPData$METABRIC_ID %in% "MB-0327",]$subjid <- tmpPData[tmpPData$METABRIC_ID %in% "MB-0547",]$subjid
tmpPData[tmpPData$METABRIC_ID %in% "MB-0549",]$subjid <- tmpPData[tmpPData$METABRIC_ID %in% "MB-0329",]$subjid
tmpPData[tmpPData$METABRIC_ID %in% "MB-0559",]$subjid <- tmpPData[tmpPData$METABRIC_ID %in% "MB-0335",]$subjid
tmpPData[tmpPData$METABRIC_ID %in% "MB-0573",]$subjid <- tmpPData[tmpPData$METABRIC_ID %in% "MB-0355",]$subjid
tmpPData[tmpPData$METABRIC_ID %in% "MB-0408",]$subjid <- tmpPData[tmpPData$METABRIC_ID %in% "MB-0407",]$subjid
tmpPData[tmpPData$METABRIC_ID %in% "MB-0432",]$subjid <- tmpPData[tmpPData$METABRIC_ID %in% "MB-0433",]$subjid
tmpPData[tmpPData$METABRIC_ID %in% "MB-0110",]$subjid <- tmpPData[tmpPData$METABRIC_ID %in% "MB-0196",]$subjid
tmpPData[tmpPData$METABRIC_ID %in% "MB-0552",]$subjid <- tmpPData[tmpPData$METABRIC_ID %in% "MB-0330",]$subjid
tmpPData[tmpPData$METABRIC_ID %in% "MB-6213",]$subjid <- tmpPData[tmpPData$METABRIC_ID %in% "MB-6206",]$subjid
tmpPData[tmpPData$METABRIC_ID %in% "MB-2820",]$subjid <- tmpPData[tmpPData$METABRIC_ID %in% "MB-2720",]$subjid

mbPData <- tmpPData
rownames(mbPData) <- mbPData$METABRIC_ID

remove(list=c("pairs", "tumour.normal.pair", "tmpPData"))
