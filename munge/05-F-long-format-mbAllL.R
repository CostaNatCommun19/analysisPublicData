
selSymbols <- c("RELB", "PAK4")

mbAllL <- melt(
    as.data.frame(mbEset[fData(mbEset)$symbol %in% selSymbols,]), 
    measure.vars = c("RELB.ILMN_1811258", "PAK4.ILMN_1728887", "PAK4.ILMN_1763187", 
        "PAK4.ILMN_2354673"))
mbAllL$variable <- factor(ifelse(mbAllL$variable == "RELB.ILMN_1811258", "RELB", 
        as.character(mbAllL$variable)))

mbAllL$tumnorm <- factor(as.character(mbAllL$tumnorm)) # change reference

mbAllL <- within(mbAllL, {
        Pam50SubtypeN = factor(ifelse(tumnorm == "Normal", "N", 
                as.character(subtypecd)),
            levels = c("N", "BL", "H2", "LA", 
                "LB", "None", "NBL"))
        
        intclustN = factor(ifelse(tumnorm == "Normal", "N", 
                as.character(intclust)), 
            levels = c("N", "1", "2", "3", "4", "5", "6", "7", "8", 
                "9", "10"))})

