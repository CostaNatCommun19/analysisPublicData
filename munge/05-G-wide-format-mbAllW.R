
selSymbols <- c("RELB", "PAK4", "REL", "RELA", "NFKB1", "NFKB2", "CEBPB")

featureNames(mbEset) <- paste(fData(mbEset)$symbol, fData(mbEset)$IlluminaID, sep = "-")

mbAllW <- as.data.frame(mbEset[fData(mbEset)$symbol %in% selSymbols,])



