
selSymbols <- c("RELB", "PAK4", "REL", "RELA", "NFKB1", "NFKB2", "PAK1", "PAK2", "PAK3", "PAK6", "PAK7",
		"ESR1", "CEBPB")

featureNames(mbEset) <- paste(fData(mbEset)$symbol, fData(mbEset)$IlluminaID, sep = "-")

mbTumW <- subset(as.data.frame(mbEset[fData(mbEset)$symbol %in% selSymbols,]), tumnorm == "Tumor")

mbTumW <- within(mbTumW, {
        PAK4172c = cut(PAK4.ILMN_1728887, 
            breaks = c('-Inf', 
                quantile(PAK4.ILMN_1728887, probs = c(0.25, 0.5, 0.75)), 
                'Inf'), 
            labels = c("<=Q1", "]Q1;Q2]", "]Q2;Q3]", ">Q3"))
        PAK4172d = factor(cut(PAK4.ILMN_1728887, 
            breaks = c('-Inf', 
                quantile(PAK4.ILMN_1728887, probs = 0.5), 
                'Inf'), 
            labels = c("Low", "High")), levels = c("High", "Low"))
        PAK4172c3 = cut(PAK4.ILMN_1728887, 
            breaks = c('-Inf', 
                quantile(PAK4.ILMN_1728887, probs = c(0.33, 0.66)), 
                'Inf'), 
            labels = c("Low", "Interm", "High"))
        RELBc = cut(RELB.ILMN_1811258, 
            breaks = c('-Inf', 
                quantile(RELB.ILMN_1811258, probs = c(0.25, 0.5, 0.75)), 
                'Inf'), 
            labels = c("<=Q1", "]Q1;Q2]", "]Q2;Q3]", ">Q3"))
        RELBd = factor(cut(RELB.ILMN_1811258, 
            breaks = c('-Inf', 
                quantile(RELB.ILMN_1811258, probs = 0.5), 
                'Inf'), 
            labels = c("Low", "High")), levels = c("High", "Low"))
        Pam50Sh = factor(subtype, levels = c("Basal-like", "HER2-enriched", 
                "Luminal B",  "Luminal A", "Normal Breast-like"), 
            labels = c("BL", "H2","LB", "LA", "NBL"))
        Pam50Sh2 <- factor(subtype, levels = c("Basal-like", "Normal Breast-like",  
                "Luminal A", "Luminal B", "HER2-enriched"), 
            labels = c("BL", "NBL", "LA", "LB", "H2"))
        Pam50SubtypeRefA = factor(subtype, levels = c("Luminal A", 
                "Basal-like", "HER2-enriched", "Luminal B", "Normal Breast-like"))
        agec = cut(age, breaks = c('-Inf', 45, 55, 'Inf'), 
            labels = c("Age <= 45y", "Age > 45y and <= 55y", "Age > 55y"))
        agec2 = cut(age, breaks = c('-Inf', 55, 'Inf'), 
            labels = c("Age <= 55y", "Age > 55y"))
        tsizec = cut(tumsize, breaks = c('-Inf', 20, 'Inf'), 
            labels = c("Tumor size <= 20mm", "Tumor size > 20mm"))
        ProlifMeta = Desmedt08CCRmoduleAURKA
        ProlifMetac = cut(ProlifMeta, breaks = c('-Inf', quantile(Desmedt08CCRmoduleAURKA,
                    probs = c(0.33, 0.66)), 'Inf'), 
            labels = c("Low", "Interm", "High"))
        tumgr2 = factor(ifelse(as.numeric(tumgr) %in% c(1,2), 
                "gr1-2", ifelse(as.numeric(tumgr) %in% 3, "gr3", NA)))
        RELB = RELB.ILMN_1811258})
