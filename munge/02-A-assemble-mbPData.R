
identical(names(mbPDataDisc), names(mbPDataValid))
#[1] TRUE


mbPData <- 
    base::merge(
        base::merge(
            data.frame(mbPDataDisc, tumnorm = "Tumor", set = "Discovery", stringsAsFactors = FALSE), 
            data.frame(mbPDataValid, tumnorm = "Tumor", set = "Validation", stringsAsFactors = FALSE), 
            all = TRUE), 
        data.frame(METABRIC_ID = NormalIDs, tumnorm = "Normal", stringsAsFactors = FALSE),
        all = TRUE)
rownames(mbPData) <- mbPData$METABRIC_ID

mbPData <- within(mbPData, {
        siteid = factor(Site)
        usampid = METABRIC_ID
        tumnorm = factor(tumnorm)
        set = factor(set)
        
        age = age_at_diagnosis
        ageu = ifelse(is.na(age), NA, "years")
        agerfdt = ifelse(is.na(age), NA, "diagnosis")
        menopstatimp = factor(ifelse(menopausal_status_inferred %in% "null", NA, 
                menopausal_status_inferred))
        
        tumsize = ifelse(size %in% "null", NA, size)
        tumsize = as.numeric(tumsize)
        tumsizeu = ifelse(is.na(tumsize), NA, "mm")        
        lnnr = ifelse(lymph_nodes_positive %in% "null", NA, lymph_nodes_positive)
        lnnr = as.numeric(lnnr)
        lnstat = factor(ifelse(lnnr > 0, "LN+", ifelse(lnnr %in% 0, "LN-", NA)))
        lnremoved = ifelse(lymph_nodes_removed %in% "null", NA, lymph_nodes_removed)
        lnremoved = as.numeric(lnremoved)
        stage = factor(stage)
        tumgr = factor(ifelse(grade %in% "null", NA, grade))
        npi = as.numeric(NPI)

        erstat = factor(ifelse(ER_IHC_status %in% "neg", "ER-", 
                ifelse(ER_IHC_status %in% "pos", "ER+", NA)))
        ermth = ifelse(!is.na(erstat), "IHC", NA)
        erstatimp = factor(ifelse(ER.Expr %in% "+", "ER+", 
                ifelse(ER.Expr %in% "-", "ER-", NA)))
        prstatimp = factor(ifelse(PR.Expr %in% "+", "PR+", 
                ifelse(PR.Expr %in% "-", "PR-", NA)))
        her2stat = factor(ifelse(HER2_SNP6_state %in% "GAIN", "HER2+", 
                ifelse(HER2_SNP6_state %in% c("LOSS", "NEUT"), "HER2-", NA)))   
        her2mth = ifelse(is.na(her2stat), NA, "DNA copy-number changes") 
        her2val = ifelse(HER2_IHC_status %in% "null", NA, HER2_IHC_status)
        her2val = as.numeric(her2val)
        her2statimp = factor(ifelse(Her2.Expr %in% "+", "HER2+", 
                ifelse(Her2.Expr %in% "-", "HER2-", NA)))
        
        cellularity = ifelse(cellularity %in% "undef", NA, cellularity)
        cellularity = factor(cellularity)
        histoltype = factor(ifelse(histological_type %in% "null", NA, histological_type))
        
        p53stat = factor(ifelse(P53_mutation_status %in% "MUT", "p53mut", 
                ifelse(P53_mutation_status %in% "WT", "p53wt", NA)))
        p53muttype = factor(P53_mutation_type)
        p53mutdetails = P53_mutation_details
        
        subtype = factor(ifelse(Pam50Subtype %in% "Basal", "Basal-like", 
                ifelse(Pam50Subtype %in% "LumA", "Luminal A", 
                    ifelse(Pam50Subtype %in% "Her2", "HER2-enriched", 
                        ifelse(Pam50Subtype %in% "LumB", "Luminal B", 
                            ifelse(Pam50Subtype %in% "NC", "No subtype", 
                                ifelse(Pam50Subtype %in% "Normal", "Normal Breast-like", NA)))))))
        subtypecd = factor(ifelse(Pam50Subtype %in% "Basal", "BL", 
                ifelse(Pam50Subtype %in% "LumA", "LA", 
                    ifelse(Pam50Subtype %in% "Her2", "H2", 
                        ifelse(Pam50Subtype %in% "LumB", "LB", 
                            ifelse(Pam50Subtype %in% "NC", "None", 
                                ifelse(Pam50Subtype %in% "Normal", "NBL", NA)))))))
        subtypemth = ifelse(!is.na(subtype), "PAM50", NA)
        intclust = factor(IntClustMemb)
        genius = factor(Genefu)
        
        adjtrt = factor(Treatment)

        survdthtm = T/365.25
        survdthtmu = ifelse(is.na(survdthtm), NA, "years")
        survdthrfdt = ifelse(is.na(survdthtm), NA, "unknown")
        dthstat = ifelse(last_follow_up_status %in% "a", FALSE, 
            ifelse(last_follow_up_status %in% c("d", "d-d.s.", "d-o.c."), TRUE, NA))
        dthcd = factor(ifelse(last_follow_up_status %in% "d-d.s.", "BC", 
                ifelse(last_follow_up_status %in% "d-o.c.", "Other", NA)))
        dthbcstat = ifelse(last_follow_up_status %in% c("a", "d-o.c."), FALSE, 
            ifelse(last_follow_up_status %in% "d-d.s.", TRUE, NA))
        dthbcstat2 = ifelse(last_follow_up_status %in% c("a", "d-o.c.", "d"), FALSE, 
            ifelse(last_follow_up_status %in% "d-d.s.", TRUE, NA))			
        dthbcstat3 = ifelse(last_follow_up_status %in% c("a", "d-o.c."), FALSE, 
            ifelse(last_follow_up_status %in% c("d-d.s.", "d"), TRUE, NA))	
        
        mbgroup = factor(group)    
    })

remove(list=c("mbPDataDisc", "mbPDataValid", "NormalIDs"))
