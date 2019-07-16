

library("ProjectTemplate")
reload.project(list(data_loading = FALSE, cache_loading = TRUE, munging = FALSE, 
                    load_libraries = TRUE, recursive_loading = FALSE))

theme_set(theme_bw())

colWeigelt10subtypecd <- c("#ed3123", "orange", "#926eb0", "#00539d", "#9cabd3", 
    "#6cb23e", "gray")
names(colWeigelt10subtypecd) <- c("Basal", "Claudin-low", "Her2", "LumA", "LumB", 
    "Normal", "Normal breast")
IntClustCol <- c("gray", "#ff5500", "#01ed6f", "#cf3079", "#00c1d3", "#8d0100", 
    "#ffff3d", "#0000d1", "#fea900", "#f47ef3", "#7a25ca")
names(IntClustCol) <- c("N", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
colWeigelt10subtype <- c("#ed3123", "orange", "#926eb0", "#00539d", "#9cabd3", 
    "#6cb23e", "gray")
names(colWeigelt10subtype) <- c("Basal-like", "Claudin-low", "HER2-enriched", 
    "Luminal A", "Luminal B", "Normal Breast-like", "Normal")
colWeigelt10subtypeSh <- colWeigelt10subtype
names(colWeigelt10subtypeSh) <- c("BL", "CL", "H2", "LA", "LB", "NBL", "N")

## figure 1
# fig1 A: tum vs normal
tmp <- mbAllL %>%
    filter(variable == "PAK4.ILMN_1728887")
df <- data.frame(Fold_Change = round(2^(summary(lm(value ~ tumnorm, data = tmp))$coefficients[ "tumnormTumor", "Estimate"]), 1),
    pvalue = formatPValue(wilcox.test(tmp$value ~ tmp$tumnorm, paired = FALSE)$p.value))
rownames(df) <- "PAK4 Tumor (vs. Normal)"

txt <- paste(c("Fold Change = ", df$Fold_Change, "\n", "p ", as.character(df$pvalue)), collapse = "")

# bitmap(file=paste("graphs/fig1A_tumVsNorm_probe172_withLegends", "tiff", sep="."), 
#     res = 600, type = "tiff24nc")  
theme_set(theme_bw(base_size = 25))
my_grob = grobTree(textGrob(txt, x=0.05,  y=0.92, hjust=0,
        gp=gpar(fontsize=15)))
p <- mbAllL %>%
    filter(variable == "PAK4.ILMN_1728887") %>% 
    ggplot(aes(x = tumnorm, y = value)) +
    geom_boxplot(notch = TRUE, notchwidth = 0.5, aes(fill = tumnorm)) + 
    labs(x = "", y = "PAK4 expression") +
    theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_rect(colour = "black"),
        legend.position = "none") +
    scale_fill_manual(values = c("gray", "black")) +
    annotation_custom(my_grob)

plot(p)
# dev.off()

# fig1 B: PAK4 and PAM50
# bitmap(file=paste("figures/fig1B_PAK4VsPam50_all_probe172_withLegends", "tiff", sep="."), 
#     res = 600, type = "tiff24nc")
p <- mbAllL %>%
    filter(variable == "PAK4.ILMN_1728887" & (subtype != "No subtype" | tumnorm == "Normal")) %>%
    ggplot(aes(x = Pam50SubtypeN, y = value)) + 
    geom_boxplot(notch = TRUE, notchwidth = 0.5, aes(fill = Pam50SubtypeN)) + 
    labs(y = "PAK4 expression", x = "PAM50 subtypes") +
    scale_fill_manual(values = colWeigelt10subtypeSh) + 
    theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_rect(colour = "black"),
        legend.position="none")
plot(p)
# dev.off()

# fig1 C: PAK4 and Integrative Clusters
# bitmap(file=paste("figures/fig1C_PAK4VsIntegrSub_DiscSetpNormal_probe172_withLegends", "tiff", sep="."), 
#     res = 600, type = "tiff24nc")
p <- mbAllL %>%
    filter(variable == "PAK4.ILMN_1728887" & (set == "Discovery" | tumnorm == "Normal")) %>%
    ggplot(aes(x = intclustN, y = value)) + 
    geom_boxplot(notch = TRUE, notchwidth = 0.5, aes(fill = intclustN)) + 
    labs(y = "PAK4 expression", x = "Integrative Subtypes") +
    scale_fill_manual(values = IntClustCol) + 
    theme(legend.position="none", 
        panel.background = element_rect(colour = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
plot(p)
# dev.off()

# fig1 D: PAK4 and DSS
# bitmap(file=paste("figures/fig1D_PAK4andDSS_tum_probe172_withLegends", "tiff", 
#         sep="."), res = 600, type = "tiff24nc", width = 12, height = 10)  
par(ps = 21) 
survplotJCOJL(
    Surv(survdthtm, dthbcstat) ~ PAK4c, 
    data = mbPatL,
    subset = (variable == "PAK4.ILMN_1728887" & tumnorm == "Tumor"),
    show.JCOtable = FALSE, 
    show.pvalue = FALSE,
    show.nrisk = TRUE,
    mark.time = FALSE,
    stitle = "PAK4 expression",
    xlab = "Years", 
    ylab = "Disease-specific Survival",
    col = c("#6cb23e", "lightblue", "#0065BD", "#00305e"))
cox <- coxph(Surv(survdthtm, dthbcstat) ~ PAK4c + strata(siteid), data = mbPatL, 
    subset = (variable == "PAK4.ILMN_1728887" & tumnorm == "Tumor"))
tab <- tableCoxph(cox, 
    stitles = c("]Q1;Q2] (vs. <=Q1)", "]Q2;Q3] (vs. <=Q1)", ">Q3 (vs. <=Q1)"),
    show.pvalue = FALSE, digits = 2)
tab = rbind(c("ref.", ""), tab)
p <- round(summary(cox)$sctest[3], 4)
txt1 <- bquote(.(paste("Score", "(log-rank) test: p = ", sep = " ")) ~ .(p))
legend("bottomright", legend = txt1, bty = 'n')
addtable2plot(7, -0.01, table = tab, xjust = 0, yjust = 1, ypad = 0.65, 
    box.col = "black", hlines = TRUE, vlines = TRUE, bty = 'o')
# dev.off()

# fig1 E: PAK4 and DSS (untreated)
# bitmap(file=paste("figures/fig1E_PAK4andDSS_UntreatedTum_probe172_withLegends", "tiff", 
#         sep="."), res = 600, type = "tiff24nc", width = 12, height = 10)  
par(ps = 21) 
survplotJCOJL(
    Surv(survdthtm, dthbcstat) ~ PAK4c, 
    data = mbPatL,
    subset = (variable == "PAK4.ILMN_1728887" & tumnorm == "Tumor" & Treatment %in% c("NONE", "RT")),
    show.JCOtable = FALSE, 
    show.pvalue = FALSE,
    show.nrisk = TRUE,
    mark.time = FALSE,
    stitle = "PAK4 expression",
    xlab = "Years", 
    ylab = "Disease-specific Survival",
    col = c("#6cb23e", "lightblue", "#0065BD", "#00305e"))
cox <- coxph(Surv(survdthtm, dthbcstat) ~ PAK4c + strata(siteid), data = mbPatL, 
    subset = (variable == "PAK4.ILMN_1728887" & tumnorm == "Tumor" & Treatment %in% c("NONE", "RT")))
tab <- tableCoxph(cox, 
    stitles = c("]Q1;Q2] (vs. <=Q1)", "]Q2;Q3] (vs. <=Q1)", ">Q3 (vs. <=Q1)"),
    show.pvalue = FALSE, digits = 2)
tab = rbind(c("ref.", ""), tab)
p <- round(summary(cox)$sctest[3], 3)
txt1 <- bquote(.(paste("Score", "(log-rank) test: p = ", sep = " ")) ~ .(p))
legend("bottomright", legend = txt1, bty = 'n')
addtable2plot(7, -0.01, table = tab, xjust = 0, yjust = 1, ypad = 0.65, 
    box.col = "black", hlines = TRUE, vlines = TRUE, bty = 'o')
# dev.off()



# fig1 F: PAK4 and AURKA

tmp <- mbTumL %>%
    filter(variable == "PAK4.ILMN_1728887" & tumnorm == "Tumor")

corr <- round(cor(tmp[, c("ProlifMeta", "value")], method = "spearman")[1, 2], 2)
pval <- formatPValue(cor.test(tmp$ProlifMeta, tmp$value, method = "spearman",
        exact = FALSE)$p.value)
txt <- paste(c("Spearman Correlation = ", corr, "\n", "p ", as.character(pval)), collapse = "")
theme_set(theme_bw(base_size = 25))
tmp <- mbTumL %>%
    filter(variable == "PAK4.ILMN_1728887" & tumnorm == "Tumor" & 
            subtype != "No subtype")

# bitmap(file=paste("figures/fig1F_PAK4VsProlif_tum_probe172_scatterplot_withLegends", "tiff", 
#         sep="."), res = 600, type = "tiff24nc") 
my_grob = grobTree(textGrob(txt, x=0.05, y=0.92, hjust=0,
        gp=gpar(fontsize=15)))
p <- tmp %>%
    ggplot(aes(x = ProlifMeta, y = value)) + 
    geom_point(size = 1.7, aes(col = Pam50Sh)) + 
    stat_smooth(method = lm, col = "black") +
    scale_colour_manual(values = colWeigelt10subtypeSh) +
    labs(x = "AURKA module score", y = "PAK4 expression") +
    theme(legend.position = "none", 
        panel.background = element_rect(colour = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
#    ylim(6.3, 10.3) +
    annotation_custom(my_grob)
plot(p)
# dev.off()


## suppl fig S1
# suppl fig S1 A: tum vs. normal (paired)
pairedSubjid <- subset(mbAllW, tumnorm %in% "Normal" & !is.na(subjid))$subjid

tmpL <- subset(mbAllW, subjid %in% pairedSubjid)
tmpL <- ddply(tmpL, .(subjid), transform, 
    nrSamples = nrow(piece),
    PAK4LowerNorm = sum(PAK4.ILMN_1728887 == min(PAK4.ILMN_1728887) & tumnorm == "Normal") > 0)

tmpL2 <- ddply(tmpL, 
    .(subjid, tumnorm, PAK4LowerNorm), 
    summarise, 
    PAK4 = mean(PAK4.ILMN_1728887)) # one patient has one normal sample and 2 tumor samples (take the mean of the tumor samples)
library(tidyr)
tmpW <- spread(tmpL2, tumnorm, PAK4)
table(tmpW$PAK4LowerNorm, useNA = 'ifany')

df <- data.frame(
    Fold_Change = round(2^(summary(lm(PAK4.ILMN_1728887 ~ tumnorm, 
                        data = tmpL))$coefficients[ "tumnormTumor", 
                    "Estimate"]), 1),
    pvalue = formatPValue(wilcox.test(tmpW$Tumor,
            tmpW$Normal, paired = TRUE)$p.value))
txt <- paste(c("Fold Change = ", df$Fold_Change, "\n", "p ", 
        as.character(df$pvalue)), collapse = "")

my_grob = grobTree(textGrob(txt, x=0.05,  y=0.92, hjust=0,
        gp=gpar(fontsize=15)))

# bitmap(file=paste("figures/figS1A_pairedTumVsNorm_probe172_withLegends2", "tiff", sep="."), 
#     res = 600, type = "tiff24nc")
p <- tmpL %>% 
    ggplot(aes(x = tumnorm, y = PAK4.ILMN_1728887)) +
    geom_line(aes(group = factor(subjid), col = PAK4LowerNorm, linetype = PAK4LowerNorm)) +
    geom_boxplot(notch = TRUE, notchwidth = 0.5, aes(fill = tumnorm)) + 
    labs(x = "", y = "PAK4 expression") +
    theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_rect(colour = "black"),
        legend.position = "none") +
    scale_fill_manual(values = c("gray", "#00539d")) +
    scale_colour_manual(values = c("gray", "#00539d")) +
    scale_linetype_manual(values = c(1, 3)) +
    annotation_custom(my_grob)
plot(p)
# dev.off()

# bitmap(file=paste("figures/figS1A_pairedTumVsNorm_probe172_withLegends", "tiff", sep="."), 
#     res = 600, type = "tiff24nc")
p <- tmpL %>% 
    ggplot(aes(x = tumnorm, y = PAK4.ILMN_1728887)) +
    geom_line(aes(group = factor(subjid), col = PAK4LowerNorm, linetype = PAK4LowerNorm)) +
    geom_boxplot(notch = TRUE, notchwidth = 0.5, aes(fill = tumnorm)) + 
    labs(x = "", y = "PAK4 expression") +
    theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_rect(colour = "black"),
        legend.position = "none") +
    scale_fill_manual(values = c("gray", "black")) +
    scale_colour_manual(values = c("gray", "black")) +
    scale_linetype_manual(values = c(1, 3)) +
    annotation_custom(my_grob)
plot(p)
# dev.off()



# suppl fig S1 C: PAK4 and OS
# bitmap(file=paste("figures/figS1C_PAK4andOS_tum_probe172_withLegends", "tiff", 
#         sep="."), res = 600, type = "tiff24nc", width = 12, height = 10)  
par(ps = 21) 
survplotJCOJL(
    Surv(survdthtm, dthstat) ~ PAK4c, 
    data = mbPatL,
    subset = (variable == "PAK4.ILMN_1728887" & tumnorm == "Tumor"),
    show.JCOtable = FALSE, 
    show.pvalue = FALSE,
    show.nrisk = TRUE,
    mark.time = FALSE,
    stitle = "PAK4 expression",
    xlab = "Years", 
    ylab = "Overall Survival",
    col = c("#6cb23e", "lightblue", "#0065BD", "#00305e"))
cox <- coxph(Surv(survdthtm, dthstat) ~ PAK4c + strata(siteid), data = mbPatL, 
    subset = (variable == "PAK4.ILMN_1728887" & tumnorm == "Tumor"))
tab <- tableCoxph(cox, 
    stitles = c("]Q1;Q2] (vs. <=Q1)", "]Q2;Q3] (vs. <=Q1)", ">Q3 (vs. <=Q1)"),
    show.pvalue = FALSE, digits = 2)
tab = rbind(c("ref.", ""), tab)
p <- round(summary(cox)$sctest[3], 3)
txt1 <- bquote(.(paste("Score", "(log-rank) test: p = ", sep = " ")) ~ .(p))
legend("bottomright", legend = txt1, bty = 'n')
addtable2plot(7, -0.01, table = tab, xjust = 0, yjust = 1, ypad = 0.65, 
    box.col = "black", hlines = TRUE, vlines = TRUE, bty = 'o')
# dev.off()

# suppl fig S1 D: PAK4 and modules
# bitmap(file=paste("figures/figS1D_modules", "tiff", 
#         sep="."), res = 600, type = "tiff24nc")  
tmp <- mbTumW
names(tmp)[substr(names(tmp), 1, 12) == "Desmedt08CCR"] <- 
    gsub("CCRmodule", " ", names(tmp)[substr(names(tmp), 1, 12) == 
                "Desmedt08CCR"])
names(tmp)[substr(names(tmp), 1, 27) == "Ignatiadis12JCOmodulePIK3CA"] <- 
    gsub("JCOmodule", " ", names(tmp)[substr(names(tmp), 1, 27) == 
                "Ignatiadis12JCOmodulePIK3CA"])
tmp$PAK4 <- tmp$PAK4.ILMN_1728887
tmp <- tmp[, c("PAK4", 
        "Desmedt08 ESR1", "Desmedt08 ERBB2", "Desmedt08 AURKA",
        "Desmedt08 PLAU", "Desmedt08 VEGF", "Desmedt08 STAT1", 
        "Desmedt08 CASP3", "Ignatiadis12 PIK3CA")]
corrplotJL(cor(tmp, method = "spearman"),  method="color", 
    order = "original", tl.col = "black") 
corrplotJL(cor(tmp, method = "spearman"), add = TRUE, method="number", 
    order = "original", type="lower", diag = FALSE, tl.pos="n", cl.pos="n", 
    col="black") 
# dev.off()

## NFkB figures
# correlation matrix with only PAK4 ILMN1728887 + RelB 
# bitmap(file=paste("figures/corr_matrix_172pak4_relb", "tiff", sep="."), 
#     res = 400, type = "tiff24nc")
#par(ps = 10) 
mbTumW$Lerebours08metageneNFkB <- mbTumW$NFkBsig
tmp <- mbTumW[, c("RELB.ILMN_1811258", "PAK4.ILMN_1728887")]
tmp <- tmp[, order(names(tmp))]
corrplot(cor(tmp, method = "spearman"),  method="color", order = "hclust")
# dev.off()

# bitmap(file=paste("figures/scatterPlot_172pak4_relb_with_legends", "tiff", 
#         sep="."), res = 600, type = "tiff24nc") 
p <- mbAllW %>%
    ggplot(aes(x = RELB.ILMN_1811258, y = PAK4.ILMN_1728887, col = tumnorm)) + 
    geom_point(size = 1.7) + 
#    stat_smooth(method = lm, size = 0.5, aes(fill = tumnorm)) +
    scale_colour_manual(values = c("gray60", "#00539d")) +
#    scale_fill_manual(values = c("gray60", "#00539d")) +
    labs(x = "RELB expression", y = "PAK4 expression") +
    theme(legend.position = "none", 
        panel.background = element_rect(colour = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
plot(p)
# dev.off()



# correlation matrix with only PAK4 ILMN1728887 + Lerebours08 + RelB + 
# NFkB2 
# bitmap(file=paste("figures/corr_matrix_172pak4_Lerebours_relb_NFkB2", "tiff", sep="."), 
#     res = 400, type = "tiff24nc")
mbTumW$Lerebours08metageneNFkB <- mbTumW$NFkBsig
tmp <- mbTumW[, c("PAK4.ILMN_1728887", "Lerebours08metageneNFkB", 
        "RELB.ILMN_1811258", "NFKB2.ILMN_1799062", "NFKB2.ILMN_2390859")]
corrplotJL(cor(tmp, method = "spearman"),  method="color", order = "original", 
    tl.col = "black")
corrplotJL(cor(tmp, method = "spearman"), add = TRUE, method="number", 
    order = "original", type="lower", diag = FALSE, tl.pos="n", cl.pos="n", 
    col="black") 
# dev.off()

# correlation matrix with Only PAK4 ILMN1728887 + Lerebours08 + RelB + 
# NFkB2 + RelA + NFKB1 + 2xREL
# bitmap(file=paste("figures/corr_matrix_172pak4_Lerebours_relb_NFkB2_rela_NFkB1_rel", 
#         "tiff", sep="."), 
#     res = 400, type = "tiff24nc")
mbTumW$Lerebours08metageneNFkB <- mbTumW$NFkBsig
tmp <- mbTumW[, c("PAK4.ILMN_1728887", "Lerebours08metageneNFkB", 
        "RELB.ILMN_1811258", "NFKB2.ILMN_1799062", "NFKB2.ILMN_2390859", 
        "RELA.ILMN_1705266", "NFKB1.ILMN_1714965", "REL.ILMN_1766085", 
        "REL.ILMN_2124064")]

corrplotJL(cor(tmp, method = "spearman"),  method="color", order = "original", 
    tl.col = "black")
corrplotJL(cor(tmp, method = "spearman"), add = TRUE, method="number", 
    order = "original", type="lower", diag = FALSE, tl.pos="n", cl.pos="n", 
    col="black")
# dev.off()

p <- mbAllW %>%
  ggplot(aes(x = NFkBsig, y = PAK4.ILMN_1728887, col = tumnorm)) + 
  geom_point(size = 1.7) + 
  #    stat_smooth(method = lm, size = 0.5, aes(fill = tumnorm)) +
  scale_colour_manual(values = c("gray60", "#00539d")) +
  #    scale_fill_manual(values = c("gray60", "#00539d")) +
  labs(x = "RELB expression", y = "PAK4 expression") +
  theme(legend.position = "none", 
        panel.background = element_rect(colour = "black"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
plot(p)

tmp <- mbTumL %>%
  filter(variable == "PAK4.ILMN_1728887" & tumnorm == "Tumor")
corr <- round(cor(tmp[, c("NFkBsig", "value")], method = "spearman")[1, 2], 2)
pval <- formatPValue(cor.test(tmp$NFkBsig, tmp$value, method = "spearman",
                              exact = FALSE)$p.value)
tmp <- mbAllL %>%
  filter(variable == "PAK4.ILMN_1728887" & tumnorm == "Normal")
corr <- round(cor(tmp[, c("NFkBsig", "value")], method = "spearman")[1, 2], 2)
pval <- formatPValue(cor.test(tmp$NFkBsig, tmp$value, method = "spearman",
                              exact = FALSE)$p.value)

tmpTum <- subset(mbAllW, tumnorm == "Tumor")
corr <- round(cor(tmpTum[, c("CEBPB.ILMN_1693014", "RELB.ILMN_1811258")], method = "spearman")[1, 2], 2)
pval <- formatPValue(cor.test(tmpTum$CEBPB.ILMN_1693014, tmpTum$RELB.ILMN_1811258, method = "spearman",
                              exact = FALSE)$p.value)


tmpNorm <- subset(mbAllW, tumnorm == "Normal")
corr <- round(cor(tmpNorm[, c("CEBPB.ILMN_1693014", "RELB.ILMN_1811258")], method = "spearman")[1, 2], 2)
pval <- formatPValue(cor.test(tmpNorm$CEBPB.ILMN_1693014, tmpNorm$RELB.ILMN_1811258, method = "spearman",
                              exact = FALSE)$p.value)


## PAK4 and RELB KM
mar <- par('mar')
mar[2] <- mar[2] + 4.5
opar <- par(mar = mar)
#on.exit(par(opar))
survplotJCOJL(
  Surv(survdthtm, dthbcstat) ~ PAK4172d + RELBd, 
  data = mbPatW,
  show.JCOtable = FALSE, 
  show.pvalue = FALSE,
  show.nrisk = TRUE,
  mark.time = FALSE,
  stitle = "PAK4 + RELB expression",
  hr.pos = NA,
  xlab = "Years", 
  ylab = "Disease Specific Survival",
  lty = c(1, 2, 1, 2),
  col = c("#ed3123", "#ed3123", "#00539d", "#00539d"), 
  snames = c("PAK4 High / RELB High", "PAK4 High / RELB Low", 
             "PAK4 Low / RELB High", "PAK4 Low / RELB Low"))


mbPatW$PAK4RELB <- factor(paste("PAK4 ", mbPatW$PAK4172d, " / RELB ", mbPatW$RELBd, sep = ""),
                          labels = c("PAK4 Low / RELB High", "PAK4 Low / RELB Low", "PAK4 High / RELB Low",
                                     "PAK4 High / RELB High"),
                          levels = c("PAK4 Low / RELB High", "PAK4 Low / RELB Low", "PAK4 High / RELB Low",
                                     "PAK4 High / RELB High"))
print(xtable(coxph(
  data = mbPatW,
  Surv(survdthtm, dthbcstat) ~ PAK4RELB + strata(siteid)), digits = 2, 
  cap = "Prognostic role of the interaction between PAK4 and RELB - Cox model - Disease Specific Survival", 
  stitles = c("PAK4 Low / RELB Low (vs. Low/High)",
              "PAK4 High / RELB Low (vs. Low/High)", "PAK4 High / RELB High (vs. Low/High)"),
  label = "UnivCoxPAK4RELBDSS"), table.placement = "h!", type = "html")


print(xtable(coxph(
  data = mbPatW,
  subset = (subtype %in% "HER2-enriched"),
  Surv(survdthtm, dthbcstat) ~ PAK4RELB + strata(siteid)), digits = 2, 
  cap = "HER2-enriched subgroup - Prognostic role of the interaction between PAK4 and RELB - Cox model - Disease Specific Survival", 
  stitles = c("PAK4 Low / RELB Low (vs. Low/High)",
              "PAK4 High / RELB Low (vs. Low/High)", "PAK4 High / RELB High (vs. Low/High)"),
  label = "UnivCoxPAK4RELBDSSH2"), table.placement = "h!", type = "html")

