

mbTumW <- ddply(mbTumW, .(subjid), transform, nrSamples = nrow(piece)) 

mbPatW <- ddply(mbTumW, .(subjid), subset, nrSamples == 1 |
        (age == min(age) & min(age) < max(age)) | 
        (survdthtm == max(survdthtm) & min(survdthtm) < max(survdthtm) & min(age) == max(age)) |
        (npi == max(npi) & min(npi) < max(npi) & min(survdthtm) == max(survdthtm) & min(age) == max(age)) |
        (set == "Discovery" & min(npi) == max(npi) & min(survdthtm) == max(survdthtm) & min(age) == max(age)) | 
        (usampid == min(usampid) & min(as.character(set)) == max(as.character(set)) & min(npi) == max(npi) & min(survdthtm) == max(survdthtm) & min(age) == max(age)))

## Martingale residuals
# Excess deaths
tmpPDataSubset <- subset(mbPatW, !is.na(survdthtm) & !is.na(dthstat))

model <- coxph(Surv(survdthtm, dthstat) ~ strata(siteid), data=tmpPDataSubset)
tmpPDataSubset$residualDth <- residuals(model, method="martingale")

mbPatW <- base::merge(mbPatW, tmpPDataSubset, all=TRUE)

remove(list=c("tmpPDataSubset", "model"))

# Excess Breast cancer deaths
tmpPDataSubset <- subset(mbPatW, !is.na(survdthtm) & !is.na(dthbcstat))

model <- coxph(Surv(survdthtm, dthbcstat) ~ strata(siteid), data=tmpPDataSubset)
tmpPDataSubset$residualBcDth <- residuals(model, method="martingale")

mbPatW <- base::merge(mbPatW, tmpPDataSubset, all=TRUE)

remove(list=c("tmpPDataSubset", "model"))

## Adjusted Martingale residuals
# Excess deaths
tmpPDataSubset <- subset(mbPatW, !is.na(survdthtm) & !is.na(dthstat) & 
        !is.na(tsizec) & !is.na(lnstat) & !is.na(tumgr) & !is.na(agec) & 
        !is.na(Pam50SubtypeRefA))

model <- coxph(Surv(survdthtm, dthstat) ~ tsizec + lnstat + tumgr + agec + 
        Pam50SubtypeRefA + strata(siteid), data=tmpPDataSubset)
tmpPDataSubset$AdjResidualDth <- residuals(model, method="martingale")

mbPatW <- base::merge(mbPatW, tmpPDataSubset, all=TRUE)

remove(list=c("tmpPDataSubset", "model"))

# Excess Breast cancer deaths
tmpPDataSubset <- subset(mbPatW, !is.na(survdthtm) & !is.na(dthbcstat) & 
        !is.na(tsizec) & !is.na(lnstat) & !is.na(tumgr) & !is.na(agec) & 
        !is.na(Pam50SubtypeRefA))

model <- coxph(Surv(survdthtm, dthbcstat) ~ tsizec + lnstat + tumgr + agec + 
        Pam50SubtypeRefA + strata(siteid), data=tmpPDataSubset)
tmpPDataSubset$AdjResidualBcDth <- residuals(model, method="martingale")

mbPatW <- base::merge(mbPatW, tmpPDataSubset, all=TRUE)

remove(list=c("tmpPDataSubset", "model"))
