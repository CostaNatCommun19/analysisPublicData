
mbTumL <- melt(mbTumW, 
    measure.vars = c("RELB", "PAK4.ILMN_1728887", "PAK4.ILMN_1763187", "PAK4.ILMN_2354673"))
mbTumL$variable <- factor(as.character(mbTumL$variable))

cutpoint172 <- quantile(subset(mbTumL, variable == "PAK4.ILMN_1728887")$value, 
    probs = c(0.25, 0.5, 0.75))
cutpoint176 <- quantile(subset(mbTumL, variable == "PAK4.ILMN_1763187")$value, 
    probs = c(0.25, 0.5, 0.75))
cutpoint235 <- quantile(subset(mbTumL, variable == "PAK4.ILMN_2354673")$value, 
    probs = c(0.25, 0.5, 0.75))
RELBcutpoint <- quantile(subset(mbTumL, variable == "RELB")$value, 
    probs = c(0.25, 0.5, 0.75))

mbTumL <- within(mbTumL, {
        PAK4c = factor(ifelse(
                variable == "PAK4.ILMN_1728887", 
                as.character(cut(value, breaks = c('-Inf', cutpoint172, 'Inf'), 
                        labels = c("<=Q1", "]Q1;Q2]", "]Q2;Q3]", ">Q3"))), 
                ifelse(
                    variable == "PAK4.ILMN_1763187", 
                    as.character(cut(value, breaks = c('-Inf', cutpoint176, 'Inf'), 
                            labels = c("<=Q1", "]Q1;Q2]", "]Q2;Q3]", ">Q3"))),
                    ifelse(
                        variable == "PAK4.ILMN_2354673", 
                        as.character(cut(value, breaks = c('-Inf', cutpoint235, 'Inf'), 
                                labels = c("<=Q1", "]Q1;Q2]", "]Q2;Q3]", ">Q3"))),
                        NA))), 
            levels = c("<=Q1", "]Q1;Q2]", "]Q2;Q3]", ">Q3"))
        PAK4d = factor(ifelse(
                variable == "PAK4.ILMN_1728887", 
                as.character(cut(value, breaks = c('-Inf', cutpoint172[2], 'Inf'), 
                        labels = c("Low", "High"))), 
                ifelse(
                    variable == "PAK4.ILMN_1763187", 
                    as.character(cut(value, breaks = c('-Inf', cutpoint176[2], 'Inf'), 
                            labels = c("Low", "High"))),
                    ifelse(
                        variable == "PAK4.ILMN_2354673", 
                        as.character(cut(value, breaks = c('-Inf', cutpoint235[2], 'Inf'), 
                                labels = c("Low", "High"))),
                        NA))))
        RELBc = factor(ifelse(
                variable == "RELB", 
                as.character(cut(value, breaks = c('-Inf', RELBcutpoint, 'Inf'), 
            labels = c("<=Q1", "]Q1;Q2]", "]Q2;Q3]", ">Q3"))), NA))
        RELBd = factor(ifelse(
                variable == "RELB", 
                as.character(cut(value, breaks = c('-Inf', RELBcutpoint[2], 'Inf'), 
            labels = c("Low", "High"))), NA))})
