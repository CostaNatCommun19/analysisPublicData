
rownames(mbVariableNamingConvention) <- 
    mbVariableNamingConvention$variableName

metadata <- data.frame(
    row.names = names(mbPData), 
    variableName = mbVariableNamingConvention[names(mbPData), 
        "variableName"],
    labelDescription = mbVariableNamingConvention[names(mbPData), 
        "labelDescription"],
    type = mbVariableNamingConvention[names(mbPData), "type"],
    RClass = mbVariableNamingConvention[names(mbPData), "RClass"],
    controlledTermsOrFormat = mbVariableNamingConvention[names(mbPData), 
        "controlledTermsOrFormat"],
    fromMetabricVariable = mbVariableNamingConvention[names(mbPData), 
        "fromMetabricVariable"])

