# 
# read pData of the "discovery" set. Phenotypic data available in supplementary tables of Curtis et al., 
# Nature, 2012
# http://www.nature.com/nature/journal/v486/n7403/extref/nature10983-s2.zip
# local copy 2014-02-28 in "data/repository/asprovided/metabric/clinicalData/table_S2_revised.txt"
# tab delimited file
###############################################################################

mbPDataDisc <- read.delim(file = file.path("..", "inputData", "table_S2_revised.txt"), 
                          stringsAsFactors = FALSE)

