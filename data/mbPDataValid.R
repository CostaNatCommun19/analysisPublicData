# 
# read pData of the "discovery" set. Phenotypic data available in supplementary tables of Curtis et al., Nature, 2012
# http://www.nature.com/nature/journal/v486/n7403/extref/nature10983-s2.zip
# local copy 2014-02-28 "data/repository/asprovided/metabric/clinicalData/table_S3_revised.txt"
# tab delimited file
###############################################################################

mbPDataValid <- read.delim(file = file.path("..", "inputData", "table_S3_revised.txt"), 
           stringsAsFactors = FALSE)



