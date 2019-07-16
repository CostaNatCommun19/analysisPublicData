

NormalIDs <- colnames(read.delim(file.path("..", "inputData", "normals_ExpressionMatrix.txt"),
    check.names = FALSE))

tumour.normal.pair <- read.delim(file.path("..", "inputData", "tumour_normal_pair.txt"))
