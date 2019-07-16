


Disc <- read.delim(file.path("..", "inputData", "discovery_ExpressionMatrix.txt"),
    check.names = FALSE)
Valid <- read.delim(file.path("..", "inputData", "validation_ExpressionMatrix.txt"),
    check.names = FALSE, sep = " ")
Norm <- read.delim(file.path("..", "inputData", "normals_ExpressionMatrix.txt"),
    check.names = FALSE)

# length(intersect(colnames(Disc), colnames(Valid)))
# length(intersect(colnames(Disc), colnames(Norm)))
# length(intersect(colnames(Norm), colnames(Valid)))
# identical(rownames(Disc), rownames(Valid[rownames(Disc), ]))
# identical(rownames(Disc), rownames(Norm[rownames(Disc), ]))

exprs <- cbind(
    Disc, 
    Valid[rownames(Disc), ],
    Norm[rownames(Disc),])

rm(list = c("Disc", "Valid", "Norm"))
gc()