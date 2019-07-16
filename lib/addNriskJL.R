

addNriskJL <- function(x, at = axTicks(1), 
        line = 4, hadj = 0.5, 
        title = 'Number at risk', title.adj = 0, 
        labels, hoff = 5, col = 1) {
    m <- nrisk(x, times = at)
    ns <- nrow(m)  # number of strata
    if(missing(labels)) {
        if(ns > 1) { 
            labels <- names(x$strata)
        } else {
            labels <- NA
        }
    }    
    label.pad <- paste(rep(' ', hoff), collapse = '') 
    labels2 <- paste(labels, label.pad, sep = '')
    labels2[is.na(labels)] <- NA
    col <- rep(col, length.out = ns)
    hasTitle <- (!is.null(title)) && (!is.na(title))
    if(hasTitle) {
        axis(1, at = par('usr')[1], labels = paste(title, label.pad, sep = ''),
                line = line - 1, tick = FALSE, hadj = 1)    
    }
    for (i in 1:ns) {
        axis(1, at = at, labels = m[i,], 
                line = line + i + hasTitle - 2, tick = FALSE, 
                col.axis = col[i], hadj = hadj)
        axis(1, at = par('usr')[1], labels = labels2[i],
                line = line + i + hasTitle - 2, tick = FALSE,
                col.axis = col[i], hadj = 1)
    }
    invisible(m)
}
