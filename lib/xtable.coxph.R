#
# Partly inspired by code by Jun Yan on 
# https://stat.ethz.ch/pipermail/r-help/2002-October/025501.html# 
# 
# Author(s): 
# John Lövrot <john.lovrot@ki.se>
# 
# License: Artistic-2.0
#
# Depends on the "xtable" and "plotrix" packages

xtable.coxph <- 
		function (x, caption = NULL, label = NULL, align = c("r", "l", "c", "l"), vsep =
						NULL, digits = NULL, display = NULL, conf.int = 0.95,
                    stitles = NULL) 
{
	formatPValue <- function(p, digits = 2, nsmall=2) {
		if (p<0.001) {
			txt <- "<.001" 
		} else {
			txt <- sub("0.", ".", format(p, digits = digits, nsmall=nsmall))
		}
		return(txt)
	}
	cox <- summary(x, conf.int = conf.int)
	if (nrow(cox$conf.int)==1) {
		tmp <- cbind(
				format(cox$conf.int[, 1], digits=digits), 
				paste(format(cox$conf.int[, 3:4], digits=digits), collapse=" to "), 
				sapply(cox$coefficients[, "Pr(>|z|)"], formatPValue))		
	} else {
		tmp <- cbind(
				format(cox$conf.int[, 1], digits=digits), 
				apply(format(cox$conf.int[, 3:4], digits=digits), 1, paste, collapse=" to "), 
				sapply(cox$coefficients[, "Pr(>|z|)"], formatPValue))
	}
    if (is.null(stitles)){
        stitles <- rownames(cox$conf.int)
    }
	dimnames(tmp) <- list(
        stitles, c("HR", paste(conf.int*100, "% CI", sep=""), "P"))
	return(xtable(tmp, caption = caption, label = label, align = align, vsep = vsep, 
					digits = digits, display = display))
           
}


tableCoxph <- 
    function (x, conf.int = 0.95,
        stitles = NULL, digits = NULL, 
        show.pvalue = TRUE, nsmall = 2) 
{
    formatPValue <- function(p, digits = 2, nsmall=2) {
        if (p<0.001) {
            txt <- "<.001" 
        } else {
            txt <- sub("0.", ".", format(p, digits = digits, nsmall=nsmall))
        }
        return(txt)
    }
    cox <- summary(x, conf.int = conf.int)
    if (nrow(cox$conf.int)==1) {
        tmp <- cbind(
            format(cox$conf.int[, 1], digits=digits, nsmall=nsmall), 
            paste(format(cox$conf.int[, 3:4], digits=digits, nsmall=nsmall), collapse=" to "), 
            sapply(cox$coefficients[, "Pr(>|z|)"], formatPValue))		
    } else {
        tmp <- cbind(
            format(cox$conf.int[, 1], digits=digits, nsmall=nsmall), 
            apply(format(cox$conf.int[, 3:4], digits=digits, nsmall=nsmall), 1, paste, collapse=" to "), 
            sapply(cox$coefficients[, "Pr(>|z|)"], formatPValue))
    }
    if (is.null(stitles)){
        stitles <- rownames(cox$conf.int)
    }
    dimnames(tmp) <- list(
        stitles, c("HR", paste(conf.int*100, "% CI", sep=""), "P"))
    
    if(!show.pvalue){
        tmp <- tmp[, which(colnames(tmp) != "P")]
    }
    return(tmp)
    
}


