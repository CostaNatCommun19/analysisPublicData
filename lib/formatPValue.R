

formatPValue <- function(p, digits = 1, nsmall = 2) {
	if (p < 0.001) {
		txt <- "< .001"
	} else {
		txt <- format(
				p, 
				digits = digits, 
				nsmall = nsmall)
	}
	return(txt)
}
