# This is a slightly modified version of the survplot function in the 
# survplot package (version 0.0.7) by Aron Eklund, available at
# http://www.cbs.dtu.dk/~eklund/survplot/
#
# Modification made by John Lövrot to, to some degree, mimic the 
# layout of survival plots in JCO
# *  A table with information about # events and median survival time, 
#    etc. is added to the plot
# * "HR = 2.50 (0.92 - 6.81)" -> "HR = 2.50 (95% CI, 0.92 to 6.81)" 
#    when adding hazard ratio information in the plot 
# * "Number at risk" -> "No at risk" 
#    when indicating number-at-risk for each stratum below the plot
# 
# Small modifications by JL:
# * Plot table with information about # events and median survival time for any number of 
#   strata (not only 2)
# * Position of HR and logrank pvalue
# * Option to choose the exact position of the JCO table (use option hr.pos="customised" and 
#   specify xjust and yjust parameters)
# * change the background color of the JCO table (to #FFFFFF)
# 
# Author(s):
#   Aron Eklund <eklund+survplot@cbs.dtu.dk> 
#   John Lövrot <john.lovrot@ki.se>
#   Julie Lorent <julie.lorent@ki.se>
#
# License: Artistic-2.0
#
# Depends on the "plotrix", "survival" and "survplot" packages
# 


survplotJCOJL <- function(x, 
        data = NULL, 
        subset = NULL,  
        snames, 
        stitle, 
        col, 
        lty, 
        lwd,
        show.nrisk = TRUE, 
        color.nrisk = TRUE,
        show.JCOtable = TRUE,
        hr.pos = 'topright', 
        legend.pos = 'bottomleft',
        JCOtable.pos = 'left',
        show.pvalue = TRUE, 
        alternative = "two.sided", 
        conf.int = 0.95, 
        xjust, 
        yjust, 
        mark.time=TRUE, ...) {
    
    require(plotrix)
    
    formatPValue <- function(p, digits = 1, nsmall = 2) {
        if (p < 0.001) {
            expr <- bquote(italic("P") ~ " < " ~ ".001")
        } else {
            expr <- bquote(
                    italic("P") ~ " = " ~ .(sub(
                                    '^(-)?0[.]', '\\1.',
                                    format(
                                            p, 
                                            digits = digits, 
                                            nsmall = nsmall))))
        }
        return(expr)
    }
    
    eval(bquote(s <- survfit(x, data = data, subset = .(substitute(subset)))))
    if('strata' %in% names(s)) {
        if(missing(stitle)) stitle <- strsplit(deparse(x), " ~ ")[[1]][2]
        if(missing(snames)) {
            snames <- names(s$strata)
            prefx <- paste(strsplit(deparse(x), " ~ ")[[1]][2], '=', sep = '')
            if(all(substr(snames, 1, nchar(prefx)) == prefx)) {
                snames <- substr(snames, nchar(prefx) + 1, 100)
            }
        }
        ns <- length(s$strata)
        stopifnot(length(snames) == ns)
    } else {    # no strata
        ns <- 1
        snames <- NA
        legend.pos <- NA
    }
    if(show.nrisk) {
        mar <- par('mar')
        mar[1] <- mar[1] + ns
        mar[2] <- mar[2] + 4
        opar <- par(mar = mar)
        on.exit(par(opar))
    }
    if(missing(col)) col <- 1:ns
    if(missing(lty)) lty <- 1
    if(missing(lwd)) lwd <- par('lwd')
    plot(s, col = col, lty = lty, lwd = lwd, mark.time = mark.time, ...)
    
    if(length(legend.pos) > 1 || !is.na(legend.pos)) {
        legend(legend.pos, legend = snames, title = stitle,
                col = col, lty = lty, lwd = lwd, bty = 'n')
    }
    if(ns == 1) { 
        st <- summary(s)$table  
        tmp <- matrix(
                data = c(snames,
                        paste(st["events"], st["n.start"], sep=" / "),
                        format(round(st["events"]/st["n.start"]*100))), 
                nrow = ns, 
                ncol = 3, 
                byrow = FALSE, 
                dimnames = list(
                        NULL,
                        c("", "Events / N", "%")))
    } else { 
        st <-  as.data.frame(summary(s)$table)
        tmp <- matrix(
                data = c(snames,
                        paste(st$events, st$n.start, sep=" / "),
                        format(round(st$events/st$n.start*100))), 
                nrow = ns, 
                ncol = 3, 
                byrow = FALSE, 
                dimnames = list(
                        NULL,
                        c("", "Events / N", "%")))
        eval(bquote(cox <- summary(coxph(x, data = data, subset = .(substitute(subset))), conf.int=conf.int)))
        if (alternative=="two.sided") {
            p <- formatPValue(cox$sctest[3])
        } else {	
            z <- sign(cox$coefficients[1, 1])*sqrt(cox$sctest[1])
            if (alternative=="less") {
                p <- formatPValue(pnorm(z))
            } else { # "greater"
                p <- formatPValue((1 - pnorm(z)))
            }
        }
        txt2 <- bquote(.(paste(ifelse(alternative=="two.sided", '', 'One-sided '), 'log-rank ', sep = "")) ~ .(p))
        if (ns == 2) {
            hr <- format(cox$conf.int[1, c(1, 3, 4)], digits = 2)
            ci <- format(100*conf.int)
            
            txt1 <- paste('HR = ', hr[1], ' (', ci, '% CI, ', hr[2], ' to ', hr[3], ')', sep = '')
            
            if(show.pvalue) { 
                legend(hr.pos, legend = sapply(c(bquote(.(txt1)), bquote(.(txt2))), as.expression), bty = 'n', y.intersp = 0.8)
                ##legend(x=max(s$time), y=0.85, legend = c(txt1, txt2), bty = 'n',
                ##xjust = 1, yjust = 1, cex = 0.9)
            } else {
                legend(hr.pos, legend = txt1, bty = 'n', y.intersp = 0.8)
                ##legend(x=max(s$time), y=0.85, legend = txt1, bty = 'n',
                ##	xjust = 1, yjust = 1, cex = 0.9)
            }
        } else {
            if(show.pvalue) { 
                legend(hr.pos, legend = bquote(.(txt2)), bty = 'n', y.intersp = 0.8)
                ##legend(x=max(s$time), y=0.85, legend = c(txt1, txt2), bty = 'n',
                ##xjust = 1, yjust = 1, cex = 0.9)
            }
        }
    }
    
    if(show.JCOtable) {
        if (JCOtable.pos=="left") {
            addtable2plot(min(s$time), 0.5, table = tmp, xjust = 0, yjust = 0, bty = "o", xpad = 0.3, ypad =0.5,
                    box.col = "black", hlines = TRUE, vlines = TRUE)
        }
        if (JCOtable.pos=="bottomleft") {
            addtable2plot(min(s$time), 0, table = tmp, xjust = 0, yjust = 1, bty = "o", xpad = 0.3, ypad =0.5,
                    box.col = "black", hlines = TRUE, vlines = TRUE)
        }
        if (JCOtable.pos=="bottomright") {
            addtable2plot(max(s$time), 0, table = tmp, xjust = 1, yjust = 1, bty = "o", xpad = 0.3, ypad =0.5,
                    box.col = "black", hlines = TRUE, vlines = TRUE)
        }
        if (JCOtable.pos=="customised") {
            addtable2plot(xjust, yjust, table = tmp, xjust = 0, yjust = 0, bty = "o", xpad = 0.3, ypad =0.5,
                    box.col = "black", hlines = TRUE, vlines = TRUE)
        }	
    }
    
    if(show.nrisk) {
        addNriskJL(s, labels = snames, col = if(color.nrisk) col else 1, title = "No. at risk")
    }
    if(ns == 2) return(invisible(c(txt1, txt2)))
}


