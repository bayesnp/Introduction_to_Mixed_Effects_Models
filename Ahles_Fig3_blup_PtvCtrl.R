
#setEPS() # subsequent calls to postscript() should save file in EPS
#postscript("Fig2_blup_PvC.eps", paper="special", horizontal = FALSE, height = 5, width=8)
pdf(file = "Ahles_Fig3.pdf", height = 5, width = 8, paper = "special")
M <- matrix(NA, ncol = 3, nrow = 9)
dimnames(M) <- list(c("Verbal Ability", "Verbal Memory", "Visual Memory", "Working Memory", "Processing Speed", "Sorting", "Distractibility", "Reaction Time", "Global"), c("estimate", "95L", "95U"))
M[1, ] <- c(-0.09243, -0.7885, 0.6037)   ##f1
M[2, ] <- c(-0.04798, -0.9054, 0.8095)   ##f2
M[3, ] <- c(0.03665, -0.8434, 0.9167)    ##f3
M[4, ] <- c(-1.2543, -2.3005, -0.2080)   ##f4
M[5, ] <- c(-0.5396, -1.0147, -0.06437)  ##f5
M[6, ] <- c(-0.2249, -1.3768, 0.9269)    ##f6
M[7, ] <- c(-0.4338, -1.4783, 0.6108)    ##f7
M[8, ] <- c(0.1250, -0.9448, 1.1949)     ##f8
M[9, ] <- c(-0.08789, -0.4968, 0.3210)   ##f9

par(mar = c(6.1, 9.1, 3.1, 1.1)) # margins
plot(x = range(M), y = c(1, 9), type = "n", xlab="", ylab="", xlim = c(-2.3, 2.3), axes=F)
points(x = M[, "estimate"], y = 9:1)
title(main="Patients vs. controls")
segments(x0 = M[, "95L"], x1 = M[, "95U"], y0 = 9:1, y1 = 9:1)
abline(v = 0)
mtext(rownames(M), side = 2, at = 9:1, las = 1, line = 1, adj = 1)
axis(1, at = seq(-2, 2, by=0.5))
mtext("<- Worse in APOE4+ Non-smokers ", cex = .8, side = 1, line = 2.5, at = -2.0, adj = 0)
mtext("Better in APOE4+ Non-smokers ->" , cex = .8, side = 1, line = 2.5, at = 2.0, adj = 1)
mtext("Post-Tx Changes (z-scores)", side = 1, line = 4.2, cex = 1.2)
graphics.off()
