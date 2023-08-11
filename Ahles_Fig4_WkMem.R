
# setEPS() # subsequent calls to postscript() should save file in EPS
# postscript("Fig3_WkMem.eps", paper="special", width = 8, height=11)

pdf("Ahles_Fig4_WkMem.pdf", paper="letter", width = 8, height=11)
###
# Working Memory
###
par(mar=c(3.5,4.0,1.1,.1))
tn <- list(c("Ever Smoked", "Never Smoked"), c("E4 present", "E4 absent"), 
           c("chemo", "no.chemo", "control"))
ylim <- c(-.44, 2.0)
# [present,smoked] [present, no] [absent, smoked] [absent, no]
M <- array(NA, unlist(lapply(tn, length)), dimnames=tn)
M[, , "chemo"] <- c(0.638,.521,.471,.317)
M[, , "no.chemo"] <- c(.813,.239,.451,.564)
M[, , "control"] <- c(-.071,1.223,.519,.650)
# Bayesian model-predicted means from the LSMEANS e4_allele*smoking2*txgrp
# [present,smoked] [present, no] [absent, smoked] [absent, no]
Mpred <- array(NA, unlist(lapply(tn, length)), dimnames=tn)
Mpred[, , "chemo"] <- c(0.156, -0.152, 0.080, 0.216)
Mpred[, , "no.chemo"] <- c(0.167, 0.023, 0.083, 0.207)
Mpred[, , "control"] <- c(0.219, 0.400, 0.248, 0.262)

# 95% CI from PROC GENMOD LSMEANS e4_allele*smoking2*txgrp
MeL <- array(NA, unlist(lapply(tn, length)), dimnames=tn)
MeL[, , "chemo"] <- c(.3953,.1833,.1732,.1877)
MeL[, , "no.chemo"] <- c(.3275,-.1665,.09259,.3363)
MeL[, , "control"] <- c(-.40,.3121,.3590,.4204)

MeU <- array(NA, unlist(lapply(tn, length)), dimnames=tn)
MeU[, , "chemo"] <- c(1.1130,1.0525,.6131,.6717)
MeU[, , "no.chemo"] <- c(.9830,.7287,.5058,.8020)
MeU[, , "control"] <- c(.5774,1.7878,.8138,.9705)
##
# set up observed means, model-predicted lower and upper bounds
##
# tcol <- c("grey100", "grey80")
# tcol <- c("#FFA200", "skyblue", "green", "red")
tcol <- c("skyblue4", "skyblue", "goldenrod3", "gold")
tden <- c(-1, -1, -1, -1)
tang <- c(0, 0, 0, 0) 
M <- matrix(M, nrow = 4)
Mpred <- matrix(Mpred, nrow = 4)
MeU <- matrix(MeU, nrow = 4)
MeL <- matrix(MeL, nrow = 4)
# plot(c(0, 16), c(-0.42, 1), axes = F, type="n")
tb <- barplot(M, ylim=ylim, axes=F, axisnames=F, beside=T, col=tcol, density=tden, angle = tang)
abline(h = 0)
#
# [chemo, present, never smoked] bar has to be plotted separately
# because it is below zero.
#
segments(x0 = tb, y0 = MeL, x1 = tb, y1 = MeU) # all others
segments(x0 = tb-0.03, y0 = MeU, x1 = tb+0.03, y1 = MeU) # upper whiskers
segments(x0 = tb-0.03, y0 = MeL, x1 = tb+0.03, y1 = MeL) # lower whiskers
mtext(c("Chemotherapy", "No\nChemotherapy", "Control"), side = 1, line = 1.5, at = apply(tb, 2, mean), cex = 1.2)
mtext(c("(8)","(6)", "(23)","(18)","(11)","(7)","(32)","(18)","(4)","(3)","(22)","(18)"), side = 1, at=tb, line = 2.5, cex = 0.9)
# mark the clusters of bars at the bottom of graph
segments(tb[1], -.43, tb[4], -.43)     # chemo cluster of bars
segments(tb[c(1,4)], -.43, tb[c(1,4)], -.41)
#segments(mean(tb[c(1,4)]), -.43, mean(tb[c(1,4)]), -.45)

segments(tb[5], -.43, tb[8], -.43)     # no.chemo cluster of bars
segments(tb[c(5,8)], -.43, tb[c(5,8)], -.41)
#segments(mean(tb[c(5,8)]), -.43, mean(tb[c(5,8)]), -.45)

segments(tb[9], -.43, tb[12], -.43)     # control cluster of bars
segments(tb[c(9,12)], -.43, tb[c(9,12)], -.41)
#segments(mean(tb[c(9,12)]), -.43, mean(tb[c(9,12)]), -.45)

axis(2, at=seq(-.40, 1.8, by = 0.20), las = 1, cex = .8)
mtext("Standardized Change in Working Memory (z-scores)", side = 2, line = 3., cex = 1.2)
###
# p-values
###
tx <- cbind(apply(tb[1:2, ], 2, mean), apply(tb[3:4, ], 2, mean))
y0 <- 1.4
y1 <- 1.5
# white polygon to mask tall tb[6] bar
polygon(x=c(tb[10]-.15,tb[10]+.15,tb[10]+.15,tb[10]-.15),
   y = c(y0-.1, y0-.1, y1+0.15, y1+0.15), density=-1, col="white", border=NA)
# dotted line to show hidden error for tallest bar
segments(x0=tb[10], y0=y0-.1, x1=tb[10], y1=y0, lty = 2) # beneath p-value
segments(x0=tb[10], y0=y1, x1=tb[10], y1=MeU[2,3]-.1, lty = 2) # above

segments(tx[, 1], y1, tx[, 2], y1)  # horizontal top: chemo, no, ctrl
segments(tx[, 1], y0, tx[, 1], y1)  # left verti: chemo, no, ctrl
segments(tx[, 2], y0, tx[, 2], y1)  # right verti: chemo, no, ctrl

text("p = 0.6021", x = mean(tb[2:3]), y = y1-.05, cex = .8)
text("(a)", x = mean(tb[2:3]), y = y0, cex = .8)
text("p = 0.0445", x = mean(tb[6:7]), y = y1-.05, cex = .8)
text("(b)", x = mean(tb[6:7]), y =y0, cex = .8)
text("p = 0.0818", x = mean(tb[10:11]), y = y1-.05, cex = .8)
text("(c)", x = mean(tb[10:11]), y = y0, cex = .8)

y0 <- y0 + .11
y1 <- y1 + .05
segments(mean(tb[6:7]), y1, mean(tb[10:11]), y1)  # top: no_chemo vs. ctrl
segments(mean(tb[6:7]), y0, mean(tb[6:7]), y1)    # left verti
segments(mean(tb[10:11]), y0, mean(tb[10:11]), y1)# right verti
text("p = 0.0112", x = mean(tb[7:10]), y = y0, cex = .8)
text("(e)", x = mean(tb[7:10]), y = y0-.05, cex = .8)

y0 <- y0 + .05
y1 <- y1 + .05
segments(mean(tb[2:3]), y1, mean(tb[10:11]), y1)  # top: chemo vs. ctrl
segments(mean(tb[2:3]), y0-.05, mean(tb[2:3]), y1)    # left verti
segments(mean(tb[10:11]), y0, mean(tb[10:11]), y1)# right verti
text("p = 0.0793 (d)", x = mean(tb[3:10]), y = y1+.03, cex = .8)

###
# legend 
###
tx <- c(tb[1]-.5, tb[1]+.5, tb[1]+.5, tb[1]-.5)
ty <- c(2.0, 2.0, 1.95, 1.95)  # legend y-positions
polygon(x=tx, y=ty, col=tcol[1])
polygon(x=tx, y=ty-.05, col=tcol[2])
text(c("E4+, Ever Smoked","E4+, Never Smoked"), x=tb[2]-.3, y=c(ty[1]-.025,ty[3]-.025), adj=0, cex = .8)

tx <- c(tb[5]-.5, tb[5]+.5, tb[5]+.5, tb[5]-.5)
polygon(x=tx, y=ty, col=tcol[3], density=tden[3], angle=tang[3])
polygon(x=tx, y=ty)  # repeat for black box
polygon(x=tx, y=ty-.05, col=tcol[4], density=tden[4], angle=tang[4])
polygon(x=tx, y=ty-.05)
text(c("E4-, Ever Smoked","E4-, Never Smoked"), x=tb[6]-.3, y=c(ty[1],ty[3])-.025, adj=0, cex = .8)

# rm(tcol, M, tden, MeU, MeL, tx, tb)
