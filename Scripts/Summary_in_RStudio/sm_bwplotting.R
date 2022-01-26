## Charlesworth, Yang, Mann, Kurdi, Banaji
# Gender stereotypes in natural language

## Script date: July 12th, 2020

################################################################################

## Additional code to be run AFTER running devembeddings_summary.R (all objects for plotting need to be in Global environment)

################################################################################

## STUDY ONE----
metadatsum2 <- metadatsum[1:4,]
metadatsum2 # mfgb, mfwh, mfsa, mfmr
metadat.plot <- metadat[metadat$cat != "weapons vs. instruments",] # Remove instruments/weapons for main plot
metadat.plot$att2 <- factor(metadat.plot$att, 
                            levels = c("bad vs. good", "work vs. home", "science vs. arts", "math vs. reading"))
metadat.plot <- arrange(metadat.plot, att2)

## All effects but with BW color scheme
pdf(file = "bw_plot.pdf", width = 15, height = 10)
op <- par(mar = c(7,7,2,2))
plot(1, type="n", xlab="", ylab="", 
     xlim=c(0.5, 4.5), ylim=c(-2.25, 2.25), axes = FALSE)
box()
mtext("WEAT Effect Size", side = 2, line = 5, cex = 2.5)
axis(1, at = c(1:4), labels = c("Male-Bad\nFemale-Good", "Male-Work\nFemale-Home", 
                                "Male-Science\nFemale-Arts", "Male-Math\nFemale-Reading"),
     cex.axis = 2, padj = 1)
axis(2, at = seq(-2,2, by = 0.5), labels = seq(-2,2, by = 0.5), las = 2, cex.axis = 2)
abline(h = 0, lty = 2)
abline(v = c(1.5, 2.5, 3.5), lty = 2, lwd = 2.5)

points(seq(1, 4, by = 1),
       metadatsum2$metamean, 
       pch = 17, cex = 3, col = "black")
segments(x0 = seq(1, 4, by = 1), x1 = seq(1, 4, by = 1),
         y0 = metadatsum2$meta95low,
         y1 = metadatsum2$meta95high, 
         lty = 1, lwd = 3, col = "black")

points(seq(0.6, 3.6, by = 1),
       metadat.plot$eff.correct[metadat.plot$data=="childes_children"], 
       pch = 4, lwd = 4, cex = 2, col = "darkgrey")
segments(x0 = seq(0.6, 3.6, by = 1), x1 = seq(0.6, 3.6, by = 1),
         y0 = metadat.plot$eff.correct[metadat.plot$data=="childes_children"] - 1.96*metadat.plot$se[metadat.plot$data=="childes_children"],
         y1 = metadat.plot$eff.correct[metadat.plot$data=="childes_children"] + 1.96*metadat.plot$se[metadat.plot$data=="childes_children"], 
         lty = 1, lwd = 3, col = "darkgrey")

points(seq(0.7, 3.7, by = 1),
       metadat.plot$eff.correct[metadat.plot$data=="childes_parents"], 
       pch = 16, cex = 3.5, col = "darkgrey")
segments(x0 = seq(0.7, 3.7, by = 1), x1 = seq(0.7, 3.7, by = 1),
         y0 = metadat.plot$eff.correct[metadat.plot$data=="childes_parents"] - 1.96*metadat.plot$se[metadat.plot$data=="childes_parents"],
         y1 = metadat.plot$eff.correct[metadat.plot$data=="childes_parents"] + 1.96*metadat.plot$se[metadat.plot$data=="childes_parents"], 
         lwd = 3.5, col = "darkgrey", lty = 2)
points(seq(0.8, 3.8, by = 1),
       metadat.plot$eff.correct[metadat.plot$data=="child_books"], 
       pch = 18, cex = 3.5, col = "darkgrey")
segments(x0 = seq(0.8, 3.8, by = 1), x1 = seq(0.8, 3.8, by = 1),
         y0 = metadat.plot$eff.correct[metadat.plot$data=="child_books"] - 1.96*metadat.plot$se[metadat.plot$data=="child_books"],
         y1 = metadat.plot$eff.correct[metadat.plot$data=="child_books"] + 1.96*metadat.plot$se[metadat.plot$data=="child_books"], 
         lwd = 3.5, col = "darkgrey", lty = 3)
points(seq(0.9, 3.9, by = 1),
       metadat.plot$eff.correct[metadat.plot$data=="kids_tv_combined"], 
       pch = 15, cex = 3.5, col = "darkgrey")
segments(x0 = seq(0.9, 3.9, by = 1), x1 = seq(0.9, 3.9, by = 1),
         y0 = metadat.plot$eff.correct[metadat.plot$data=="kids_tv_combined"] - 1.96*metadat.plot$se[metadat.plot$data=="kids_tv_combined"],
         y1 = metadat.plot$eff.correct[metadat.plot$data=="kids_tv_combined"] + 1.96*metadat.plot$se[metadat.plot$data=="kids_tv_combined"], 
         lwd = 3.5, col = "darkgrey", lty = 5)

points(seq(1.1, 4.1, by = 1),
       metadat.plot$eff.correct[metadat.plot$data=="adult_speech"], 
       pch = 1, cex = 3, lwd = 4, col = "lightgray")
segments(x0 = seq(1.1, 4.1, by = 1), x1 = seq(1.1, 4.1, by = 1),
         y0 = metadat.plot$eff.correct[metadat.plot$data=="adult_speech"] - 1.96*metadat.plot$se[metadat.plot$data=="adult_speech"],
         y1 = metadat.plot$eff.correct[metadat.plot$data=="adult_speech"] + 1.96*metadat.plot$se[metadat.plot$data=="adult_speech"], 
         lwd = 3.5, col = "lightgray", lty = 2)
points(seq(1.2, 4.2, by = 1),
       metadat.plot$eff.correct[metadat.plot$data=="gutenberg"], 
       pch = 5, cex = 3, lwd = 4, col = "lightgray")
segments(x0 = seq(1.2, 4.2, by = 1), x1 = seq(1.2, 4.2, by = 1),
         y0 = metadat.plot$eff.correct[metadat.plot$data=="gutenberg"] - 1.96*metadat.plot$se[metadat.plot$data=="gutenberg"],
         y1 = metadat.plot$eff.correct[metadat.plot$data=="gutenberg"] + 1.96*metadat.plot$se[metadat.plot$data=="gutenberg"], 
         lwd = 3.5, col = "lightgray", lty = 3)
points(seq(1.3, 4.3, by = 1),
       metadat.plot$eff.correct[metadat.plot$data=="simply_scripts"], 
       pch = 0, lwd = 4, cex = 3, col = "lightgray")
segments(x0 = seq(1.3, 4.3, by = 1), x1 = seq(1.3, 4.3, by = 1),
         y0 = metadat.plot$eff.correct[metadat.plot$data=="simply_scripts"] - 1.96*metadat.plot$se[metadat.plot$data=="simply_scripts"],
         y1 = metadat.plot$eff.correct[metadat.plot$data=="simply_scripts"] + 1.96*metadat.plot$se[metadat.plot$data=="simply_scripts"], 
         lwd = 3.5, col = "lightgray", lty = 5)

legend("bottom", c("Child-produced speech", "Child-directed speech",
                       "Child-directed books", "Child-directed media",
                       "Adult-produced speech", "Adult-directed books", 
                       "Adult-directed media", "Meta Estimate"),
       col = c(rep("darkgrey", 4), rep("lightgray", 3), "black"),
       pch = c(4, 16, 18, 15, 1, 5, 0, 17), lwd = 4, lty = c(1, 2, 3, 5, 2, 3, 5, 1),
       cex = 1.7, pt.cex = c(3, rep(3.5, 3), rep(3, 3), 3), bg = "white", ncol = 2)

par(op)
dev.off()


# STUDY TWO ----
# "mischievous" (effect size ~ -0.1) and "creative" (effect size ~ 0.1)
# relaxed (-0.2) and verbal (0.2)
# jealous (-0.3) and stable (0.3)

# first page
pdf(file = "metatrait_plot_efver_bw_page1.pdf", width = 45, height = 15)
op <- par(mar = c(18, 10, 2, 2))
plot(meta.groups.traitsum$mean[order(meta.groups.traitsum$mean, decreasing = TRUE)][1:85], ylim = c(-1.2, 0.8),
     xlab = "", ylab = "", axes = FALSE,
     pch = 20, cex = 4, col = "gray")
box()
axis(1, at = 1:85, 
     labels = meta.groups.traitsum$att[order(meta.groups.traitsum$mean, decreasing = TRUE)][1:85], las = 2, cex.axis = 3)
axis(2, at = seq(-1.4, 1.4, by = 0.2), labels = round(seq(-1.4, 1.4, by = 0.2), 2), las = 1, cex.axis = 3)
mtext("Meta-analytic Effect (Trait = Male)", line = 7, side = 2, cex = 3)
abline(h = 0, lty = 2, col = "darkgray", lwd = 4)
segments(x0 = c(1:85), x1 = c(1:85), 
         y0 = meta.groups.traitsum$mean[order(meta.groups.traitsum$mean, decreasing = TRUE)][1:85] - 1.96*meta.groups.traitsum$se[order(meta.groups.traitsum$mean, decreasing = TRUE)][1:85],
         y1 = meta.groups.traitsum$mean[order(meta.groups.traitsum$mean, decreasing = TRUE)][1:85] + 1.96*meta.groups.traitsum$se[order(meta.groups.traitsum$mean, decreasing = TRUE)][1:85],
         lty = 1, lwd = 3, col = "gray")
abline(v = which(meta.groups.traitsum$att[order(meta.groups.traitsum$mean, decreasing = TRUE)] == "stable"), 
       col = "black", lty = 3, lwd = 7)
abline(v = which(meta.groups.traitsum$att[order(meta.groups.traitsum$mean, decreasing = TRUE)] == "jealous"),
       col = "black", lty = 3, lwd = 7)
abline(v = which(meta.groups.traitsum$att[order(meta.groups.traitsum$mean, decreasing = TRUE)] == "verbal"), 
       col = "black", lty = 2, lwd = 7)
abline(v = which(meta.groups.traitsum$att[order(meta.groups.traitsum$mean, decreasing = TRUE)] == "relaxed"),
       col = "black", lty = 2, lwd = 7)
abline(v = which(meta.groups.traitsum$att[order(meta.groups.traitsum$mean, decreasing = TRUE)] == "creative"), 
       col = "black", lty = 1, lwd = 7)
abline(v = which(meta.groups.traitsum$att[order(meta.groups.traitsum$mean, decreasing = TRUE)] == "mischievous"),
       col = "black", lty = 1, lwd = 7)
legend("bottomleft", c("[-0.1, 0.1]", "[-0.2, 0.2]", "[-0.3, 0.3]"), title = "Effects beyond",
       col = "black", lty = c(1, 2, 3), lwd = 5, bty = "n", cex = 3.5)
par(op)
dev.off()

# second page
pdf(file = "metatrait_plot_efver_bw_page2.pdf", width = 45, height = 15)
op <- par(mar = c(18, 10, 2, 2))
plot(meta.groups.traitsum$mean[order(meta.groups.traitsum$mean, decreasing = TRUE)][86:170], ylim = c(-1.2, 0.8),
     xlab = "", ylab = "", axes = FALSE,
     pch = 20, cex = 4, col = "gray")
box()
axis(1, at = 1:85, 
     labels = meta.groups.traitsum$att[order(meta.groups.traitsum$mean, decreasing = TRUE)][86:170], las = 2, cex.axis = 3)
axis(2, at = seq(-1.4, 1.4, by = 0.2), labels = round(seq(-1.4, 1.4, by = 0.2), 2), las = 1, cex.axis = 3)
mtext("Meta-analytic Effect (Trait = Male)", line = 7, side = 2, cex = 3)
abline(h = 0, lty = 2, col = "darkgray", lwd = 4)
segments(x0 = c(1:85), x1 = c(1:85), 
         y0 = meta.groups.traitsum$mean[order(meta.groups.traitsum$mean, decreasing = TRUE)][86:170] - 1.96*meta.groups.traitsum$se[order(meta.groups.traitsum$mean, decreasing = TRUE)][86:170],
         y1 = meta.groups.traitsum$mean[order(meta.groups.traitsum$mean, decreasing = TRUE)][86:170] + 1.96*meta.groups.traitsum$se[order(meta.groups.traitsum$mean, decreasing = TRUE)][86:170],
         lty = 1, lwd = 3, col = "gray")
abline(v = which(meta.groups.traitsum$att[order(meta.groups.traitsum$mean, decreasing = TRUE)][86:170] == "stable"), 
       col = "black", lty = 3, lwd = 7)
abline(v = which(meta.groups.traitsum$att[order(meta.groups.traitsum$mean, decreasing = TRUE)][86:170] == "jealous"),
       col = "black", lty = 3, lwd = 7)
abline(v = which(meta.groups.traitsum$att[order(meta.groups.traitsum$mean, decreasing = TRUE)][86:170] == "verbal"), 
       col = "black", lty = 2, lwd = 7)
abline(v = which(meta.groups.traitsum$att[order(meta.groups.traitsum$mean, decreasing = TRUE)][86:170] == "relaxed"),
       col = "black", lty = 2, lwd = 7)
abline(v = which(meta.groups.traitsum$att[order(meta.groups.traitsum$mean, decreasing = TRUE)][86:170] == "creative"), 
       col = "black", lty = 1, lwd = 7)
abline(v = which(meta.groups.traitsum$att[order(meta.groups.traitsum$mean, decreasing = TRUE)][86:170] == "mischievous"),
       col = "black", lty = 1, lwd = 7)
legend("bottomleft", c("[-0.1, 0.1]", "[-0.2, 0.2]", "[-0.3, 0.3]"), title = "Effects beyond",
col = "black", lty = c(1, 2, 3), lwd = 5, bty = "n", cex = 3.5)
par(op)
dev.off()


## STUDY THREE ----
pdf(file = "metaprof_plot_efver_bw_page1.pdf", width = 45, height = 15)
op <- par(mar = c(18, 10, 2, 2))
plot(meta.profsum$mean[order(meta.profsum$mean, decreasing = TRUE)][1:41], ylim = c(-1.6, 1.6),
     xlab = "", ylab = "", axes = FALSE,
     pch = 20, cex = 4, col = "gray")
box()
axis(1, at = 1:41, 
     labels = meta.profsum$att[order(meta.profsum$mean, decreasing = TRUE)][1:41], las = 2, cex.axis = 3)
axis(2, at = seq(-1.6, 1.6, by = 0.4), labels = round(seq(-1.6, 1.6, by = 0.4), 2), las = 1, cex.axis = 3)
mtext("Meta-analytic Effect (Occupation = Male)", line = 7, side = 2, cex = 3)
abline(h = 0, lty = 2, col = "gray", lwd = 4)
segments(x0 = c(1:41), x1 = c(1:41), 
         y0 = meta.profsum$mean[order(meta.profsum$mean, decreasing = TRUE)][1:41] - 1.96*meta.profsum$se[order(meta.profsum$mean, decreasing = TRUE)][1:41],
         y1 = meta.profsum$mean[order(meta.profsum$mean, decreasing = TRUE)][1:41] + 1.96*meta.profsum$se[order(meta.profsum$mean, decreasing = TRUE)][1:41],
         lty = 1, lwd = 3, col = "gray")
abline(v = which(meta.profsum$att[order(meta.profsum$mean, decreasing = TRUE)][1:41] == "writer"), 
       col = "black", lty = 1, lwd = 7)
abline(v = which(meta.profsum$att[order(meta.profsum$mean, decreasing = TRUE)][1:41] == "designer"),
       col = "black", lty = 1, lwd = 7)
abline(v = which(meta.profsum$att[order(meta.profsum$mean, decreasing = TRUE)][1:41] == "painter"), 
       col = "black", lty = 2, lwd = 7)
abline(v = which(meta.profsum$att[order(meta.profsum$mean, decreasing = TRUE)][1:41] == "grader"),
       col = "black", lty = 2, lwd = 7)
abline(v = which(meta.profsum$att[order(meta.profsum$mean, decreasing = TRUE)][1:41] == "hairdresser"), 
       col = "black", lty = 3, lwd = 7)
abline(v = which(meta.profsum$att[order(meta.profsum$mean, decreasing = TRUE)][1:41] == "psychologist"),
       col = "black", lty = 3, lwd = 7)
legend("bottomleft", c("[-0.1, 0.1]", "[-0.2, 0.2]", "[-0.3, 0.3]"), title = "Effects beyond",
       col = "black", lty = c(1, 2, 3), lwd = 5, bty = "n", cex = 4)
par(op)
dev.off()

## second page
pdf(file = "metaprof_plot_efver_bw_page2.pdf", width = 45, height = 15)
op <- par(mar = c(18, 10, 2, 2))
plot(meta.profsum$mean[order(meta.profsum$mean, decreasing = TRUE)][42:82], ylim = c(-1.6, 1.6),
     xlab = "", ylab = "", axes = FALSE,
     pch = 20, cex = 4, col = "gray")
box()
axis(1, at = 1:41, 
     labels = meta.profsum$att[order(meta.profsum$mean, decreasing = TRUE)][42:82], las = 2, cex.axis = 3)
axis(2, at = seq(-1.6, 1.6, by = 0.4), labels = round(seq(-1.6, 1.6, by = 0.4), 2), las = 1, cex.axis = 3)
mtext("Meta-analytic Effect (Occupation = Male)", line = 7, side = 2, cex = 3)
abline(h = 0, lty = 2, col = "gray", lwd = 4)
segments(x0 = c(1:41), x1 = c(1:41), 
         y0 = meta.profsum$mean[order(meta.profsum$mean, decreasing = TRUE)][42:82] - 1.96*meta.profsum$se[order(meta.profsum$mean, decreasing = TRUE)][42:82],
         y1 = meta.profsum$mean[order(meta.profsum$mean, decreasing = TRUE)][42:82] + 1.96*meta.profsum$se[order(meta.profsum$mean, decreasing = TRUE)][42:82],
         lty = 1, lwd = 3, col = "gray")
abline(v = which(meta.profsum$att[order(meta.profsum$mean, decreasing = TRUE)][42:82] == "writer"), 
       col = "black", lty = 1, lwd = 7)
abline(v = which(meta.profsum$att[order(meta.profsum$mean, decreasing = TRUE)][42:82] == "designer"),
       col = "black", lty = 1, lwd = 7)
abline(v = which(meta.profsum$att[order(meta.profsum$mean, decreasing = TRUE)][42:82] == "painter"), 
       col = "black", lty = 2, lwd = 7)
abline(v = which(meta.profsum$att[order(meta.profsum$mean, decreasing = TRUE)][42:82] == "grader"),
       col = "black", lty = 2, lwd = 7)
abline(v = which(meta.profsum$att[order(meta.profsum$mean, decreasing = TRUE)][42:82] == "hairdresser"), 
       col = "black", lty = 3, lwd = 7)
abline(v = which(meta.profsum$att[order(meta.profsum$mean, decreasing = TRUE)][42:82] == "psychologist"),
       col = "black", lty = 3, lwd = 7)
legend("bottomleft", c("[-0.1, 0.1]", "[-0.2, 0.2]", "[-0.3, 0.3]"), title = "Effects beyond",
       col = "black", lty = c(1, 2, 3), lwd = 5, bty = "n", cex = 4)
par(op)
dev.off()


