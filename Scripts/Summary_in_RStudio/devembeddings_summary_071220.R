
## Charlesworth, Yang, Mann, Kurdi, Banaji
# Gender stereotypes in natural language

## Script date: July 12th, 2020

################################################################################
# Install and load packages
if(!require("meta")){install.packages("meta", dependencies = TRUE); require("meta")}
if(!require("scales")){install.packages("scales", dependencies = TRUE); require("scales")}
if(!require("car")){install.packages("car", dependencies = TRUE); require("car")}
if(!require("Hmisc")){install.packages("Hmisc", dependencies = TRUE); require("Hmisc")}
if(!require("dplyr")){install.packages("dplyr", dependencies = TRUE); require("dplyr")}
if(!require("ggplot2")){install.packages("ggplot2", dependencies = TRUE); require("ggplot2")}
if(!require("ggrepel")){install.packages("ggrepel", dependencies = TRUE); require("ggrepel")}
if(!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE); require("tidyverse")}


# Set your working directory (location of the data)
setwd("C://Users/Dell/Final_fastText_Word_Vectors") 

# If using, load in the .RData summary object
source("devembeddings_summary_071220.R")
?load
################################################################################

# Read in WEAT results (if you didn't load the summary object)
embdata <- read.csv("embeddings_results_all_.csv")
names(embdata) <- c("dataset", "categories", "attributes",
                    "effsize", "miss", "pleft", "pright", "ptot",
                    "se", "coh_cat", "coh_att")

################################################################################

# (1) STUDY ONE: Summary ----
# Effects for two-category results across datasets

# 5 results:
# (a) M vs. F Good vs. Bad => Multiply by -1 to get the expected direction (Male-Bad, Female-Good) - see below
# (b) M vs. F Home vs. Work => Multiply by -1 to get the expected direction (Male-Work, Female-Home)
# (c) M vs. F Reading vs. Math => Multiply by -1 to get the expected direction (Male-Math, Female-Reading)
# (d) M vs. F Science vs. Arts
# (e) And control: Instruments vs. Weapons Good vs. Bad

table(embdata$dataset)

# Primary datasets, that have cohesive attribute and categories (more cohesive than chance):
# (a) childes_children (only children's speech)
# (b) childes_parents (only parents' speech)
# (c) adult_speech (from the Talkbank corpus: naturalistic adult-only speech)
# (d) child_books (from the Facebook Children's Books corpus)
# (e) gutenberg (Adult books from Project Gutenberg)
# (f) kids_tv_combined (Children's TV, from Nickelodeon, PBS, and Disney)
# (g) simply_scripts (Adult's TV)

# (1a) Data preparation ----
# For all double differences
table(embdata$categories)

embdoub <- embdata[embdata$categories=="female vs. male" | embdata$categories=="male vs. female" ,]
embdoub <- embdoub[grepl("vs.", embdoub$attributes)==TRUE,]
embdoub <- embdoub[c("dataset", "categories", "attributes", "effsize", "se", "ptot")]
colnames(embdoub) <- c("data", "cat", "att", "eff", "se", "p")

# now add on the instruments-weapons effect sizes
instweap <- embdata[embdata$categories=="instruments vs. weapons" & grepl("vs.", embdata$attributes)==TRUE,]
instweap <- instweap[c("dataset", "categories", "attributes", "effsize", "se", "ptot")]
colnames(instweap) <- c("data", "cat", "att", "eff", "se", "p")

embdoub <- rbind(embdoub, instweap)

# To get all the expected effects in the positive direction...
# (a) Switch around the order of the labels for the double-difference scores
# The first attribute should be the one stereotypically-associated with males
# Should be "bad vs. good," "work vs. home," "math vs. reading," "science vs. arts" (already in the correct direction)
# (b) Then multiply the first four double-differences by -1
table(embdoub$att)
embdoub$att <- factor(embdoub$att, labels = c("bad vs. good", "work vs. home", "math vs. reading", "science vs. arts"))
table(embdoub$cat)
embdoub$cat <- factor(embdoub$cat, labels = c("weapons vs. instruments", "male vs. female"))


# And correct the d-scores to align with positive = expected order.
# Multiply by -1 for all double-differences except instrumen/weapons and arts/science
embdoub$eff.correct <- ifelse(grepl("science", embdoub$att)==TRUE | grepl("weapons", embdoub$cat)==TRUE, 
                              embdoub$eff, embdoub$eff*-1)
head(embdoub)


## (1b) SM: supplementary descriptives for instweap ----
embdoub[embdoub$cat == "weapons vs. instruments",]
# Note: Results are reported in supplementary materials (SM Table S3)

# (2) Meta analysis of all effect sizes ----
# More information on the metagen function in the metafor package
?metagen

# Only keeping primary data (7 data sources)
table(embdoub$data)
metadat <- embdoub[embdoub$data=="childes_children" | embdoub$data=="childes_parents" | embdoub$data=="child_books" | embdoub$data=="kids_tv_combined" | embdoub$data=="adult_speech" | embdoub$data=="gutenberg" | embdoub$data=="simply_scripts",]

metadat$data <- factor(metadat$data)
table(metadat$data)

# Add another factor for child-directed, child-produced, or adult-directed and produced
metadat$datacat <- car::recode(metadat$data, "'childes_parents' = 'childdir'; 'child_books' = 'childdir'; 
                          'kids_tv_combined' = 'childdir'; 'childes_children' = 'childpro';
                          'adult_speech' = 'adultpd'; 'gutenberg' = 'adultpd'; 'simply_scripts' = 'adultpd'")
table(metadat$datacat)

# (2a) Descriptives ----
# Mean and median effect sizes across datasets
by(metadat$eff.correct, list(metadat$att, metadat$cat), mean, na.rm = TRUE)
by(metadat$eff.correct, list(metadat$att, metadat$cat), median, na.rm = TRUE)
# Standard errors across datasets
by(metadat$se, list(metadat$att, metadat$cat), mean, na.rm = TRUE)
# Range
by(metadat$eff.correct, list(metadat$att, metadat$cat), range, na.rm = TRUE)

# Mean and median effect sizes by dataset category
by(metadat$eff.correct, list(metadat$datacat), mean, na.rm = TRUE)
by(metadat$eff.correct, list(metadat$datacat), median, na.rm = TRUE)
# Standard errors across datasets
by(metadat$se, list(metadat$datacat), mean, na.rm = TRUE)
# Range
by(metadat$eff.correct, list(metadat$datacat), range, na.rm = TRUE)


# (2b) Perform meta-analyses for each attribute and category combination ----
# Meta-analysis for Instruments-Weapons Good-Bad
# vector of treatment effects
metaeff.instweap <- metadat$eff.correct[grepl("instruments", metadat$cat)==TRUE]
metaeff.se.instweap <- metadat$se[grepl("instruments", metadat$cat)==TRUE]
meta.instweap <- metagen(TE = metaeff.instweap, seTE = metaeff.se.instweap, sm = "MD") # using mean effect method
summary(meta.instweap)
meta.instweap$seTE.fixed



# Meta-analysis for Male-Female Good-Bad
# vector of treatment effects
metaeff.mfgb <- metadat$eff.correct[grepl("male", metadat$cat)==TRUE & grepl("good", metadat$att)==TRUE]
metaeff.se.mfgb <- metadat$se[grepl("male", metadat$cat)==TRUE & grepl("good", metadat$att)==TRUE]
meta.mfgb <- metagen(TE = metaeff.mfgb, seTE = metaeff.se.mfgb, sm = "MD") # using mean effect method
summary(meta.mfgb)

# Meta-analysis for Male-Female Work-Home
# vector of treatment effects
metaeff.mfwh <- metadat$eff.correct[grepl("male", metadat$cat)==TRUE & grepl("home", metadat$att)==TRUE]
metaeff.se.mfwh <- metadat$se[grepl("male", metadat$cat)==TRUE & grepl("home", metadat$att)==TRUE]
meta.mfwh <- metagen(TE = metaeff.mfwh, seTE = metaeff.se.mfwh, sm = "MD") # using mean effect method
summary(meta.mfwh)

# Meta-analysis for Male-Female Science-Arts
# vector of treatment effects
metaeff.mfsa <- metadat$eff.correct[grepl("male", metadat$cat)==TRUE & grepl("science", metadat$att)==TRUE]
metaeff.se.mfsa <- metadat$se[grepl("male", metadat$cat)==TRUE & grepl("science", metadat$att)==TRUE]
meta.mfsa <- metagen(TE = metaeff.mfsa, seTE = metaeff.se.mfsa, sm = "MD") # using mean effect method
summary(meta.mfsa)

# Meta-analysis for Male-Female Math-Reading
# vector of treatment effects
metaeff.mfmr <- metadat$eff.correct[grepl("male", metadat$cat)==TRUE & grepl("math", metadat$att)==TRUE]
metaeff.se.mfmr <- metadat$se[grepl("male", metadat$cat)==TRUE & grepl("math", metadat$att)==TRUE]
meta.mfmr <- metagen(TE = metaeff.mfmr, seTE = metaeff.se.mfmr, sm = "MD") # using mean effect method
summary(meta.mfmr)


# (2b) Combine meta data (for fixed) ----
metadatsum <- as.data.frame(matrix(nrow = 5, ncol = 6))
colnames(metadatsum) <- c("metamean", "metase", "meta95low", "meta95high", "metap", "attcat")
metadatsum$metamean <- c(meta.mfgb$TE.fixed, meta.mfwh$TE.fixed, meta.mfsa$TE.fixed, meta.mfmr$TE.fixed, meta.instweap$TE.fixed)
metadatsum$metase <- c(meta.mfgb$seTE.fixed, meta.mfwh$seTE.fixed, meta.mfsa$seTE.fixed, meta.mfmr$seTE.fixed, meta.instweap$seTE.fixed)
metadatsum$meta95low <- c(meta.mfgb$lower.fixed, meta.mfwh$lower.fixed, meta.mfsa$lower.fixed, 
                          meta.mfmr$lower.fixed, meta.instweap$lower.fixed)
metadatsum$meta95high <- c(meta.mfgb$upper.fixed, meta.mfwh$upper.fixed, meta.mfsa$upper.fixed,
                           meta.mfmr$upper.fixed, meta.instweap$upper.fixed)
metadatsum$metap <- c(meta.mfgb$pval.fixed, meta.mfwh$pval.fixed, meta.mfsa$pval.fixed, meta.mfmr$pval.fixed, meta.instweap$pval.fixed)
metadatsum$attcat <-c("mfgb", "mfwh", "mfsa", "mfmr", "iwgb")
# Meta-analysis summary
metadatsum

# Combine meta data (for random meta-analysis)
metadatsum.ran <- as.data.frame(matrix(nrow = 5, ncol = 4))
colnames(metadatsum.ran) <- c("metamean", "metase", "metap", "attcat")
metadatsum.ran$metamean <- c(meta.mfgb$TE.random, meta.mfwh$TE.random, meta.mfsa$TE.random, meta.mfmr$TE.random, meta.instweap$TE.random)
metadatsum.ran$metase <- c(meta.mfgb$seTE.random, meta.mfwh$seTE.random, meta.mfsa$seTE.random, meta.mfmr$seTE.random, meta.instweap$seTE.random)
metadatsum.ran$metap <- c(meta.mfgb$pval.random, meta.mfwh$pval.random, meta.mfsa$pval.random, meta.mfmr$pval.random, meta.instweap$pval.random)
metadatsum.ran$attcat <-c("mfgb", "mfwh", "mfsa", "mfmr", "iwgb")
# Random effects meta-analysis summary
metadatsum.ran
# Note: Results from this summary are reported in supplementary materials (SM Table S2.1)
# Note: Results reported in SM Table S2.2 (heterogeneity) are retrieved from each individual effect size meta-analysis above (fixed effects meta-analyses)

# Means, SEs, ps for each corpora, organized by att
# Note: Results from this summary are reported in Table 1 in the main text.
metadat2 <- metadat[metadat$cat =="male vs. female",]
by(metadat2$eff.correct, list(metadat2$att, metadat2$data), mean)
by(metadat2$se, list(metadat2$att, metadat2$data), mean)
by(metadat2$p, list(metadat2$att, metadat2$data), mean)


## (3) Meta-analysis across child-produced, child-directed, and adult-produced/directed corpora subsets ----

# Meta-analysis in child-directed corpora
table(metadat$datacat)
metadat.childdir <- metadat[metadat$datacat=="childdir",]

# For Male-Female Good-Bad 
metaeff.cd.mfgb <- metadat.childdir$eff.correct[grepl("male", metadat.childdir$cat)==TRUE & grepl("good", metadat.childdir$att)==TRUE]
metaeff.cd.se.mfgb <- metadat.childdir$se[grepl("male", metadat.childdir$cat)==TRUE & grepl("good", metadat.childdir$att)==TRUE]
meta.cd.mfgb <- metagen(TE = metaeff.cd.mfgb, seTE = metaeff.cd.se.mfgb, sm = "MD") # using mean effect method
summary(meta.cd.mfgb)
meta.cd.mfgb$seTE.fixed
meta.cd.mfgb$seTE.random

# For Work-Home
metaeff.cd.mfwh <- metadat.childdir$eff.correct[grepl("male", metadat.childdir$cat)==TRUE & grepl("work", metadat.childdir$att)==TRUE]
metaeff.cd.se.mfwh <- metadat.childdir$se[grepl("male", metadat.childdir$cat)==TRUE & grepl("work", metadat.childdir$att)==TRUE]
meta.cd.mfwh <- metagen(TE = metaeff.cd.mfwh, seTE = metaeff.cd.se.mfwh, sm = "MD") # using mean effect method
summary(meta.cd.mfwh)
meta.cd.mfwh$seTE.fixed
meta.cd.mfwh$seTE.random

# For Science-Arts
metaeff.cd.mfsa <- metadat.childdir$eff.correct[grepl("male", metadat.childdir$cat)==TRUE & grepl("arts", metadat.childdir$att)==TRUE]
metaeff.cd.se.mfsa <- metadat.childdir$se[grepl("male", metadat.childdir$cat)==TRUE & grepl("arts", metadat.childdir$att)==TRUE]
meta.cd.mfsa <- metagen(TE = metaeff.cd.mfsa, seTE = metaeff.cd.se.mfsa, sm = "MD") # using mean effect method
summary(meta.cd.mfsa)
meta.cd.mfsa$seTE.fixed
meta.cd.mfsa$seTE.random

# For Reading-Math
metaeff.cd.mfrm <- metadat.childdir$eff.correct[grepl("male", metadat.childdir$cat)==TRUE & grepl("reading", metadat.childdir$att)==TRUE]
metaeff.cd.se.mfrm <- metadat.childdir$se[grepl("male", metadat.childdir$cat)==TRUE & grepl("reading", metadat.childdir$att)==TRUE]
meta.cd.mfrm <- metagen(TE = metaeff.cd.mfrm, seTE = metaeff.cd.se.mfrm, sm = "MD") # using mean effect method
summary(meta.cd.mfrm)
meta.cd.mfrm$seTE.fixed
meta.cd.mfrm$seTE.random


# Meta-analysis in adult-directed corpora
table(metadat$datacat)
metadat.adultdp <- metadat[metadat$datacat=="adultpd",]

# For Male-Female Good-Bad 
metaeff.adp.mfgb <- metadat.adultdp$eff.correct[grepl("male", metadat.adultdp$cat)==TRUE & grepl("good", metadat.adultdp$att)==TRUE]
metaeff.adp.se.mfgb <- metadat.adultdp$se[grepl("male", metadat.adultdp$cat)==TRUE & grepl("good", metadat.adultdp$att)==TRUE]
meta.adp.mfgb <- metagen(TE = metaeff.adp.mfgb, seTE = metaeff.adp.se.mfgb, sm = "MD") # using mean effect method
summary(meta.adp.mfgb)
meta.adp.mfgb$seTE.fixed
meta.adp.mfgb$seTE.random

# For Work-Home
metaeff.adp.mfwh <- metadat.adultdp$eff.correct[grepl("male", metadat.adultdp$cat)==TRUE & grepl("work", metadat.adultdp$att)==TRUE]
metaeff.adp.se.mfwh <- metadat.adultdp$se[grepl("male", metadat.adultdp$cat)==TRUE & grepl("work", metadat.adultdp$att)==TRUE]
meta.adp.mfwh <- metagen(TE = metaeff.adp.mfwh, seTE = metaeff.adp.se.mfwh, sm = "MD") # using mean effect method
summary(meta.adp.mfwh)
meta.adp.mfwh$seTE.fixed
meta.adp.mfwh$seTE.random

# For Science-Arts
metaeff.adp.mfsa <- metadat.adultdp$eff.correct[grepl("male", metadat.adultdp$cat)==TRUE & grepl("arts", metadat.adultdp$att)==TRUE]
metaeff.adp.se.mfsa <- metadat.adultdp$se[grepl("male", metadat.adultdp$cat)==TRUE & grepl("arts", metadat.adultdp$att)==TRUE]
meta.adp.mfsa <- metagen(TE = metaeff.adp.mfsa, seTE = metaeff.adp.se.mfsa, sm = "MD") # using mean effect method
summary(meta.adp.mfsa)
meta.adp.mfsa$seTE.fixed
meta.adp.mfsa$seTE.random

# For Reading-Math
metaeff.adp.mfrm <- metadat.adultdp$eff.correct[grepl("male", metadat.adultdp$cat)==TRUE & grepl("reading", metadat.adultdp$att)==TRUE]
metaeff.adp.se.mfrm <- metadat.adultdp$se[grepl("male", metadat.adultdp$cat)==TRUE & grepl("reading", metadat.adultdp$att)==TRUE]
meta.adp.mfrm <- metagen(TE = metaeff.adp.mfrm, seTE = metaeff.adp.se.mfrm, sm = "MD") # using mean effect method
summary(meta.adp.mfrm)
meta.adp.mfrm$seTE.fixed
meta.adp.mfrm$seTE.random


## (3b) Meta-analysis across all stereotypes and across child-produced, child-directed, and adult-produced/directed corpora subsets ----
# Meta-analysis in all stereotypes and all corpora
metaeff.all <- metadat$eff.correct[metadat$cat != "weapons vs. instruments"]
metaeff.se.all <- metadat$se[metadat$cat != "weapons vs. instruments"]
meta.all <- metagen(TE = metaeff.all, seTE = metaeff.se.all, sm = "MD") # using mean effect method
summary(meta.all)
meta.all$seTE.fixed

# Across child-produced
metaeff.chprod <- metadat$eff.correct[metadat$data=="childes_children" & metadat$cat != "weapons vs. instruments"]
metaeff.se.chprod <- metadat$se[metadat$data=="childes_children" & metadat$cat != "weapons vs. instruments"]
meta.chprod <- metagen(TE = metaeff.chprod, seTE = metaeff.se.chprod, sm = "MD") # using mean effect method
summary(meta.chprod)
meta.chprod$seTE.fixed
# Across child-directed
metadat.childdir <- metadat[metadat$datacat=="childdir",]
metaeff.chdir <- metadat.childdir$eff.correct[metadat.childdir$cat != "weapons vs. instruments"]
metaeff.se.chdir <-metadat.childdir$se[metadat.childdir$cat != "weapons vs. instruments"]
meta.chdir <- metagen(TE = metaeff.chdir, seTE = metaeff.se.chdir, sm = "MD") # using mean effect method
summary(meta.chdir)
meta.chdir$seTE.fixed
# Across adult-directed
metadat.adultpd <- metadat[metadat$datacat=="adultpd",]
metaeff.adultpd <- metadat.adultpd$eff.correct[metadat.adultpd$cat != "weapons vs. instruments"]
metaeff.se.adultpd <-metadat.adultpd$se[metadat.adultpd$cat != "weapons vs. instruments"]
meta.adultpd <- metagen(TE = metaeff.adultpd, seTE = metaeff.se.adultpd, sm = "MD") # using mean effect method
summary(meta.adultpd)
meta.adultpd$seTE.fixed


# Child-directed speech
metaeff.chdirspeech <- metadat$eff.correct[metadat$data=="childes_parents" & metadat$cat != "weapons vs. instruments"]
metaeff.se.chdirspeech <- metadat$se[metadat$data=="childes_parents" & metadat$cat != "weapons vs. instruments"]
meta.chdirspeech <- metagen(TE = metaeff.chdirspeech, seTE = metaeff.se.chdirspeech, sm = "MD") # using mean effect method
summary(meta.chdirspeech)
meta.chdirspeech$seTE.fixed
# Child-directed books
metaeff.chbooks <- metadat$eff.correct[metadat$data=="child_books" & metadat$cat != "weapons vs. instruments"]
metaeff.se.chbooks <- metadat$se[metadat$data=="child_books" & metadat$cat != "weapons vs. instruments"]
meta.chbooks <- metagen(TE = metaeff.chbooks, seTE = metaeff.se.chbooks, sm = "MD") # using mean effect method
summary(meta.chbooks)
meta.chbooks$seTE.fixed
# Child-directed TV
metaeff.chtv <- metadat$eff.correct[metadat$data=="kids_tv_combined" & metadat$cat != "weapons vs. instruments"]
metaeff.se.chtv <- metadat$se[metadat$data=="kids_tv_combined" & metadat$cat != "weapons vs. instruments"]
meta.chtv <- metagen(TE = metaeff.chtv, seTE = metaeff.se.chtv, sm = "MD") # using mean effect method
summary(meta.chtv)
meta.chtv$seTE.fixed
# Adult speech
metaeff.adspeech <- metadat$eff.correct[metadat$data=="adult_speech" & metadat$cat != "weapons vs. instruments"]
metaeff.se.adspeech <- metadat$se[metadat$data=="adult_speech" & metadat$cat != "weapons vs. instruments"]
meta.adspeech <- metagen(TE = metaeff.adspeech, seTE = metaeff.se.adspeech, sm = "MD") # using mean effect method
summary(meta.adspeech)
meta.adspeech$seTE.fixed
# Adult books
metaeff.adbooks <- metadat$eff.correct[metadat$data=="gutenberg" & metadat$cat != "weapons vs. instruments"]
metaeff.se.adbooks <- metadat$se[metadat$data=="gutenberg" & metadat$cat != "weapons vs. instruments"]
meta.adbooks <- metagen(TE = metaeff.adbooks, seTE = metaeff.se.adbooks, sm = "MD") # using mean effect method
summary(meta.adbooks)
meta.adbooks$seTE.fixed
# Adult TV
metaeff.adtv <- metadat$eff.correct[metadat$data=="simply_scripts" & metadat$cat != "weapons vs. instruments"]
metaeff.se.adtv <- metadat$se[metadat$data=="simply_scripts" & metadat$cat != "weapons vs. instruments"]
meta.adtv <- metagen(TE = metaeff.adtv, seTE = metaeff.se.adtv, sm = "MD") # using mean effect method
summary(meta.adtv)
meta.adtv$seTE.fixed

# Note: Results for each corpus/stereotype combination meta-analysis are reported in Table 1 in the main text.
metadat[metadat$data=="childes_children",c("att", "eff.correct", "se", "p")]
metadat[metadat$data=="childes_parents",c("att", "eff.correct", "se", "p")]
metadat[metadat$data=="child_books",c("att", "eff.correct", "se", "p")]
metadat[metadat$data=="kids_tv_combined",c("att", "eff.correct", "se", "p")]
metadat[metadat$data=="adult_speech",c("att", "eff.correct", "se", "p")]
metadat[metadat$data=="gutenberg",c("att", "eff.correct", "se", "p")]
metadat[metadat$data=="simply_scripts",c("att", "eff.correct", "se", "p")]

## (3c) Meta-regression across results ----
metadat2$chad <- car::recode(metadat2$datacat, "'childdir' = 'child'; 'childpro' = 'child'; 'adultpd' = 'adult'")
metadat2$chad <- relevel(metadat2$chad, "child")

metadat2$time <- car::recode(metadat2$data, 
                            "'adult_speech' = 'mid'; 'childes_children' = 'mid'; 'childes_parents' = 'mid';
                            'child_books' = 'early'; 'gutenberg' = 'early';
                            'simply_scripts' = 'late'; 'kids_tv_combined' = 'late'")

# (3c.1) Comparisons across four attitude/stereotype topics ----
table(metadat2$att) # only including gender stereotypes
meta.study1 <- metagen(TE = eff.correct, seTE = se, sm = "MD", data = metadat2) # using mean effect method
metareg.study1.att <- metareg(meta.study1, att)
summary(metareg.study1.att) # no difference; intercept is significantly higher than zero

# (3c.2) Comparisons across two age subgroups (child and adult) ----
table(metadat2$chad)
meta.study1.chad <- metagen(TE = eff.correct, 
                            seTE = se, sm = "MD", data = metadat2) # using mean effect method
metareg.study1.chad <- metareg(meta.study1.chad, chad)
summary(metareg.study1.chad) # no significant differences
bubble.metareg(metareg.study1.chad, ylim = c(-0.75, 2))

# (3c.3) Comparisons across three time subgroups (early, mid, late) ----
table(metadat2$time)
meta.study1.time <- metagen(TE = eff.correct, seTE = se, sm = "MD", data = metadat2) # using mean effect method
metareg.study1.time <- metareg(meta.study1.time, paste(time))
summary(metareg.study1.time) # no significant differences

# (3d) Quantifying heterogeneity in meta-analyses ----
metadat.hetero <- as.data.frame(matrix(nrow = 15, ncol = 8))
colnames(metadat.hetero) <- c("corp", "attcat", "I2", "I2low", "I2high", "Q", "df", "Qp")
metadat.hetero$corp <- c("ov", "chdir", "addir", "ov", "ov", "ov", "ov",
                         "chdir","chdir","chdir","chdir",
                         "addir","addir","addir","addir")
metadat.hetero$attcat <- c("ov", "ov", "ov", "goodbad", "workhome", "sciarts", "mathread",
                           "goodbad", "workhome", "sciarts", "mathread",
                           "goodbad", "workhome", "sciarts", "mathread")
metadat.hetero$I2 <- c(meta.all$I2, meta.chdir$I2, meta.adultpd$I2,
                    meta.mfgb$I2, meta.mfwh$I2, meta.mfsa$I2, meta.mfmr$I2,
                    meta.cd.mfgb$I2, meta.cd.mfwh$I2, meta.cd.mfsa$I2, meta.cd.mfrm$I2,
                    meta.adp.mfgb$I2, meta.adp.mfwh$I2, meta.adp.mfsa$I2, meta.adp.mfrm$I2)
metadat.hetero$I2low <- c(meta.all$lower.I2, meta.chdir$lower.I2, meta.adultpd$lower.I2,
                       meta.mfgb$lower.I2, meta.mfwh$lower.I2, meta.mfsa$lower.I2, meta.mfmr$lower.I2,
                       meta.cd.mfgb$lower.I2, meta.cd.mfwh$lower.I2, meta.cd.mfsa$lower.I2, meta.cd.mfrm$lower.I2,
                       meta.adp.mfgb$lower.I2, meta.adp.mfwh$lower.I2, meta.adp.mfsa$lower.I2, meta.adp.mfrm$lower.I2)
metadat.hetero$I2high <- c(meta.all$upper.I2, meta.chdir$upper.I2, meta.adultpd$upper.I2,
                          meta.mfgb$upper.I2, meta.mfwh$upper.I2, meta.mfsa$upper.I2, meta.mfmr$upper.I2,
                          meta.cd.mfgb$upper.I2, meta.cd.mfwh$upper.I2, meta.cd.mfsa$upper.I2, meta.cd.mfrm$upper.I2,
                          meta.adp.mfgb$upper.I2, meta.adp.mfwh$upper.I2, meta.adp.mfsa$upper.I2, meta.adp.mfrm$upper.I2)
metadat.hetero$Q <- c(meta.all$Q, meta.chdir$Q, meta.adultpd$Q,
                          meta.mfgb$Q, meta.mfwh$Q, meta.mfsa$Q, meta.mfmr$Q,
                      meta.cd.mfgb$Q, meta.cd.mfwh$Q, meta.cd.mfsa$Q, meta.cd.mfrm$Q,
                      meta.adp.mfgb$Q, meta.adp.mfwh$Q, meta.adp.mfsa$Q, meta.adp.mfrm$Q)
metadat.hetero$df <- c(meta.all$df.Q, meta.chdir$df.Q, meta.adultpd$df.Q,
                          meta.mfgb$df.Q, meta.mfwh$df.Q, meta.mfsa$df.Q, meta.mfmr$df.Q,
                       meta.cd.mfgb$df.Q, meta.cd.mfwh$df.Q, meta.cd.mfsa$df.Q, meta.cd.mfrm$df.Q,
                       meta.adp.mfgb$df.Q, meta.adp.mfwh$df.Q, meta.adp.mfsa$df.Q, meta.adp.mfrm$df.Q)
metadat.hetero$Qp <- c(meta.all$pval.Q, meta.chdir$pval.Q, meta.adultpd$pval.Q,
                          meta.mfgb$pval.Q, meta.mfwh$pval.Q, meta.mfsa$pval.Q, meta.mfmr$pval.Q,
                       meta.cd.mfgb$pval.Q, meta.cd.mfwh$pval.Q, meta.cd.mfsa$pval.Q, meta.cd.mfrm$pval.Q,
                       meta.adp.mfgb$pval.Q, meta.adp.mfwh$pval.Q, meta.adp.mfsa$pval.Q, meta.adp.mfrm$pval.Q)
metadat.hetero



## (4) STUDY TWO: Gender-trait association results ----
## Trait SC-WEAT with *groups* of trait synonyms
# Read in all SC-WEAT
traitdat.groups <- read.csv("embeddings_singletraits_groups_071220.csv") # Also in .RData summary file
names(traitdat.groups) <- c("data", "categories", "attributes", 
                            "effsize", "miss", "pleft", "pright", "ptot", "se", "cohesion")

# (4a) Data preparation ----
length(table(traitdat.groups$attributes)) # currently = 170 (chosen from the list of traits w/ > 5 occurrences)

# Make new data frame with only 7 primary corpora
traitdat.groupssub <- traitdat.groups[traitdat.groups$data=="childes_children" | traitdat.groups$data=="childes_parents" | traitdat.groups$data=="child_books" | traitdat.groups$data=="kids_tv_combined" | traitdat.groups$data=="adult_speech" | traitdat.groups$data=="gutenberg" | traitdat.groups$data=="simply_scripts",]

traitdat.groupssub$attributes <- factor(traitdat.groupssub$attributes)
traitdat.groupssub$data <- factor(traitdat.groupssub$data)

length(table(traitdat.groupssub$attributes))

# drop factors with < 5 obs 
traitdat.groupsnew <- traitdat.groupssub[!(as.numeric(traitdat.groupssub$attributes) %in% which(table(traitdat.groupssub$attributes) < 5)),]
traitdat.groupsnew$attributes <- factor(traitdat.groupsnew$attributes)
length(table(traitdat.groupsnew$attributes)) # now = 170 traits (kept all traits as before)
table(traitdat.groupsnew$data) # variability across corpora in the number of traits that are actually present

traitdat.groupsnew <- traitdat.groupsnew[c("data", "categories", "attributes", "effsize", "se", "ptot")]
colnames(traitdat.groupsnew) <- c("data", "cat", "att", "eff", "se", "p")


## (4b) Descriptives ----

# Mean effect sizes across datasets and across attributes
mean(traitdat.groupsnew$eff, na.rm = TRUE) 
range(traitdat.groupsnew$eff, na.rm = TRUE) 
# Mean effect sizes across datasets
by(traitdat.groupsnew$eff, list(traitdat.groupsnew$data), mean, na.rm = TRUE)
# Mean effect sizes across attributes
by(traitdat.groupsnew$eff, list(traitdat.groupsnew$att), mean, na.rm = TRUE)


## Meta analysis of effect sizes for each attribute (170 traits)
table(traitdat.groupsnew$att)
traitdat.groupsnew$attnum <- as.numeric(traitdat.groupsnew$att)
table(traitdat.groupsnew$att)
table(traitdat.groupsnew$attnum)

meta.groups.alltrait <- list()
meta.groups.traitmeans <- vector()
meta.groups.traitse <- vector()
meta.groups.traitp <- vector()

for (i in 1:length(table(traitdat.groupsnew$att))){
  meta.groups.alltrait[[i]] <- metagen(TE = traitdat.groupsnew$eff[traitdat.groupsnew$attnum==i], seTE = traitdat.groupsnew$se[traitdat.groupsnew$attnum==i], sm = "MD")
  meta.groups.traitmeans[i] <- meta.groups.alltrait[[i]]$TE.fixed
  meta.groups.traitse[i] <- meta.groups.alltrait[[i]]$seTE.fixed
  meta.groups.traitp[i] <- meta.groups.alltrait[[i]]$pval.fixed
}

names(meta.groups.alltrait) <- levels(traitdat.groupsnew$att)
meta.groups.traitsum <- as.data.frame(meta.groups.traitmeans)
colnames(meta.groups.traitsum) <- "mean"
meta.groups.traitsum$se <- meta.groups.traitse
meta.groups.traitsum$p <- meta.groups.traitp
meta.groups.traitsum$att <- levels(traitdat.groupsnew$att)

# now add on vectors for each sub database
table(traitdat.groupsnew$data)
meta.groups.traitsum2 <- merge(meta.groups.traitsum, 
                               traitdat.groupsnew[traitdat.groupsnew$data=="childes_children", 
                                                  c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.groups.traitsum2)[5:6] <- c("effchchild", "sechchild")
meta.groups.traitsum2 <- merge(meta.groups.traitsum2, 
                               traitdat.groupsnew[traitdat.groupsnew$data=="childes_parents", 
                                                  c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.groups.traitsum2)[7:8] <- c("effchpar", "sechpar")
meta.groups.traitsum2 <- merge(meta.groups.traitsum2, 
                               traitdat.groupsnew[traitdat.groupsnew$data=="child_books", 
                                                  c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.groups.traitsum2)[9:10] <- c("effchbook", "sechbook")
meta.groups.traitsum2 <- merge(meta.groups.traitsum2, 
                               traitdat.groupsnew[traitdat.groupsnew$data=="gutenberg", 
                                                  c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.groups.traitsum2)[11:12] <- c("effguten", "seguten")
meta.groups.traitsum2 <- merge(meta.groups.traitsum2, 
                               traitdat.groupsnew[traitdat.groupsnew$data=="kids_tv_combined", 
                                                  c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.groups.traitsum2)[13:14] <- c("effchtv", "sechtv")
meta.groups.traitsum2 <- merge(meta.groups.traitsum2, 
                               traitdat.groupsnew[traitdat.groupsnew$data=="simply_scripts", 
                                                  c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.groups.traitsum2)[15:16] <- c("effadtv", "seadtv")
meta.groups.traitsum2 <- merge(meta.groups.traitsum2, 
                               traitdat.groupsnew[traitdat.groupsnew$data=="adult_speech", 
                                                  c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.groups.traitsum2)[17:18] <- c("effadspch", "seadspch")


## (4c) Descriptives of meta-analytic data ----
# Overall data
mean(meta.groups.traitsum2$mean)
t.test(meta.groups.traitsum2$mean, mu = 0)
range(meta.groups.traitsum2$mean) 
head(meta.groups.traitsum2$att[order(meta.groups.traitsum2$mean, decreasing = TRUE)], n = 6)
head(meta.groups.traitsum2$att[order(meta.groups.traitsum2$mean, decreasing = FALSE)], n = 6)
# Note: Result is reported in main text, Table 2

## Range of top effect sizes
range(traitdat.groupsnew$eff[traitdat.groupsnew$att == head(meta.groups.traitsum2$att[order(meta.groups.traitsum2$mean, decreasing = TRUE)][1])]) # For the top effect size for men
range(traitdat.groupsnew$eff[traitdat.groupsnew$att == head(meta.groups.traitsum2$att[order(meta.groups.traitsum2$mean, decreasing = FALSE)][1])]) # For the top effect size for women


## How many are significant?
length(which(meta.groups.traitsum2$p < .05))
meta.groups.traitsum2$sig <- ifelse(meta.groups.traitsum2$p < .05, "sig", "nsig")
table(meta.groups.traitsum2$sig)/dim(meta.groups.traitsum2)[1]
meta.groups.traitsum2$sig.mf <- ifelse(meta.groups.traitsum2$p < .05 & meta.groups.traitsum2$mean < 0, "sig.f", 
                                       ifelse(meta.groups.traitsum2$p < .05 & meta.groups.traitsum2$mean > 0, "sig.m", "nsig"))
table(meta.groups.traitsum2$sig.mf)/dim(meta.groups.traitsum2)[1]


## How many fall beyond [-0.1, 0.1]?
meta.groups.traitsum2$effcut <- cut(meta.groups.traitsum2$mean, breaks = c(-2, -0.1, 0.1, 2), labels = c("sig.f", "nsig", "sig.m"))
table(meta.groups.traitsum2$effcut) 
table(meta.groups.traitsum2$effcut)/dim(meta.groups.traitsum2)[1]
binom.test(sum(table(meta.groups.traitsum2$effcut)[c(1, 3)]),
           dim(meta.groups.traitsum2)[1]) 

## How many fall beyond [-0.2, 0.2]?
meta.groups.traitsum2$effcut2 <- cut(meta.groups.traitsum2$mean, breaks = c(-2, -0.2, 0.2, 2), labels = c("sig.f", "nsig", "sig.m"))
table(meta.groups.traitsum2$effcut2) 
table(meta.groups.traitsum2$effcut2)/dim(meta.groups.traitsum2)[1]
binom.test(sum(table(meta.groups.traitsum2$effcut2)[c(1, 3)]),
           dim(meta.groups.traitsum2)[1])

## How many fall beyond [-0.3, 0.3]?
meta.groups.traitsum2$effcut3 <- cut(meta.groups.traitsum2$mean, breaks = c(-2, -0.3, 0.3, 2), labels = c("sig.f", "nsig", "sig.m"))
table(meta.groups.traitsum2$effcut3) 
table(meta.groups.traitsum2$effcut3)/dim(meta.groups.traitsum2)[1]
binom.test(sum(table(meta.groups.traitsum2$effcut3)[c(1, 3)]),
           dim(meta.groups.traitsum2)[1]) # 29% are meaningful

## Are traits more female or more male?
t.test(meta.groups.traitsum2$mean, mu = 0)
meta.groups.traitsum2$malfem <- ifelse(meta.groups.traitsum2$mean > 0, "mal", "fem")
table(meta.groups.traitsum2$malfem)/dim(meta.groups.traitsum2)[1]
binom.test(table(meta.groups.traitsum2$malfem)[1],
           dim(meta.groups.traitsum2)[1]) 

# Note: All summary results below are reported in main text, Table 2
# Within child-produced speech
head(meta.groups.traitsum2$att[order(meta.groups.traitsum2$effchchild, decreasing = TRUE)])
head(meta.groups.traitsum2$att[order(meta.groups.traitsum2$effchchild, decreasing = FALSE)])
# Within child-directed speech
head(meta.groups.traitsum2$att[order(meta.groups.traitsum2$effchpar, decreasing = TRUE)])
head(meta.groups.traitsum2$att[order(meta.groups.traitsum2$effchpar, decreasing = FALSE)])
# Within child-directed books
head(meta.groups.traitsum2$att[order(meta.groups.traitsum2$effchbook, decreasing = TRUE)])
head(meta.groups.traitsum2$att[order(meta.groups.traitsum2$effchbook, decreasing = FALSE)])
# Within child-directed TV
head(meta.groups.traitsum2$att[order(meta.groups.traitsum2$effchtv, decreasing = TRUE)])
head(meta.groups.traitsum2$att[order(meta.groups.traitsum2$effchtv, decreasing = FALSE)])
# Within adult-directed speech
head(meta.groups.traitsum2$att[order(meta.groups.traitsum2$effadspch, decreasing = TRUE)])
head(meta.groups.traitsum2$att[order(meta.groups.traitsum2$effadspch, decreasing = FALSE)])
# Within adult-directed books
head(meta.groups.traitsum2$att[order(meta.groups.traitsum2$effguten, decreasing = TRUE)])
head(meta.groups.traitsum2$att[order(meta.groups.traitsum2$effguten, decreasing = FALSE)])
# Within adult-directed tv
head(meta.groups.traitsum2$att[order(meta.groups.traitsum2$effadtv, decreasing = TRUE)])
head(meta.groups.traitsum2$att[order(meta.groups.traitsum2$effadtv, decreasing = FALSE)])

## (4d) Meta-regression across results ----
# combine all effects, collapsing across the content of the trait
metareg.groups.traitsum <- as.data.frame(c(meta.groups.traitsum2$effchchild, 
                                           meta.groups.traitsum2$effchpar, 
                               meta.groups.traitsum2$effchbook, meta.groups.traitsum2$effguten,
                               meta.groups.traitsum2$effchtv, meta.groups.traitsum2$effadtv,
                               meta.groups.traitsum2$effadspch))
metareg.groups.traitsum$se <- c(meta.groups.traitsum2$sechchild, meta.groups.traitsum2$sechpar, 
                                    meta.groups.traitsum2$sechbook, meta.groups.traitsum2$seguten,
                                    meta.groups.traitsum2$sechtv, meta.groups.traitsum2$seadtv,
                                    meta.groups.traitsum2$seadspch)
metareg.groups.traitsum$corpus <- rep(c("effchchild", "effchpar", "effchbook", 
                            "effguten", "effchtv", "effadtv", "effadspch"), each = 170)
metareg.groups.traitsum$corpus <- factor(metareg.groups.traitsum$corpus, 
                                  levels = c("effchchild", "effchpar", "effchbook", "effchtv",
                                                            "effadspch", "effguten", "effadtv"))
metareg.groups.traitsum$chad <- car::recode(metareg.groups.traitsum$corpus, 
"'effchpar' = 'child'; 'effchbook' = 'child'; 'effchtv' = 'child'; 'effchchild' = 'child';
                          'effadspch' = 'adult'; 'effguten' = 'adult'; 'effadtv' = 'adult'")
metareg.groups.traitsum$chad <- factor(metareg.groups.traitsum$chad, levels = c("child", "adult"))
table(metareg.groups.traitsum$chad)

metareg.groups.traitsum$time <- car::recode(metareg.groups.traitsum$corpus, "'effchpar' = 'mid'; 'effchbook' = 'early'; 
                          'effchtv' = 'late'; 'effchchild' = 'mid';
                          'effadspch' = 'mid'; 'effguten' = 'early'; 'effadtv' = 'late'")
metareg.groups.traitsum$time <- factor(metareg.groups.traitsum$time, levels = c("early", "mid", "late"))
table(metareg.groups.traitsum$time)

colnames(metareg.groups.traitsum) <- c("eff.correct", "se", "corpus", "chad", "time")

meta.groups.study2 <- metagen(TE = eff.correct, seTE = se, sm = "MD", data = metareg.groups.traitsum)
library(metafor)
# (4d.1) Comparisons across two age subgroups (child and adult) ----
table(metareg.groups.traitsum$chad)
metareg.groups.study2.chad <- metareg(meta.groups.study2, chad)
summary(metareg.groups.study2.chad) # significant difference: adult is lower than child
bubble.metareg(metareg.groups.study2.chad, ylim = c(-2, 2))

# (4d.2) Comparisons across three time subgroups (early, mid, late) ----
table(metareg.groups.traitsum$time)
metareg.groups.study2.time <- metareg(meta.groups.study2, paste(time))
summary(metareg.groups.study2.time) # significant differences



# (4e) Plotting single trait stimuli results ----
# note: see sm_bwplotting for two-page plot provided in the main text.
# where are the cut-offs?
# "mischievous" (effect size ~ -0.1) and "creative" (effect size ~ 0.1)
# relaxed (-0.2) and verbal (0.2)
# jealous (-0.3) and stable (0.3)
meta.groups.traitsum[order(meta.groups.traitsum$mean, decreasing = TRUE),]

pdf(file = "metatrait_plot_groups_efver.pdf", width = 35, height = 20)
op <- par(mar = c(12, 8, 2, 2))
plot(meta.groups.traitsum$mean[order(meta.groups.traitsum$mean, decreasing = TRUE)], ylim = c(-1.2, 0.8),
     xlab = "", ylab = "", axes = FALSE,
     pch = 20, cex = 2, col = "gray")
box()
axis(1, at = 1:170, 
     labels = meta.groups.traitsum$att[order(meta.groups.traitsum$mean, decreasing = TRUE)], las = 2, cex.axis = 1.7)
axis(2, at = seq(-1.4, 1.4, by = 0.2), labels = round(seq(-1.4, 1.4, by = 0.2), 2), las = 1, cex.axis = 1.7)
mtext("Meta-analytic Effect (Trait = Male)", line = 4, side = 2, cex = 3)
abline(h = 0, lty = 2, col = "gray", lwd = 3)
segments(x0 = c(1:170), x1 = c(1:170), 
         y0 = meta.groups.traitsum$mean[order(meta.groups.traitsum$mean, decreasing = TRUE)] - 1.96*meta.groups.traitsum$se[order(meta.groups.traitsum$mean, decreasing = TRUE)],
         y1 = meta.groups.traitsum$mean[order(meta.groups.traitsum$mean, decreasing = TRUE)] + 1.96*meta.groups.traitsum$se[order(meta.groups.traitsum$mean, decreasing = TRUE)],
         lty = 1, lwd = 2, col = "gray")
abline(v = which(meta.groups.traitsum$att[order(meta.groups.traitsum$mean, decreasing = TRUE)] == "mischievous"), 
       col = "red", lty = 2, lwd = 3)
abline(v = which(meta.groups.traitsum$att[order(meta.groups.traitsum$mean, decreasing = TRUE)] == "creative"),
       col = "red", lty = 2, lwd = 3)
abline(v = which(meta.groups.traitsum$att[order(meta.groups.traitsum$mean, decreasing = TRUE)] == "relaxed"), 
       col = "orange", lty = 2, lwd = 3)
abline(v = which(meta.groups.traitsum$att[order(meta.groups.traitsum$mean, decreasing = TRUE)] == "verbal"),
       col = "orange", lty = 2, lwd = 3)
abline(v = which(meta.groups.traitsum$att[order(meta.groups.traitsum$mean, decreasing = TRUE)] == "jealous"), 
       col = "seagreen", lty = 2, lwd = 3)
abline(v = which(meta.groups.traitsum$att[order(meta.groups.traitsum$mean, decreasing = TRUE)] == "stable"),
       col = "seagreen", lty = 2, lwd = 3)
legend("bottomright", c("[-0.3, 0.3]", "[-0.2, 0.2]", "[-0.1, 0.1]"), col = c("seagreen", "orange", "red"), lty = 2, lwd = 4, bty = "n", cex = 4)
par(op)
dev.off()




# (5) STUDY THREE: Gender-occupation association results ----
# read in all occupation SC-WEAT results
profdat <- read.csv("embeddings_singleprofs_.csv") # Also provided in .RData summary object
names(profdat) <- c("data", "categories", "attributes",
                     "effsize", "miss", "pleft", "pright", "ptot", "se", "cohesion")

# (5a) Data preparation ----
length(table(profdat$attributes)) # currently = 84 profs in at least one corpus

# Make new data frame with only 7 primary corpora
profdatsub <- profdat[profdat$data=="childes_children" | profdat$data=="childes_parents" | profdat$data=="child_books" | profdat$data=="kids_tv_combined" | profdat$data=="adult_speech" | profdat$data=="gutenberg" | profdat$data=="simply_scripts",]

profdatsub$attributes <- factor(profdatsub$attributes)
profdatsub$data <- factor(profdatsub$data)

length(table(profdatsub$attributes)) # now = 82 profs in at least one corpus

# drop factors without > 5 obs
profdatnew <- profdatsub[!(as.numeric(profdatsub$attributes) %in% which(table(profdatsub$attributes) < 5)),]
profdatnew$attributes <- factor(profdatnew$attributes)
length(table(profdatnew$attributes)) # now = 39 profs
table(profdatnew$data) # variability across corpora in the number of profs that are actually present
# drop factors without > 4 obs
profdatnew2 <- profdatsub[!(as.numeric(profdatsub$attributes) %in% which(table(profdatsub$attributes) < 4)),]
profdatnew2$attributes <- factor(profdatnew2$attributes)
length(table(profdatnew2$attributes)) # now = 50 profs
# drop factors without > 3 obs
profdatnew3 <- profdatsub[!(as.numeric(profdatsub$attributes) %in% which(table(profdatsub$attributes) < 3)),]
profdatnew3$attributes <- factor(profdatnew3$attributes)
length(table(profdatnew3$attributes)) # now = 60 profs
# keep factors with only 7 occurrences
profdatnew4 <- profdatsub[!(as.numeric(profdatsub$attributes) %in% which(table(profdatsub$attributes) < 7)),]
profdatnew4$attributes <- factor(profdatnew4$attributes)
length(table(profdatnew4$attributes)) # now = 17 profs
table(profdatnew4$data)
# because we want to correlate to bureau of labor statistics, keep max number of professions
profdatnew <- profdatsub[c("data", "categories", "attributes", "effsize", "se", "ptot")]
colnames(profdatnew) <- c("data", "cat", "att", "eff", "se", "p")


# Also make new data frame with only adult corpora
profdatadultnew <- profdatsub[profdatsub$data=="adult_speech" | profdatsub$data=="gutenberg" | profdatsub$data=="simply_scripts",]
profdatadultnew$attributes <- factor(profdatadultnew$attributes)
length(table(profdatadultnew$attributes)) 

profdatadultnew <- profdatadultnew[c("data", "categories", "attributes", "effsize", "se", "ptot")]
colnames(profdatadultnew) <- c("data", "cat", "att", "eff", "se", "p")



# (5b) Descriptives ----
# Mean effect sizes across datasets and across attributes
mean(profdatnew$eff, na.rm = TRUE) 
range(profdatnew$eff, na.rm = TRUE) 
by(profdatnew$eff, list(profdatnew$data), mean, na.rm = TRUE)
by(profdatnew$eff, list(profdatnew$att), mean, na.rm = TRUE)
length(by(profdatnew$eff, list(profdatnew$att), mean, na.rm = TRUE))


# (5c) Meta analysis of effect sizes for each attribute (82 profs) ----
table(profdatnew$att)

profdatnew$attnum <- as.numeric(profdatnew$att)
table(profdatnew$att)
table(profdatnew$attnum)

meta.allprof <- list()
meta.profmeans <- vector()
meta.profse <- vector()
meta.profp <- vector()

for (i in 1:length(table(profdatnew$att))){
  meta.allprof[[i]] <- metagen(TE = profdatnew$eff[profdatnew$attnum==i], seTE = profdatnew$se[profdatnew$attnum==i], sm = "MD")
  meta.profmeans[i] <- meta.allprof[[i]]$TE.fixed
  meta.profse[i] <- meta.allprof[[i]]$seTE.fixed
  meta.profp[i] <- meta.allprof[[i]]$pval.fixed
}


names(meta.allprof) <- levels(profdatnew$att)
meta.profsum <- as.data.frame(meta.profmeans)
colnames(meta.profsum) <- "mean"
meta.profsum$se <- meta.profse
meta.profsum$p <- meta.profp
meta.profsum$att <- levels(profdatnew$att)

# now add on vectors for each sub database
table(profdatnew$data)
meta.profsum2 <- merge(meta.profsum, 
                        profdatnew[profdatnew$data=="childes_children", 
                                    c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.profsum2)[5:6] <- c("effchchild", "sechchild")
meta.profsum2 <- merge(meta.profsum2, 
                        profdatnew[profdatnew$data=="childes_parents", 
                                    c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.profsum2)[7:8] <- c("effchpar", "sechpar")
meta.profsum2 <- merge(meta.profsum2, 
                        profdatnew[profdatnew$data=="child_books", 
                                    c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.profsum2)[9:10] <- c("effchbook", "sechbook")
meta.profsum2 <- merge(meta.profsum2, 
                        profdatnew[profdatnew$data=="gutenberg", 
                                    c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.profsum2)[11:12] <- c("effguten", "seguten")
meta.profsum2 <- merge(meta.profsum2, 
                        profdatnew[profdatnew$data=="kids_tv_combined", 
                                    c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.profsum2)[13:14] <- c("effchtv", "sechtv")
meta.profsum2 <- merge(meta.profsum2, 
                        profdatnew[profdatnew$data=="simply_scripts", 
                                    c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.profsum2)[15:16] <- c("effadtv", "seadtv")
meta.profsum2 <- merge(meta.profsum2, 
                        profdatnew[profdatnew$data=="adult_speech", 
                                    c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.profsum2)[17:18] <- c("effadspch", "seadspch")


## Descriptives of meta data ----
# Overall data
range(meta.profsum2$mean) # smaller range from meta-analyses
head(meta.profsum2$att[order(meta.profsum2$mean, decreasing = TRUE)])
head(meta.profsum2$att[order(meta.profsum2$mean, decreasing = FALSE)])
# Note: Summary results are reported in main text (Table 3)

## How many are significant?
length(which(meta.profsum2$p < .05))
meta.profsum2$sig <- ifelse(meta.profsum2$p < .05, "sig", "nsig")
table(meta.profsum2$sig)
binom.test(table(meta.profsum2$sig)[2],
           dim(meta.profsum2)[1]) 
meta.profsum2$sig.mf <- ifelse(meta.profsum2$p < .05 & meta.profsum2$mean < 0, "sig.f", 
                                ifelse(meta.profsum2$p < .05 & meta.profsum2$mean > 0, "sig.m", "nsig"))
table(meta.profsum2$sig.mf)/dim(meta.profsum2)[1]


## How many fall beyond [-0.1, 0.1]?
meta.profsum2$effcut <- cut(meta.profsum2$mean, breaks = c(-2, -0.1, 0.1, 2), labels = c("sig.f", "nsig", "sig.m"))
table(meta.profsum2$effcut) 
table(meta.profsum2$effcut)/dim(meta.profsum2)[1] 
binom.test(sum(table(meta.profsum2$effcut)[c(1, 3)]),
           dim(meta.profsum2)[1]) 

## How many fall beyond [-0.2, 0.2]?
meta.profsum2$effcut2 <- cut(meta.profsum2$mean, breaks = c(-2, -0.2, 0.2, 2), labels = c("sig.f", "nsig", "sig.m"))
table(meta.profsum2$effcut2) 
table(meta.profsum2$effcut2)/dim(meta.profsum2)[1]
binom.test(sum(table(meta.profsum2$effcut2)[c(1, 3)]),
           dim(meta.profsum2)[1]) 

## How many fall beyond [-0.3, 0.3]?
meta.profsum2$effcut3 <- cut(meta.profsum2$mean, breaks = c(-2, -0.3, 0.3, 2), labels = c("sig.f", "nsig", "sig.m"))
table(meta.profsum2$effcut3) 
table(meta.profsum2$effcut3)/dim(meta.profsum2)[1] 
binom.test(sum(table(meta.profsum2$effcut3)[c(1, 3)]),
           dim(meta.profsum2)[1]) 

## Are profs more female or more male?
t.test(meta.profsum2$mean, mu = 0)
meta.profsum2$malfem <- ifelse(meta.profsum2$mean > 0, "mal", "fem")
table(meta.profsum2$malfem) 
binom.test(table(meta.profsum2$malfem)[2],
           dim(meta.profsum2)[1]) 

# Note: Results for each corpus are reported in main text (Table 4)
# Within child-produced speech
head(meta.profsum2$att[order(meta.profsum2$effchchild, decreasing = TRUE)])
head(meta.profsum2$att[order(meta.profsum2$effchchild, decreasing = FALSE)])
head(meta.profsum2$effchchild[order(meta.profsum2$effchchild, decreasing = TRUE)])
head(meta.profsum2$effchchild[order(meta.profsum2$effchchild, decreasing = FALSE)])

# Within child-directed speech
head(meta.profsum2$att[order(meta.profsum2$effchpar, decreasing = TRUE)])
head(meta.profsum2$att[order(meta.profsum2$effchpar, decreasing = FALSE)])
head(meta.profsum2$effchpar[order(meta.profsum2$effchpar, decreasing = TRUE)])
head(meta.profsum2$effchpar[order(meta.profsum2$effchpar, decreasing = FALSE)])

# Within child-directed books
head(meta.profsum2$att[order(meta.profsum2$effchbook, decreasing = TRUE)])
head(meta.profsum2$att[order(meta.profsum2$effchbook, decreasing = FALSE)])
head(meta.profsum2$effchbook[order(meta.profsum2$effchbook, decreasing = TRUE)])
head(meta.profsum2$effchbook[order(meta.profsum2$effchbook, decreasing = FALSE)])

# Within child-directed TV
head(meta.profsum2$att[order(meta.profsum2$effchtv, decreasing = TRUE)])
head(meta.profsum2$att[order(meta.profsum2$effchtv, decreasing = FALSE)])
head(meta.profsum2$effchtv[order(meta.profsum2$effchtv, decreasing = TRUE)])
head(meta.profsum2$effchtv[order(meta.profsum2$effchtv, decreasing = FALSE)])

# Within adult-directed speech
head(meta.profsum2$att[order(meta.profsum2$effadspch, decreasing = TRUE)])
head(meta.profsum2$att[order(meta.profsum2$effadspch, decreasing = FALSE)])
head(meta.profsum2$effadspch[order(meta.profsum2$effadspch, decreasing = TRUE)])
head(meta.profsum2$effadspch[order(meta.profsum2$effadspch, decreasing = FALSE)])

# Within adult-directed books
head(meta.profsum2$att[order(meta.profsum2$effguten, decreasing = TRUE)])
head(meta.profsum2$att[order(meta.profsum2$effguten, decreasing = FALSE)])
head(meta.profsum2$effguten[order(meta.profsum2$effguten, decreasing = TRUE)])
head(meta.profsum2$effguten[order(meta.profsum2$effguten, decreasing = FALSE)])

# Within adult-directed tv
head(meta.profsum2$att[order(meta.profsum2$effadtv, decreasing = TRUE)])
head(meta.profsum2$att[order(meta.profsum2$effadtv, decreasing = FALSE)])
head(meta.profsum2$effadtv[order(meta.profsum2$effadtv, decreasing = TRUE)])
head(meta.profsum2$effadtv[order(meta.profsum2$effadtv, decreasing = FALSE)])

## (5d) Meta-regression across results ----
# combine all effects, collapsing across the content of the prof

metareg.profsum <- as.data.frame(c(meta.profsum2$effchchild, meta.profsum2$effchpar, 
                                    meta.profsum2$effchbook, meta.profsum2$effguten,
                                    meta.profsum2$effchtv, meta.profsum2$effadtv,
                                    meta.profsum2$effadspch))
metareg.profsum$se <- c(meta.profsum2$sechchild, meta.profsum2$sechpar, 
                         meta.profsum2$sechbook, meta.profsum2$seguten,
                         meta.profsum2$sechtv, meta.profsum2$seadtv,
                         meta.profsum2$seadspch)
metareg.profsum$corpus <- rep(c("effchchild", "effchpar", "effchbook", 
                                 "effguten", "effchtv", "effadtv", "effadspch"), each = 82)
metareg.profsum$corpus <- factor(metareg.profsum$corpus, 
                                  levels = c("effchchild", "effchpar", "effchbook", "effchtv",
                                             "effadspch", "effguten", "effadtv"))
metareg.profsum$chad <- car::recode(metareg.profsum$corpus, 
                                     "'effchpar' = 'child'; 'effchbook' = 'child'; 'effchtv' = 'child'; 'effchchild' = 'child';
                          'effadspch' = 'adult'; 'effguten' = 'adult'; 'effadtv' = 'adult'")
metareg.profsum$chad <- factor(metareg.profsum$chad, levels = c("child", "adult"))
table(metareg.profsum$chad)

metareg.profsum$time <- car::recode(metareg.profsum$corpus, "'effchpar' = 'mid'; 'effchbook' = 'early'; 
                          'effchtv' = 'late'; 'effchchild' = 'mid';
                          'effadspch' = 'mid'; 'effguten' = 'early'; 'effadtv' = 'late'")
metareg.profsum$time <- factor(metareg.profsum$time, levels = c("early", "mid", "late"))
table(metareg.profsum$time)

colnames(metareg.profsum) <- c("eff.correct", "se", "corpus", "chad", "time")

meta.study3 <- metagen(TE = eff.correct, seTE = se, sm = "MD", data = metareg.profsum)

# (5d.1) Comparisons across two age subgroups (child and adult) ----
table(metareg.profsum$chad)
metareg.study3.chad <- metareg(meta.study3, chad)
summary(metareg.study3.chad) # no significant difference

bubble.metareg(metareg.study3.chad, ylim = c(-2, 2),
               xlab = "Age Category",
               ylab = "Mean Size of Association Effects",
               main = "Covariance for Age Categories")

# (5d.2) Comparisons across three time subgroups (early, mid, late) ----
table(metareg.profsum$time)
metareg.study3.time <- metareg(meta.study3, paste(time))
summary(metareg.study3.time) # significant differences; mid and late are lower than early



# (5e) Plotting single prof stimuli results ----
# Also see sm_bwplotting for two-page plot
# Meta-analytic effects
meta.profsum[order(meta.profsum$mean, decreasing = TRUE),]
# with cut-offs
# [-0.3, 0.3] writer to designer
# [-0.2, 0.2] painter to grader
# [-0.1, 0.1] hairdresser to psychologist

#male
pdf(file = "metaprof_plot_efver_Male.pdf", width = 25, height = 8)
op <- par(mar = c(12, 8, 2, 2))
plot(meta.profsum$mean[order(meta.profsum$mean, decreasing = TRUE)][1:50], ylim = c(-1.6, 1.6),
     xlab = "", ylab = "", axes = FALSE,
     pch = 20, cex = 2, col = "blue")
box()
axis(1, at = 1:dim(meta.profsum)[1], 
     labels = meta.profsum$att[order(meta.profsum$mean, decreasing = TRUE)], las = 2, cex.axis = 1.7)
axis(2, at = seq(-1.6, 1.6, by = 0.4), labels = round(seq(-1.6, 1.6, by = 0.4), 2), las = 1, cex.axis = 1.7)
mtext("Meta-analytic Effect Sizes", line = 4, side = 2, cex = 2)
abline(h = 0, lty = 2, col = "black", lwd = 4)
segments(x0 = c(1:dim(meta.profsum)[1]), x1 = c(1:dim(meta.profsum)[1]), 
         y0 = meta.profsum$mean[order(meta.profsum$mean, decreasing = TRUE)] - 1.96*meta.profsum$se[order(meta.profsum$mean, decreasing = TRUE)],
         y1 = meta.profsum$mean[order(meta.profsum$mean, decreasing = TRUE)] + 1.96*meta.profsum$se[order(meta.profsum$mean, decreasing = TRUE)],
         lty = 1, lwd = 2, col = "darkgray")
par(op)
dev.off()

#female
pdf(file = "metaprof_plot_efver_Female.pdf", width = 25, height = 8)
op <- par(mar = c(12, 8, 2, 2))
plot(meta.profsum$mean[order(meta.profsum$mean, decreasing = TRUE)][82:50], ylim = c(-1.6, 1.6),
     xlab = "", ylab = "", axes = FALSE,
     pch = 20, cex = 2, col = "red")
box()
axis(1, at = 1:dim(meta.profsum)[1], 
     labels = meta.profsum$att[order(meta.profsum$mean, decreasing = FALSE)], las = 2, cex.axis = 1.7)
axis(2, at = seq(-1.6, 1.6, by = 0.4), labels = round(seq(-1.6, 1.6, by = 0.4), 2), las = 1, cex.axis = 1.7)
mtext("Meta-analytic Effect Sizes", line = 4, side = 2, cex = 2)
abline(h = 0, lty = 2, col = "black", lwd = 4)
segments(x0 = c(1:dim(meta.profsum)[1]), x1 = c(1:dim(meta.profsum)[1]), 
         y0 = meta.profsum$mean[order(meta.profsum$mean, decreasing = FALSE)] - 1.96*meta.profsum$se[order(meta.profsum$mean, decreasing = TRUE)],
         y1 = meta.profsum$mean[order(meta.profsum$mean, decreasing = FALSE)] + 1.96*meta.profsum$se[order(meta.profsum$mean, decreasing = TRUE)],
         lty = 1, lwd = 2, col = "darkgrey")

par(op)
dev.off()

pdf(file = "metaprof_plot_both.pdf", width = 32, height = 14)
op <- par(mar = c(12, 8, 2, 2))
plot(meta.profsum$mean[order(meta.profsum$mean, decreasing = TRUE)], ylim = c(-1.6, 1.6),
     xlab = "", ylab = "", axes = FALSE,
     pch = 20, cex = 2, col = "blue")
box()
axis(1, at = 1:dim(meta.profsum)[1], 
     labels = meta.profsum$att[order(meta.profsum$mean, decreasing = TRUE)], las = 2, cex.axis = 2.1)
axis(2, at = seq(-1.6, 1.6, by = 0.4), labels = round(seq(-1.6, 1.6, by = 0.4), 2), las = 1, cex.axis = 1.8)
mtext("Meta-analytic Effect Sizes", line = 4, side = 2, cex = 3)
abline(h = 0, lty = 2, col = "black", lwd = 4)
segments(x0 = c(1:dim(meta.profsum)[1]), x1 = c(1:dim(meta.profsum)[1]), 
         y0 = meta.profsum$mean[order(meta.profsum$mean, decreasing = TRUE)] - 1.96*meta.profsum$se[order(meta.profsum$mean, decreasing = TRUE)],
         y1 = meta.profsum$mean[order(meta.profsum$mean, decreasing = TRUE)] + 1.96*meta.profsum$se[order(meta.profsum$mean, decreasing = TRUE)],
         lty = 1, lwd = 2, col = "darkgrey")
abline(v = which(meta.profsum$att[order(meta.profsum$mean, decreasing = TRUE)] == "writer"), 
       col = "red", lty = 2, lwd = 3)
abline(v = which(meta.profsum$att[order(meta.profsum$mean, decreasing = TRUE)] == "designer"),
       col = "red", lty = 2, lwd = 3)
abline(v = which(meta.profsum$att[order(meta.profsum$mean, decreasing = TRUE)] == "painter"), 
       col = "orange", lty = 2, lwd = 3)
abline(v = which(meta.profsum$att[order(meta.profsum$mean, decreasing = TRUE)] == "grader"),
       col = "orange", lty = 2, lwd = 3)
abline(v = which(meta.profsum$att[order(meta.profsum$mean, decreasing = TRUE)] == "hairdresser"), 
       col = "seagreen", lty = 2, lwd = 3)
abline(v = which(meta.profsum$att[order(meta.profsum$mean, decreasing = TRUE)] == "psychologist"),
       col = "seagreen", lty = 2, lwd = 3)
abline(v = which(meta.profsum$mean[order(meta.profsum$att, decreasing = TRUE)] == "0"),
       col = "black", lty = 2, lwd = 3)
legend("topright", c("[-0.3, 0.3]", "[-0.2, 0.2]", "[-0.1, 0.1]"), col = c("red", "orange", "seagreen"), lty = 2, lwd = 4, bty = "n", cex = 3)
par(op)
dev.off()


# (5f) Read in BLS data and perform correlations ----
# Note: Results are reported in main text
profdat.bls <- read.csv("professions_bls.csv") # included in .RData summary
colnames(profdat.bls) <- c("att", "perc.f")

meta.profsum.bls <- merge(meta.profsum2, profdat.bls, by = "att")
meta.profsum.bls$perc.m <- 100 - meta.profsum.bls$perc.f

## Average distribution of females in a profession?
mean(meta.profsum.bls$perc.f)

# Correlation bt. eff size and percentage of male
# Prediction = stronger positive effects (more male-association) 
# should be positively correlated with the percentage of males in the profession
cor.test(meta.profsum.bls$mean, meta.profsum.bls$perc.m)
mean(meta.profsum.bls$mean)
# Correlation bt. eff size and percentage of male within only the child-produced speech
cor.test(meta.profsum.bls$effchchild, meta.profsum.bls$perc.m)

# Additional analysis: Correlation bt. eff size and percentage of male within all other corpora
# Note: results are reported in supplementary materials 
cor.test(meta.profsum.bls$effchpar, meta.profsum.bls$perc.m)
cor.test(meta.profsum.bls$effchbook, meta.profsum.bls$perc.m) 
cor.test(meta.profsum.bls$effchtv, meta.profsum.bls$perc.m) 
cor.test(meta.profsum.bls$effadspch, meta.profsum.bls$perc.m) 
cor.test(meta.profsum.bls$effguten, meta.profsum.bls$perc.m) 
cor.test(meta.profsum.bls$effadtv, meta.profsum.bls$perc.m) 


# Plot result
cor.bls.plot <- ggplot(meta.profsum.bls, aes(perc.m, mean, label = att))
options(ggrepel.max.overlaps = Inf)
pdf(file = "metaprof_blscor_bw_ggreprel.pdf", width = 15, height = 10)
cor.bls.plot + geom_text_repel(colour = "black", size = 6) +
  geom_point(aes(x=perc.m, y=mean), color="black", pch=20, size=1.2, na.rm=T) +
  geom_smooth(formula = y ~ x,aes(x=perc.m, y=mean), method="lm", color="black", fill=NA, size=2, na.rm=T) +
  labs(title="Occupations Sorted by Mean Effect Sizes and Percentage of Gender in Occupation", x="Percentage Male in Occupation", y="Effect Size (Occupation = Male)") +
  scale_x_continuous(limits=c(0,100), breaks=seq(from=0, to=100, by=25), expand=c(0.02,0.02)) +
  scale_y_continuous(limits=c(-1, 1), breaks=seq(from=-1, to=1, by=0.5), expand=c(0.05,0.05)) +
  theme_bw() +
  theme(plot.title=element_text(color="black",size=20, face="bold", hjust=0.5, vjust=0, family="Helvetica"), 
        axis.title.y = element_text(colour = "black", size=20, family="Helvetica"),
        axis.title.x = element_text(colour = "black", size=20, family="Helvetica"),
        axis.text.y = element_text(colour = "black", hjust = 1, size=20, family="Helvetica"),
        axis.text.x = element_text(colour = "black", hjust = 0.45, size=20, family="Helvetica"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        legend.position = "none", legend.title=element_blank(), 
        legend.text=element_text(colour = "black", size = 10, family="Helvetica"), 
        legend.background = element_rect(fill="transparent", size=0.5), 
        axis.line = element_line(colour = "black"), 
        axis.ticks = element_line(colour = "black"), 
        panel.border = element_blank(), panel.background = element_blank())
dev.off()


## (6) Building up main plots for presentation---- whole new
## All in color (see SM file for BW plotting file)
metadatsum2 <- metadatsum[1:4,]
metadatsum2 # mfgb, mfwh, mfsa, mfmr
metadat.plot <- metadat[metadat$cat != "weapons vs. instruments",] # Remove instruments/weapons for main plot
metadat.plot$att2 <- factor(metadat.plot$att, 
                            levels = c("bad vs. good", "work vs. home", "science vs. arts", "math vs. reading"))
metadat.plot <- arrange(metadat.plot, att2)

## Only meta-effects 
pdf(file = "presentplot1.pdf", width = 12, height = 10)
op <- par(mar = c(5,6,2,2))
plot(1, type="n", xlab="", ylab="", 
     xlim=c(0.5, 4.5), ylim=c(-2.25, 2.25), axes = FALSE)
box()
mtext("Effect Size", side = 2, line = 3, cex = 2)
axis(1, at = c(1:4), labels = c("Male-Bad\nFemale-Good", "Male-Work\nFemale-Home", 
                                "Male-Science\nFemale-Arts", "Male-Math\nFemale-Reading"),
     cex.axis = 1.4, padj = 1)
axis(2, at = seq(-2,2, by = 0.5), labels = seq(-2,2, by = 0.5), las = 2, cex.axis = 1.5)
abline(h = 0, lty = 2)

points(seq(1, 4, by = 1),
       metadatsum2$metamean, 
       pch = 17, cex = 3, col = "red")
segments(x0 = seq(1, 4, by = 1), x1 = seq(1, 4, by = 1),
         y0 = metadatsum2$meta95low,
         y1 = metadatsum2$meta95high, 
         lty = 1, lwd = 3, col = "red")

legend("bottomleft", c("Child-produced speech", "Child-directed speech",
                       "Child-directed books", "Child-directed media",
                       "Adult-produced speech", "Adult-directed books", 
                       "Adult-directed media", "Meta Estimate"),
       col = c("chartreuse3", "cyan2", "cadetblue", "darkblue",
               "gold1", "darkorange", "plum2", "red"),
       pch = c(rep(20, 7), 17), 
       cex = 2, pt.cex = c(rep(3, 7), 2.5), bty = "n", ncol = 2)

par(op)
dev.off()


## + Child produced speech 
pdf(file = "presentplot2.pdf", width = 12, height = 10)
op <- par(mar = c(5,6,2,2))
plot(1, type="n", xlab="", ylab="", 
     xlim=c(0.5, 4.5), ylim=c(-2.25, 2.25), axes = FALSE)
box()
mtext("Effect Size", side = 2, line = 3, cex = 2)
axis(1, at = c(1:4), labels = c("Male-Bad\nFemale-Good", "Male-Work\nFemale-Home", 
                                "Male-Science\nFemale-Arts", "Male-Math\nFemale-Reading"),
     cex.axis = 1.4, padj = 1)
axis(2, at = seq(-2,2, by = 0.5), labels = seq(-2,2, by = 0.5), las = 2, cex.axis = 1.5)
abline(h = 0, lty = 2)

points(seq(1, 4, by = 1),
       metadatsum2$metamean, 
       pch = 17, cex = 3, col = "red")
segments(x0 = seq(1, 4, by = 1), x1 = seq(1, 4, by = 1),
         y0 = metadatsum2$meta95low,
         y1 = metadatsum2$meta95high, 
         lty = 1, lwd = 3, col = "red")

points(seq(0.8, 3.8, by = 1),
       metadat.plot$eff.correct[metadat.plot$data=="childes_children"], 
       pch = 20, cex = 2, col = "chartreuse3")
segments(x0 = seq(0.8, 3.8, by = 1), x1 = seq(0.8, 3.8, by = 1),
         y0 = metadat.plot$eff.correct[metadat.plot$data=="childes_children"] - 1.96*metadat.plot$se[metadat.plot$data=="childes_children"],
         y1 = metadat.plot$eff.correct[metadat.plot$data=="childes_children"] + 1.96*metadat.plot$se[metadat.plot$data=="childes_children"], 
         lty = 1, lwd = 3, col = "chartreuse3")


legend("bottomleft", c("Child-produced speech", "Child-directed speech",
                       "Child-directed books", "Child-directed media",
                       "Adult-produced speech", "Adult-directed books", 
                       "Adult-directed media", "Meta Estimate"),
       col = c("chartreuse3", "cyan2", "cadetblue", "darkblue",
               "gold1", "darkorange", "plum2", "red"),
       pch = c(rep(20, 7), 17), 
       cex = 2, pt.cex = c(rep(3, 7), 2.5), bty = "n", ncol = 2)

par(op)
dev.off()


## + Child-directed corpora
pdf(file = "presentplot3.pdf", width = 12, height = 10)
op <- par(mar = c(5,6,2,2))
plot(1, type="n", xlab="", ylab="", 
     xlim=c(0.5, 4.5), ylim=c(-2.25, 2.25), axes = FALSE)
box()
mtext("Effect Size", side = 2, line = 3, cex = 2)
axis(1, at = c(1:4), labels = c("Male-Bad\nFemale-Good", "Male-Work\nFemale-Home", 
                                "Male-Science\nFemale-Arts", "Male-Math\nFemale-Reading"),
     cex.axis = 1.4, padj = 1)
axis(2, at = seq(-2,2, by = 0.5), labels = seq(-2,2, by = 0.5), las = 2, cex.axis = 1.5)
abline(h = 0, lty = 2)

points(seq(1, 4, by = 1),
       metadatsum2$metamean, 
       pch = 17, cex = 3, col = "red")
segments(x0 = seq(1, 4, by = 1), x1 = seq(1, 4, by = 1),
         y0 = metadatsum2$meta95low,
         y1 = metadatsum2$meta95high, 
         lty = 1, lwd = 3, col = "red")

points(seq(0.8, 3.8, by = 1),
       metadat.plot$eff.correct[metadat.plot$data=="childes_children"], 
       pch = 20, cex = 2, col = "chartreuse3")
segments(x0 = seq(0.8, 3.8, by = 1), x1 = seq(0.8, 3.8, by = 1),
         y0 = metadat.plot$eff.correct[metadat.plot$data=="childes_children"] - 1.96*metadat.plot$se[metadat.plot$data=="childes_children"],
         y1 = metadat.plot$eff.correct[metadat.plot$data=="childes_children"] + 1.96*metadat.plot$se[metadat.plot$data=="childes_children"], 
         lty = 1, lwd = 3, col = "chartreuse3")

points(seq(0.85, 3.85, by = 1),
       metadat.plot$eff.correct[metadat.plot$data=="childes_parents"], 
       pch = 20, cex = 2, col = "cyan2")
segments(x0 = seq(0.85, 3.85, by = 1), x1 = seq(0.85, 3.85, by = 1),
         y0 = metadat.plot$eff.correct[metadat.plot$data=="childes_parents"] - 1.96*metadat.plot$se[metadat.plot$data=="childes_parents"],
         y1 = metadat.plot$eff.correct[metadat.plot$data=="childes_parents"] + 1.96*metadat.plot$se[metadat.plot$data=="childes_parents"], 
         lty = 1, lwd = 3, col = "cyan2")
points(seq(0.9, 3.9, by = 1),
       metadat.plot$eff.correct[metadat.plot$data=="child_books"], 
       pch = 20, cex = 2, col = "cadetblue")
segments(x0 = seq(0.9, 3.9, by = 1), x1 = seq(0.9, 3.9, by = 1),
         y0 = metadat.plot$eff.correct[metadat.plot$data=="child_books"] - 1.96*metadat.plot$se[metadat.plot$data=="child_books"],
         y1 = metadat.plot$eff.correct[metadat.plot$data=="child_books"] + 1.96*metadat.plot$se[metadat.plot$data=="child_books"], 
         lty = 1, lwd = 3, col = "cadetblue")
points(seq(0.95, 3.95, by = 1),
       metadat.plot$eff.correct[metadat.plot$data=="kids_tv_combined"], 
       pch = 20, cex = 2, col = "darkblue")
segments(x0 = seq(0.95, 3.95, by = 1), x1 = seq(0.95, 3.95, by = 1),
         y0 = metadat.plot$eff.correct[metadat.plot$data=="kids_tv_combined"] - 1.96*metadat.plot$se[metadat.plot$data=="kids_tv_combined"],
         y1 = metadat.plot$eff.correct[metadat.plot$data=="kids_tv_combined"] + 1.96*metadat.plot$se[metadat.plot$data=="kids_tv_combined"], 
         lty = 1, lwd = 3, col = "darkblue")


legend("bottomleft", c("Child-produced speech", "Child-directed speech",
                       "Child-directed books", "Child-directed media",
                       "Adult-produced speech", "Adult-directed books", 
                       "Adult-directed media", "Meta Estimate"),
       col = c("chartreuse3", "cyan2", "cadetblue", "darkblue",
               "gold1", "darkorange", "plum2", "red"),
       pch = c(rep(20, 7), 17), 
       cex = 2, pt.cex = c(rep(3, 7), 2.5), bty = "n", ncol = 2)

par(op)
dev.off()


## + Adult data 
pdf(file = "presentplot4.pdf", width = 12, height = 10)
op <- par(mar = c(5,6,2,2))
plot(1, type="n", xlab="", ylab="", 
     xlim=c(0.5, 4.5), ylim=c(-2.25, 2.25), axes = FALSE)
box()
mtext("Effect Size", side = 2, line = 3, cex = 2)
axis(1, at = c(1:4), labels = c("Male-Bad\nFemale-Good", "Male-Work\nFemale-Home", 
                                "Male-Science\nFemale-Arts", "Male-Math\nFemale-Reading"),
     cex.axis = 1.4, padj = 1)
axis(2, at = seq(-2,2, by = 0.5), labels = seq(-2,2, by = 0.5), las = 2, cex.axis = 1.5)
abline(h = 0, lty = 2)

points(seq(1, 4, by = 1),
       metadatsum2$metamean, 
       pch = 17, cex = 3, col = "red")
segments(x0 = seq(1, 4, by = 1), x1 = seq(1, 4, by = 1),
         y0 = metadatsum2$meta95low,
         y1 = metadatsum2$meta95high, 
         lty = 1, lwd = 3, col = "red")

points(seq(0.8, 3.8, by = 1),
       metadat.plot$eff.correct[metadat.plot$data=="childes_children"], 
       pch = 18, cex = 3, col = "chartreuse3")
segments(x0 = seq(0.8, 3.8, by = 1), x1 = seq(0.8, 3.8, by = 1),
         y0 = metadat.plot$eff.correct[metadat.plot$data=="childes_children"] - 1.96*metadat.plot$se[metadat.plot$data=="childes_children"],
         y1 = metadat.plot$eff.correct[metadat.plot$data=="childes_children"] + 1.96*metadat.plot$se[metadat.plot$data=="childes_children"], 
         lty = 1, lwd = 3, col = "chartreuse3")

points(seq(0.85, 3.85, by = 1),
       metadat.plot$eff.correct[metadat.plot$data=="childes_parents"], 
       pch = 19, cex = 2, col = "cyan2")
segments(x0 = seq(0.85, 3.85, by = 1), x1 = seq(0.85, 3.85, by = 1),
         y0 = metadat.plot$eff.correct[metadat.plot$data=="childes_parents"] - 1.96*metadat.plot$se[metadat.plot$data=="childes_parents"],
         y1 = metadat.plot$eff.correct[metadat.plot$data=="childes_parents"] + 1.96*metadat.plot$se[metadat.plot$data=="childes_parents"], 
         lty = 1, lwd = 3, col = "cyan2")
points(seq(0.9, 3.9, by = 1),
       metadat.plot$eff.correct[metadat.plot$data=="child_books"], 
       pch = 19, cex = 2, col = "cadetblue")
segments(x0 = seq(0.9, 3.9, by = 1), x1 = seq(0.9, 3.9, by = 1),
         y0 = metadat.plot$eff.correct[metadat.plot$data=="child_books"] - 1.96*metadat.plot$se[metadat.plot$data=="child_books"],
         y1 = metadat.plot$eff.correct[metadat.plot$data=="child_books"] + 1.96*metadat.plot$se[metadat.plot$data=="child_books"], 
         lty = 1, lwd = 3, col = "cadetblue")
points(seq(0.95, 3.95, by = 1),
       metadat.plot$eff.correct[metadat.plot$data=="kids_tv_combined"], 
       pch = 19, cex = 2, col = "darkblue")
segments(x0 = seq(0.95, 3.95, by = 1), x1 = seq(0.95, 3.95, by = 1),
         y0 = metadat.plot$eff.correct[metadat.plot$data=="kids_tv_combined"] - 1.96*metadat.plot$se[metadat.plot$data=="kids_tv_combined"],
         y1 = metadat.plot$eff.correct[metadat.plot$data=="kids_tv_combined"] + 1.96*metadat.plot$se[metadat.plot$data=="kids_tv_combined"], 
         lty = 1, lwd = 3, col = "darkblue")

points(seq(1.05, 4.05, by = 1),
       metadat.plot$eff.correct[metadat.plot$data=="adult_speech"], 
       pch = 15, cex = 2, col = "gold1")
segments(x0 = seq(1.05, 4.05, by = 1), x1 = seq(1.05, 4.05, by = 1),
         y0 = metadat.plot$eff.correct[metadat.plot$data=="adult_speech"] - 1.96*metadat.plot$se[metadat.plot$data=="adult_speech"],
         y1 = metadat.plot$eff.correct[metadat.plot$data=="adult_speech"] + 1.96*metadat.plot$se[metadat.plot$data=="adult_speech"], 
         lty = 1, lwd = 3, col = "gold1")
points(seq(1.1, 4.1, by = 1),
       metadat.plot$eff.correct[metadat.plot$data=="gutenberg"], 
       pch = 15, cex = 2, col = "darkorange")
segments(x0 = seq(1.1, 4.1, by = 1), x1 = seq(1.1, 4.1, by = 1),
         y0 = metadat.plot$eff.correct[metadat.plot$data=="gutenberg"] - 1.96*metadat.plot$se[metadat.plot$data=="gutenberg"],
         y1 = metadat.plot$eff.correct[metadat.plot$data=="gutenberg"] + 1.96*metadat.plot$se[metadat.plot$data=="gutenberg"], 
         lty = 1, lwd = 3, col = "darkorange")
points(seq(1.15, 4.15, by = 1),
       metadat.plot$eff.correct[metadat.plot$data=="simply_scripts"], 
       pch = 15, cex = 2, col = "plum2")
segments(x0 = seq(1.15, 4.15, by = 1), x1 = seq(1.15, 4.15, by = 1),
         y0 = metadat.plot$eff.correct[metadat.plot$data=="simply_scripts"] - 1.96*metadat.plot$se[metadat.plot$data=="simply_scripts"],
         y1 = metadat.plot$eff.correct[metadat.plot$data=="simply_scripts"] + 1.96*metadat.plot$se[metadat.plot$data=="simply_scripts"], 
         lty = 1, lwd = 3, col = "plum2")


legend("bottomleft", c("Child-produced speech", "Child-directed speech",
                       "Child-directed books", "Child-directed media",
                       "Adult-produced speech", "Adult-directed books", 
                       "Adult-directed media", "Meta Estimate"),
       col = c("chartreuse3", "cyan2", "cadetblue", "darkblue",
               "gold1", "darkorange", "plum2", "red"),
       pch = c(18, rep(19, 3), rep(15, 3), 17), 
       cex = 2, pt.cex = c(3, rep(2.5, 6), 3), bty = "n", ncol = 2)

par(op)
dev.off()


# + Instrument-weapon data 
embdoub.plot2 <- embdoub[embdoub$cat == "weapons vs. instruments",]
metadatsum[5,]

pdf(file = "presentplot.iw.pdf", width = 14, height = 10)
op <- par(mar = c(5,6,2,2))
plot(1, type="n", xlab="", ylab="", 
     xlim=c(0.5, 5.5), ylim=c(-2, 3), axes = FALSE)
box()
mtext("Effect Size", side = 2, line = 3, cex = 2)
axis(1, at = c(1:5), labels = c("Male-Bad\nFemale-Good", "Male-Work\nFemale-Home", 
                                "Male-Science\nFemale-Arts", "Male-Math\nFemale-Reading",
                                "Weapon-Bad\nInstrument-Good"),
     cex.axis = 1.4, padj = 1)
axis(2, at = seq(-2,3, by = 0.5), labels = seq(-2,3, by = 0.5), las = 2, cex.axis = 1.5)
abline(h = 0, lty = 2)

points(seq(1, 5, by = 1),
       metadatsum$metamean, 
       pch = 17, cex = 3, col = "red")
segments(x0 = seq(1, 5, by = 1), x1 = seq(1, 5, by = 1),
         y0 = metadatsum$meta95low,
         y1 = metadatsum$meta95high, 
         lty = 1, lwd = 3, col = "red")

points(seq(0.8, 3.8, by = 1),
       metadat.plot$eff.correct[metadat.plot$data=="childes_children"], 
       pch = 20, cex = 2, col = "chartreuse3")
segments(x0 = seq(0.8, 3.8, by = 1), x1 = seq(0.8, 3.8, by = 1),
         y0 = metadat.plot$eff.correct[metadat.plot$data=="childes_children"] - 1.96*metadat.plot$se[metadat.plot$data=="childes_children"],
         y1 = metadat.plot$eff.correct[metadat.plot$data=="childes_children"] + 1.96*metadat.plot$se[metadat.plot$data=="childes_children"], 
         lty = 1, lwd = 3, col = "chartreuse3")

points(seq(0.85, 3.85, by = 1),
       metadat.plot$eff.correct[metadat.plot$data=="childes_parents"], 
       pch = 20, cex = 2, col = "cyan2")
segments(x0 = seq(0.85, 3.85, by = 1), x1 = seq(0.85, 3.85, by = 1),
         y0 = metadat.plot$eff.correct[metadat.plot$data=="childes_parents"] - 1.96*metadat.plot$se[metadat.plot$data=="childes_parents"],
         y1 = metadat.plot$eff.correct[metadat.plot$data=="childes_parents"] + 1.96*metadat.plot$se[metadat.plot$data=="childes_parents"], 
         lty = 1, lwd = 3, col = "cyan2")
points(seq(0.9, 3.9, by = 1),
       metadat.plot$eff.correct[metadat.plot$data=="child_books"], 
       pch = 20, cex = 2, col = "cadetblue")
segments(x0 = seq(0.9, 3.9, by = 1), x1 = seq(0.9, 3.9, by = 1),
         y0 = metadat.plot$eff.correct[metadat.plot$data=="child_books"] - 1.96*metadat.plot$se[metadat.plot$data=="child_books"],
         y1 = metadat.plot$eff.correct[metadat.plot$data=="child_books"] + 1.96*metadat.plot$se[metadat.plot$data=="child_books"], 
         lty = 1, lwd = 3, col = "cadetblue")
points(seq(0.95, 3.95, by = 1),
       metadat.plot$eff.correct[metadat.plot$data=="kids_tv_combined"], 
       pch = 20, cex = 2, col = "darkblue")
segments(x0 = seq(0.95, 3.95, by = 1), x1 = seq(0.95, 3.95, by = 1),
         y0 = metadat.plot$eff.correct[metadat.plot$data=="kids_tv_combined"] - 1.96*metadat.plot$se[metadat.plot$data=="kids_tv_combined"],
         y1 = metadat.plot$eff.correct[metadat.plot$data=="kids_tv_combined"] + 1.96*metadat.plot$se[metadat.plot$data=="kids_tv_combined"], 
         lty = 1, lwd = 3, col = "darkblue")

points(seq(1.05, 4.05, by = 1),
       metadat.plot$eff.correct[metadat.plot$data=="adult_speech"], 
       pch = 20, cex = 2, col = "gold1")
segments(x0 = seq(1.05, 4.05, by = 1), x1 = seq(1.05, 4.05, by = 1),
         y0 = metadat.plot$eff.correct[metadat.plot$data=="adult_speech"] - 1.96*metadat.plot$se[metadat.plot$data=="adult_speech"],
         y1 = metadat.plot$eff.correct[metadat.plot$data=="adult_speech"] + 1.96*metadat.plot$se[metadat.plot$data=="adult_speech"], 
         lty = 1, lwd = 3, col = "gold1")
points(seq(1.1, 4.1, by = 1),
       metadat.plot$eff.correct[metadat.plot$data=="gutenberg"], 
       pch = 20, cex = 2, col = "darkorange")
segments(x0 = seq(1.1, 4.1, by = 1), x1 = seq(1.1, 4.1, by = 1),
         y0 = metadat.plot$eff.correct[metadat.plot$data=="gutenberg"] - 1.96*metadat.plot$se[metadat.plot$data=="gutenberg"],
         y1 = metadat.plot$eff.correct[metadat.plot$data=="gutenberg"] + 1.96*metadat.plot$se[metadat.plot$data=="gutenberg"], 
         lty = 1, lwd = 3, col = "darkorange")
points(seq(1.15, 4.15, by = 1),
       metadat.plot$eff.correct[metadat.plot$data=="simply_scripts"], 
       pch = 20, cex = 2, col = "plum2")
segments(x0 = seq(1.15, 4.15, by = 1), x1 = seq(1.15, 4.15, by = 1),
         y0 = metadat.plot$eff.correct[metadat.plot$data=="simply_scripts"] - 1.96*metadat.plot$se[metadat.plot$data=="simply_scripts"],
         y1 = metadat.plot$eff.correct[metadat.plot$data=="simply_scripts"] + 1.96*metadat.plot$se[metadat.plot$data=="simply_scripts"], 
         lty = 1, lwd = 3, col = "plum2")


points(seq(0.8, 3.8, by = 1),
       metadat.plot$eff.correct[metadat.plot$data=="childes_children"], 
       pch = 20, cex = 2, col = "chartreuse3")
segments(x0 = seq(0.8, 3.8, by = 1), x1 = seq(0.8, 3.8, by = 1),
         y0 = metadat.plot$eff.correct[metadat.plot$data=="childes_children"] - 1.96*metadat.plot$se[metadat.plot$data=="childes_children"],
         y1 = metadat.plot$eff.correct[metadat.plot$data=="childes_children"] + 1.96*metadat.plot$se[metadat.plot$data=="childes_children"], 
         lty = 1, lwd = 3, col = "chartreuse3")

points(4.85, embdoub.plot2$eff.correct[embdoub.plot2$data=="childes_parents"], 
       pch = 20, cex = 2, col = "cyan2")
segments(x0 = 4.85, x1 = 4.85,
         y0 = embdoub.plot2$eff.correct[embdoub.plot2$data=="childes_parents"] - 1.96*embdoub.plot2$se[embdoub.plot2$data=="childes_parents"],
         y1 = embdoub.plot2$eff.correct[embdoub.plot2$data=="childes_parents"] + 1.96*embdoub.plot2$se[embdoub.plot2$data=="childes_parents"], 
         lty = 1, lwd = 3, col = "cyan2")
points(4.9, embdoub.plot2$eff.correct[embdoub.plot2$data=="child_books"], 
       pch = 20, cex = 2, col = "cadetblue")
segments(x0 = 4.9, x1 = 4.9,
         y0 = embdoub.plot2$eff.correct[embdoub.plot2$data=="child_books"] - 1.96*embdoub.plot2$se[embdoub.plot2$data=="child_books"],
         y1 = embdoub.plot2$eff.correct[embdoub.plot2$data=="child_books"] + 1.96*embdoub.plot2$se[embdoub.plot2$data=="child_books"], 
         lty = 1, lwd = 3, col = "cadetblue")
points(4.95,
       embdoub.plot2$eff.correct[embdoub.plot2$data=="kids_tv_combined"], 
       pch = 20, cex = 2, col = "darkblue")
segments(x0 = 4.95, x1 = 4.95,
         y0 = embdoub.plot2$eff.correct[embdoub.plot2$data=="kids_tv_combined"] - 1.96*embdoub.plot2$se[embdoub.plot2$data=="kids_tv_combined"],
         y1 = embdoub.plot2$eff.correct[embdoub.plot2$data=="kids_tv_combined"] + 1.96*embdoub.plot2$se[embdoub.plot2$data=="kids_tv_combined"], 
         lty = 1, lwd = 3, col = "darkblue")

points(5.05, embdoub.plot2$eff.correct[embdoub.plot2$data=="adult_speech"], 
       pch = 20, cex = 2, col = "gold1")
segments(x0 = 5.05, x1 = 5.05,
         y0 = embdoub.plot2$eff.correct[embdoub.plot2$data=="adult_speech"] - 1.96*embdoub.plot2$se[embdoub.plot2$data=="adult_speech"],
         y1 = embdoub.plot2$eff.correct[embdoub.plot2$data=="adult_speech"] + 1.96*embdoub.plot2$se[embdoub.plot2$data=="adult_speech"], 
         lty = 1, lwd = 3, col = "gold1")
points(5.1, embdoub.plot2$eff.correct[embdoub.plot2$data=="gutenberg"], 
       pch = 20, cex = 2, col = "darkorange")
segments(x0 = 5.1, x1 = 5.1,
         y0 = embdoub.plot2$eff.correct[embdoub.plot2$data=="gutenberg"] - 1.96*embdoub.plot2$se[embdoub.plot2$data=="gutenberg"],
         y1 = embdoub.plot2$eff.correct[embdoub.plot2$data=="gutenberg"] + 1.96*embdoub.plot2$se[embdoub.plot2$data=="gutenberg"], 
         lty = 1, lwd = 3, col = "darkorange")
points(5.15, embdoub.plot2$eff.correct[embdoub.plot2$data=="simply_scripts"], 
       pch = 20, cex = 2, col = "plum2")
segments(x0 = 5.15, x1 = 5.15,
         y0 = embdoub.plot2$eff.correct[embdoub.plot2$data=="simply_scripts"] - 1.96*embdoub.plot2$se[embdoub.plot2$data=="simply_scripts"],
         y1 = embdoub.plot2$eff.correct[embdoub.plot2$data=="simply_scripts"] + 1.96*embdoub.plot2$se[embdoub.plot2$data=="simply_scripts"], 
         lty = 1, lwd = 3, col = "plum2")

legend("bottomleft", c("Child-produced speech", "Child-directed speech",
                       "Child-directed books", "Child-directed media",
                       "Adult-produced speech", "Adult-directed books", 
                       "Adult-directed media", "Meta Estimate"),
       col = c("chartreuse3", "cyan2", "cadetblue", "darkblue",
               "gold1", "darkorange", "plum2", "red"),
       pch = c(rep(20, 7), 17), 
       cex = 2, pt.cex = c(rep(3, 7), 2.5), bty = "n", ncol = 2)

par(op)
dev.off()


## SUPPLEMENTARY MATERIALS ******* ----

## (7) Supplementary materials: Study 1 category and attribute cohesion ----
cohsum <- embdata[embdata$categories=="instruments vs. weapons" | embdata$categories=="male vs. female" ,]
cohsum <- cohsum[grepl("vs.", cohsum$attributes)==TRUE,]
cohsum <- cohsum[c("dataset", "categories", "attributes", "coh_cat", "coh_att")]
colnames(cohsum) <- c("data", "cat", "att", "coh_cat", "coh_att")

cohsum[cohsum$data == "childes_children",]
cohsum[cohsum$data == "childes_parents",]
cohsum[cohsum$data == "child_books",]
cohsum[cohsum$data == "kids_tv_combined",]
cohsum[cohsum$data == "adult_speech",]
cohsum[cohsum$data == "gutenberg",]
cohsum[cohsum$data == "simply_scripts",]


## (8) Supplementary materials: Study 1 single difference scores ----
embsing <- embdata[embdata$categories=="female vs. male" | embdata$categories=="male vs. female" ,]
embsing <- embsing[grepl("vs.", embsing$attributes)==FALSE,]
embsing <- embsing[c("dataset", "categories", "attributes", "effsize", "se", "ptot")]
colnames(embsing) <- c("data", "cat", "att", "eff", "se", "p")

# now add on the instruments-weapons effect sizes
instweap.sing <- embdata[embdata$categories=="instruments vs. weapons" & grepl("vs.", embdata$attributes)==FALSE,]
instweap.sing <- instweap.sing[c("dataset", "categories", "attributes", "effsize", "se", "ptot")]
colnames(instweap.sing) <- c("data", "cat", "att", "eff", "se", "p")

embsing <- rbind(embsing, instweap.sing)

# No need to change direction of effects, all positive scores indicate association with male
table(embsing$att)
embsing$att <- factor(embsing$att, levels = c("bad", "work", "math", "science", 
                                              "good", "home", "reading","arts"))

## Create data frame for meta-analysis
table(embsing$data)
metadat.sing <- embsing[embsing$data=="childes_children" | embsing$data=="childes_parents" | embsing$data=="child_books" | embsing$data=="kids_tv_combined" | embsing$data=="adult_speech" | embsing$data=="gutenberg" | embsing$data=="simply_scripts",]

metadat.sing$data <- factor(metadat.sing$data)
table(metadat.sing$data)

# Add another factor for child-directed, child-produced, or adult-directed and produced
metadat.sing$datacat <- car::recode(metadat.sing$data, "'childes_parents' = 'childdir'; 'child_books' = 'childdir'; 
                          'kids_tv_combined' = 'childdir'; 'childes_children' = 'childpro';
                          'adult_speech' = 'adultpd'; 'gutenberg' = 'adultpd'; 'simply_scripts' = 'adultpd'")
table(metadat.sing$datacat)


# (2b) Perform meta-analyses for each attribute and category combination
# Meta-analysis for Instruments-Weapons Good
table(metadat.sing$cat)
metaeffsing.instweap.good <- metadat.sing$eff[grepl("instruments", metadat.sing$cat)==TRUE & grepl("good", metadat.sing$att)==TRUE]
metaeffsing.se.instweap.good <- metadat.sing$se[grepl("instruments", metadat.sing$cat)==TRUE & grepl("good", metadat.sing$att)==TRUE]
metasing.instweap.good <- metagen(TE = metaeffsing.instweap.good, seTE = metaeffsing.se.instweap.good,
                                  sm = "MD") # using mean effect method
summary(metasing.instweap.good)

# Meta-analysis for Instruments-Weapons Bad
metaeffsing.instweap.bad <- metadat.sing$eff[grepl("instruments", metadat.sing$cat)==TRUE & grepl("bad", metadat.sing$att)==TRUE]
metaeffsing.se.instweap.bad <- metadat.sing$se[grepl("instruments", metadat.sing$cat)==TRUE & grepl("bad", metadat.sing$att)==TRUE]
metasing.instweap.bad <- metagen(TE = metaeffsing.instweap.bad, seTE = metaeffsing.se.instweap.bad,
                                 sm = "MD") # using mean effect method
summary(metasing.instweap.bad)

# Meta-analysis for Male-Female  Good
metaeffsing.mf.good <- metadat.sing$eff[grepl("male", metadat.sing$cat)==TRUE & grepl("good", metadat.sing$att)==TRUE]
metaeffsing.se.mf.good <- metadat.sing$se[grepl("male", metadat.sing$cat)==TRUE & grepl("good", metadat.sing$att)==TRUE]
metasing.mf.good <- metagen(TE = metaeffsing.mf.good, seTE = metaeffsing.se.mf.good,
                            sm = "MD") # using mean effect method
summary(metasing.mf.good)

# Meta-analysis for Male-Female Bad
metaeffsing.mf.bad <- metadat.sing$eff[grepl("male", metadat.sing$cat)==TRUE & grepl("bad", metadat.sing$att)==TRUE]
metaeffsing.se.mf.bad <- metadat.sing$se[grepl("male", metadat.sing$cat)==TRUE & grepl("bad", metadat.sing$att)==TRUE]
metasing.mf.bad <- metagen(TE = metaeffsing.mf.bad, seTE = metaeffsing.se.mf.bad,
                           sm = "MD") # using mean effect method
summary(metasing.mf.bad)

# Meta-analysis for Male-Female Work
metaeffsing.mf.work <- metadat.sing$eff[grepl("male", metadat.sing$cat)==TRUE & grepl("work", metadat.sing$att)==TRUE]
metaeffsing.se.mf.work <- metadat.sing$se[grepl("male", metadat.sing$cat)==TRUE & grepl("work", metadat.sing$att)==TRUE]
metasing.mf.work <- metagen(TE = metaeffsing.mf.work, seTE = metaeffsing.se.mf.work,
                            sm = "MD") # using mean effect method
summary(metasing.mf.work)

# Meta-analysis for Male-Female Home
metaeffsing.mf.home <- metadat.sing$eff[grepl("male", metadat.sing$cat)==TRUE & grepl("home", metadat.sing$att)==TRUE]
metaeffsing.se.mf.home <- metadat.sing$se[grepl("male", metadat.sing$cat)==TRUE & grepl("home", metadat.sing$att)==TRUE]
metasing.mf.home <- metagen(TE = metaeffsing.mf.home, seTE = metaeffsing.se.mf.home,
                            sm = "MD") # using mean effect method
summary(metasing.mf.home)

# Meta-analysis for Male-Female Science
metaeffsing.mf.science <- metadat.sing$eff[grepl("male", metadat.sing$cat)==TRUE & grepl("science", metadat.sing$att)==TRUE]
metaeffsing.se.mf.science <- metadat.sing$se[grepl("male", metadat.sing$cat)==TRUE & grepl("science", metadat.sing$att)==TRUE]
metasing.mf.science <- metagen(TE = metaeffsing.mf.science, seTE = metaeffsing.se.mf.science,
                               sm = "MD") # using mean effect method
summary(metasing.mf.science)

# Meta-analysis for Male-Female Arts
metaeffsing.mf.arts <- metadat.sing$eff[grepl("male", metadat.sing$cat)==TRUE & grepl("arts", metadat.sing$att)==TRUE]
metaeffsing.se.mf.arts <- metadat.sing$se[grepl("male", metadat.sing$cat)==TRUE & grepl("arts", metadat.sing$att)==TRUE]
metasing.mf.arts <- metagen(TE = metaeffsing.mf.arts, seTE = metaeffsing.se.mf.arts,
                            sm = "MD") # using mean effect method
summary(metasing.mf.arts)

# Meta-analysis for Male-Female Math
metaeffsing.mf.math <- metadat.sing$eff[grepl("male", metadat.sing$cat)==TRUE & grepl("math", metadat.sing$att)==TRUE]
metaeffsing.se.mf.math <- metadat.sing$se[grepl("male", metadat.sing$cat)==TRUE & grepl("math", metadat.sing$att)==TRUE]
metasing.mf.math <- metagen(TE = metaeffsing.mf.math, seTE = metaeffsing.se.mf.math,
                            sm = "MD") # using mean effect method
summary(metasing.mf.math)

# Meta-analysis for Male-Female Reading
metaeffsing.mf.reading <- metadat.sing$eff[grepl("male", metadat.sing$cat)==TRUE & grepl("reading", metadat.sing$att)==TRUE]
metaeffsing.se.mf.reading <- metadat.sing$se[grepl("male", metadat.sing$cat)==TRUE & grepl("reading", metadat.sing$att)==TRUE]
metasing.mf.reading <- metagen(TE = metaeffsing.mf.reading, seTE = metaeffsing.se.mf.reading,
                               sm = "MD") # using mean effect method
summary(metasing.mf.reading)


# (2b) Combine meta data (for fixed) 
metadatsum.sing <- as.data.frame(matrix(nrow = 8, ncol = 6))
colnames(metadatsum.sing) <- c("metamean", "metase", "meta95low", "meta95high", "metap", "attcat")
metadatsum.sing$metamean <- c(metasing.mf.bad$TE.fixed, metasing.mf.good$TE.fixed, 
                              metasing.mf.work$TE.fixed, metasing.mf.home$TE.fixed, 
                              metasing.mf.science$TE.fixed, metasing.mf.arts$TE.fixed,
                              metasing.mf.math$TE.fixed, metasing.mf.reading$TE.fixed)
metadatsum.sing$metase <-  c(metasing.mf.bad$seTE.fixed, metasing.mf.good$seTE.fixed, 
                             metasing.mf.work$seTE.fixed, metasing.mf.home$seTE.fixed, 
                             metasing.mf.science$seTE.fixed, metasing.mf.arts$seTE.fixed,
                             metasing.mf.math$seTE.fixed, metasing.mf.reading$seTE.fixed)
metadatsum.sing$meta95low <-  c(metasing.mf.bad$lower.fixed, metasing.mf.good$lower.fixed, 
                                metasing.mf.work$lower.fixed, metasing.mf.home$lower.fixed, 
                                metasing.mf.science$lower.fixed, metasing.mf.arts$lower.fixed,
                                metasing.mf.math$lower.fixed, metasing.mf.reading$lower.fixed)
metadatsum.sing$meta95high <-  c(metasing.mf.bad$upper.fixed, metasing.mf.good$upper.fixed, 
                                 metasing.mf.work$upper.fixed, metasing.mf.home$upper.fixed, 
                                 metasing.mf.science$upper.fixed, metasing.mf.arts$upper.fixed,
                                 metasing.mf.math$upper.fixed, metasing.mf.reading$upper.fixed)
metadatsum.sing$metap <-  c(metasing.mf.bad$pval.fixed, metasing.mf.good$pval.fixed, 
                            metasing.mf.work$pval.fixed, metasing.mf.home$pval.fixed, 
                            metasing.mf.science$pval.fixed, metasing.mf.arts$pval.fixed,
                            metasing.mf.math$pval.fixed, metasing.mf.reading$pval.fixed)
metadatsum.sing$attcat <-c("mfb", "mfg", "mfw", "mfh", "mfs", "mfa", "mfm", "mfr")
# Meta-analysis summary
metadatsum.sing
table(metadatsum.sing$attcat)
metadatsum.sing$attcat <- factor(metadatsum.sing$attcat,
                                 levels = c("mfg", "mfb", "mfh", "mfw", "mfa", "mfs", "mfr", "mfm"))
metadatsum.sing <- arrange(metadatsum.sing, attcat)
metadatsum.sing

## Plotting results
table(metadat.sing$cat)
metadat.singplot <- metadat.sing[metadat.sing$cat != "instruments vs. weapons",] # Remove instruments/weapons for main plot
table(metadat.singplot$att)
metadat.singplot$att2 <- factor(metadat.singplot$att, 
                            levels = c("good", "bad", "home", "work", 
                                       "arts", "science", "reading", "math"))
metadat.singplot <- arrange(metadat.singplot, att2)
head(metadat.singplot)

## Plotting altogether
pdf(file = "sm.singcatall.pdf", width = 20, height = 10)
op <- par(mar = c(5,6,2,2))
plot(1, type="n", xlab="", ylab="", 
     xlim=c(0.5, 8.5), ylim=c(-2.25, 2.25), axes = FALSE)
box()
mtext("Effect Size", side = 2, line = 3, cex = 2)
axis(1, at = c(1:8), labels = c("Male-Female\nGood", "Male-Female\nBad", 
                                "Male-Female\nHome", "Male-Female\nWork", 
                                "Male-Female\nArts", "Male-Female\nScience", 
                                "Male-Female\nReading", "Male-Female\nMath"),
     cex.axis = 1.4, padj = 1)
axis(2, at = seq(-2,2, by = 0.5), labels = seq(-2,2, by = 0.5), las = 2, cex.axis = 1.5)
abline(h = 0, lty = 2)

points(seq(1, 8, by = 1),
       metadatsum.sing$metamean, 
       pch = 17, cex = 3, col = "red")
segments(x0 = seq(1, 8, by = 1), x1 = seq(1, 8, by = 1),
         y0 = metadatsum.sing$meta95low,
         y1 = metadatsum.sing$meta95high, 
         lty = 1, lwd = 3, col = "red")

points(seq(0.8, 7.8, by = 1),
       metadat.singplot$eff[metadat.singplot$data=="childes_children"], 
       pch = 18, cex = 3, col = "chartreuse3")
segments(x0 = seq(0.8, 7.8, by = 1), x1 = seq(0.8, 7.8, by = 1),
         y0 = metadat.singplot$eff[metadat.singplot$data=="childes_children"] - 1.96*metadat.singplot$se[metadat.singplot$data=="childes_children"],
         y1 = metadat.singplot$eff[metadat.singplot$data=="childes_children"] + 1.96*metadat.singplot$se[metadat.singplot$data=="childes_children"], 
         lty = 1, lwd = 3, col = "chartreuse3")

points(seq(0.85, 7.85, by = 1),
       metadat.singplot$eff[metadat.singplot$data=="childes_parents"], 
       pch = 19, cex = 2, col = "cyan2")
segments(x0 = seq(0.85, 7.85, by = 1), x1 = seq(0.85, 7.85, by = 1),
         y0 = metadat.singplot$eff[metadat.singplot$data=="childes_parents"] - 1.96*metadat.singplot$se[metadat.singplot$data=="childes_parents"],
         y1 = metadat.singplot$eff[metadat.singplot$data=="childes_parents"] + 1.96*metadat.singplot$se[metadat.singplot$data=="childes_parents"], 
         lty = 1, lwd = 3, col = "cyan2")
points(seq(0.9, 7.9, by = 1),
       metadat.singplot$eff[metadat.singplot$data=="child_books"], 
       pch = 19, cex = 2, col = "cadetblue")
segments(x0 = seq(0.9, 7.9, by = 1), x1 = seq(0.9, 7.9, by = 1),
         y0 = metadat.singplot$eff[metadat.singplot$data=="child_books"] - 1.96*metadat.singplot$se[metadat.singplot$data=="child_books"],
         y1 = metadat.singplot$eff[metadat.singplot$data=="child_books"] + 1.96*metadat.singplot$se[metadat.singplot$data=="child_books"], 
         lty = 1, lwd = 3, col = "cadetblue")
points(seq(0.95, 7.95, by = 1),
       metadat.singplot$eff[metadat.singplot$data=="kids_tv_combined"], 
       pch = 19, cex = 2, col = "darkblue")
segments(x0 = seq(0.95, 7.95, by = 1), x1 = seq(0.95, 7.95, by = 1),
         y0 = metadat.singplot$eff[metadat.singplot$data=="kids_tv_combined"] - 1.96*metadat.singplot$se[metadat.singplot$data=="kids_tv_combined"],
         y1 = metadat.singplot$eff[metadat.singplot$data=="kids_tv_combined"] + 1.96*metadat.singplot$se[metadat.singplot$data=="kids_tv_combined"], 
         lty = 1, lwd = 3, col = "darkblue")

points(seq(1.05, 8.05, by = 1),
       metadat.singplot$eff[metadat.singplot$data=="adult_speech"], 
       pch = 15, cex = 2, col = "gold1")
segments(x0 = seq(1.05, 8.05, by = 1), x1 = seq(1.05, 8.05, by = 1),
         y0 = metadat.singplot$eff[metadat.singplot$data=="adult_speech"] - 1.96*metadat.singplot$se[metadat.singplot$data=="adult_speech"],
         y1 = metadat.singplot$eff[metadat.singplot$data=="adult_speech"] + 1.96*metadat.singplot$se[metadat.singplot$data=="adult_speech"], 
         lty = 1, lwd = 3, col = "gold1")
points(seq(1.1, 8.1, by = 1),
       metadat.singplot$eff[metadat.singplot$data=="gutenberg"], 
       pch = 15, cex = 2, col = "darkorange")
segments(x0 = seq(1.1, 8.1, by = 1), x1 = seq(1.1, 8.1, by = 1),
         y0 = metadat.singplot$eff[metadat.singplot$data=="gutenberg"] - 1.96*metadat.singplot$se[metadat.singplot$data=="gutenberg"],
         y1 = metadat.singplot$eff[metadat.singplot$data=="gutenberg"] + 1.96*metadat.singplot$se[metadat.singplot$data=="gutenberg"], 
         lty = 1, lwd = 3, col = "darkorange")
points(seq(1.15, 8.15, by = 1),
       metadat.singplot$eff[metadat.singplot$data=="simply_scripts"], 
       pch = 15, cex = 2, col = "plum2")
segments(x0 = seq(1.15, 8.15, by = 1), x1 = seq(1.15, 8.15, by = 1),
         y0 = metadat.singplot$eff[metadat.singplot$data=="simply_scripts"] - 1.96*metadat.singplot$se[metadat.singplot$data=="simply_scripts"],
         y1 = metadat.singplot$eff[metadat.singplot$data=="simply_scripts"] + 1.96*metadat.singplot$se[metadat.singplot$data=="simply_scripts"], 
         lty = 1, lwd = 3, col = "plum2")


legend("bottomleft", c("Child-produced speech", "Child-directed speech",
                       "Child-directed books", "Child-directed media",
                       "Adult-produced speech", "Adult-directed books", 
                       "Adult-directed media", "Meta Estimate"),
       col = c("chartreuse3", "cyan2", "cadetblue", "darkblue",
               "gold1", "darkorange", "plum2", "red"),
       pch = c(18, rep(19, 3), rep(15, 3), 17), 
       cex = 2, pt.cex = c(3, rep(2.5, 6), 3), bty = "n", ncol = 2)

par(op)
dev.off()

## Just meta-analytic estimates
pdf(file = "sm.singcat.pdf", width = 20, height = 10)
op <- par(mar = c(5,6,2,2))
plot(1, type="n", xlab="", ylab="", 
     xlim=c(0.5, 8.5), ylim=c(-2.25, 2.25), axes = FALSE)
box()
mtext("Effect Size", side = 2, line = 3, cex = 2)
axis(1, at = c(1:8), labels = c("Male-Female\nGood", "Male-Female\nBad", 
                                "Male-Female\nHome", "Male-Female\nWork", 
                               "Male-Female\nArts", "Male-Female\nScience", 
                                "Male-Female\nReading", "Male-Female\nMath"),
     cex.axis = 1.4, padj = 1)
axis(2, at = seq(-2,2, by = 0.5), labels = seq(-2,2, by = 0.5), las = 2, cex.axis = 1.5)
abline(h = 0, lty = 2)

points(seq(1, 8, by = 1),
       metadatsum.sing$metamean, 
       pch = 17, cex = 3, col = "red")
segments(x0 = seq(1, 8, by = 1), x1 = seq(1, 8, by = 1),
         y0 = metadatsum.sing$meta95low,
         y1 = metadatsum.sing$meta95high, 
         lty = 1, lwd = 3, col = "red")

par(op)
dev.off()



## (9) Supplementary materials: Study 1 robustness tests with new stimuli for gender categories ----
## With additional stimuli list (42 words) ----
table(embdata$categories)
embadd <- embdata[embdata$categories=="malemax vs. femalemax" ,]
embadd <- embadd[grepl("vs.", embadd$attributes)==TRUE,]
embadd <- embadd[grepl("max", embadd$attributes)==FALSE,]
embadd <- embadd[c("dataset", "categories", "attributes", "effsize", "se", "ptot")]
colnames(embadd) <- c("data", "cat", "att", "eff", "se", "p")

# To get all the expected effects in the positive direction...
# (a) Switch around the order of the labels for the double-difference scores
# The first attribute should be the one stereotypically-associated with males
# Should be "bad vs. good," "work vs. home," "math vs. reading," "science vs. arts" (already in the correct direction)
# (b) Then multiply the first four double-differences by -1
table(embadd$att)
embadd$att <- factor(embadd$att, labels = c("bad vs. good", "work vs. home", "math vs. reading", "science vs. arts"))
table(embadd$cat)
embadd$cat <- factor(embadd$cat, labels = c("malemax vs. femalemax"))


# And correct the d-scores to align with positive = expected order.
# Multiply by -1 for all double-differences except instrumen/weapons and arts/science
embadd$eff.correct <- ifelse(grepl("science", embadd$att)==TRUE | grepl("weapons", embadd$cat)==TRUE, 
                              embadd$eff, embadd$eff*-1)
head(embadd)


# (9a) Meta analysis of all effect sizes
# Only keeping primary data (7 data sources)
table(embadd$data)
metadat.add <- embadd[embadd$data=="childes_children" | embadd$data=="childes_parents" | embadd$data=="child_books" | embadd$data=="kids_tv_combined" | embadd$data=="adult_speech" | embadd$data=="gutenberg" | embadd$data=="simply_scripts",]

metadat.add$data <- factor(metadat.add$data)
table(metadat.add$data)

# Add another factor for child-directed, child-produced, or adult-directed and produced
metadat.add$datacat <- car::recode(metadat.add$data, "'childes_parents' = 'childdir'; 'child_books' = 'childdir'; 
                          'kids_tv_combined' = 'childdir'; 'childes_children' = 'childpro';
                          'adult_speech' = 'adultpd'; 'gutenberg' = 'adultpd'; 'simply_scripts' = 'adultpd'")
table(metadat.add$datacat)

# Perform meta-analyses for each attribute and category combination
# Meta-analysis for Male-Female Good-Bad
# vector of treatment effects
metaeff.add.mfgb <- metadat.add$eff.correct[grepl("male", metadat.add$cat)==TRUE & grepl("good", metadat.add$att)==TRUE]
metaeff.add.se.mfgb <- metadat.add$se[grepl("male", metadat.add$cat)==TRUE & grepl("good", metadat.add$att)==TRUE]
meta.add.mfgb <- metagen(TE = metaeff.add.mfgb, seTE = metaeff.add.se.mfgb, sm = "MD") # using mean effect method
summary(meta.add.mfgb)

# Meta-analysis for Male-Female Work-Home
# vector of treatment effects
metaeff.add.mfwh <- metadat.add$eff.correct[grepl("male", metadat.add$cat)==TRUE & grepl("home", metadat.add$att)==TRUE]
metaeff.add.se.mfwh <- metadat.add$se[grepl("male", metadat.add$cat)==TRUE & grepl("home", metadat.add$att)==TRUE]
meta.add.mfwh <- metagen(TE = metaeff.add.mfwh, seTE = metaeff.add.se.mfwh, sm = "MD") # using mean effect method
summary(meta.add.mfwh)

# Meta-analysis for Male-Female Science-Arts
# vector of treatment effects
metaeff.add.mfsa <- metadat.add$eff.correct[grepl("male", metadat.add$cat)==TRUE & grepl("science", metadat.add$att)==TRUE]
metaeff.add.se.mfsa <- metadat.add$se[grepl("male", metadat.add$cat)==TRUE & grepl("science", metadat.add$att)==TRUE]
meta.add.mfsa <- metagen(TE = metaeff.add.mfsa, seTE = metaeff.add.se.mfsa, sm = "MD") # using mean effect method
summary(meta.add.mfsa)

# Meta-analysis for Male-Female Math-Reading
# vector of treatment effects
metaeff.add.mfmr <- metadat.add$eff.correct[grepl("male", metadat.add$cat)==TRUE & grepl("math", metadat.add$att)==TRUE]
metaeff.add.se.mfmr <- metadat.add$se[grepl("male", metadat.add$cat)==TRUE & grepl("math", metadat.add$att)==TRUE]
meta.add.mfmr <- metagen(TE = metaeff.add.mfmr, seTE = metaeff.add.se.mfmr, sm = "MD") # using mean effect method
summary(meta.add.mfmr)


# Combine meta data (for fixed) 
metadat.addsum <- as.data.frame(matrix(nrow = 5, ncol = 6))
colnames(metadat.addsum) <- c("metamean", "metase", "meta95low", "meta95high", "metap", "attcat")
metadat.addsum$metamean <- c(meta.add.mfgb$TE.fixed, meta.add.mfwh$TE.fixed, meta.add.mfsa$TE.fixed, meta.add.mfmr$TE.fixed, meta.instweap$TE.fixed)
metadat.addsum$metase <- c(meta.add.mfgb$seTE.fixed, meta.add.mfwh$seTE.fixed, meta.add.mfsa$seTE.fixed, meta.add.mfmr$seTE.fixed, meta.instweap$seTE.fixed)
metadat.addsum$meta95low <- c(meta.add.mfgb$lower.fixed, meta.add.mfwh$lower.fixed, meta.add.mfsa$lower.fixed, 
                          meta.add.mfmr$lower.fixed, meta.instweap$lower.fixed)
metadat.addsum$meta95high <- c(meta.add.mfgb$upper.fixed, meta.add.mfwh$upper.fixed, meta.add.mfsa$upper.fixed,
                           meta.add.mfmr$upper.fixed, meta.instweap$upper.fixed)
metadat.addsum$metap <- c(meta.add.mfgb$pval.fixed, meta.add.mfwh$pval.fixed, meta.add.mfsa$pval.fixed, meta.add.mfmr$pval.fixed, meta.instweap$pval.fixed)
metadat.addsum$attcat <-c("mfgb", "mfwh", "mfsa", "mfmr", "iwgb")
# Overall meta-analysis summary
metadat.addsum

## Meta-analysis across child-produced, child-directed, and adult-produced/directed corpora subsets
# Meta-analysis in child-directed corpora
table(metadat.add$datacat)
metadat.add.childdir <- metadat.add[metadat.add$datacat=="childdir",]

# For Male-Female Good-Bad 
metaeff.add.cd.mfgb <- metadat.add.childdir$eff.correct[grepl("male", metadat.add.childdir$cat)==TRUE & grepl("good", metadat.add.childdir$att)==TRUE]
metaeff.add.cd.se.mfgb <- metadat.add.childdir$se[grepl("male", metadat.add.childdir$cat)==TRUE & grepl("good", metadat.add.childdir$att)==TRUE]
meta.addcd.mfgb <- metagen(TE = metaeff.add.cd.mfgb, seTE = metaeff.add.cd.se.mfgb, sm = "MD") # using mean effect method
summary(meta.addcd.mfgb)
meta.addcd.mfgb$seTE.fixed

# For Work-Home
metaeff.add.cd.mfwh <- metadat.add.childdir$eff.correct[grepl("male", metadat.add.childdir$cat)==TRUE & grepl("work", metadat.add.childdir$att)==TRUE]
metaeff.add.cd.se.mfwh <- metadat.add.childdir$se[grepl("male", metadat.add.childdir$cat)==TRUE & grepl("work", metadat.add.childdir$att)==TRUE]
meta.addcd.mfwh <- metagen(TE = metaeff.add.cd.mfwh, seTE = metaeff.add.cd.se.mfwh, sm = "MD") # using mean effect method
summary(meta.addcd.mfwh)
meta.addcd.mfwh$seTE.fixed

# For Science-Arts
metaeff.add.cd.mfsa <- metadat.add.childdir$eff.correct[grepl("male", metadat.add.childdir$cat)==TRUE & grepl("arts", metadat.add.childdir$att)==TRUE]
metaeff.add.cd.se.mfsa <- metadat.add.childdir$se[grepl("male", metadat.add.childdir$cat)==TRUE & grepl("arts", metadat.add.childdir$att)==TRUE]
meta.addcd.mfsa <- metagen(TE = metaeff.add.cd.mfsa, seTE = metaeff.add.cd.se.mfsa, sm = "MD") # using mean effect method
summary(meta.addcd.mfsa)
meta.addcd.mfsa$seTE.fixed

# For Reading-Math
metaeff.add.cd.mfrm <- metadat.add.childdir$eff.correct[grepl("male", metadat.add.childdir$cat)==TRUE & grepl("reading", metadat.add.childdir$att)==TRUE]
metaeff.add.cd.se.mfrm <- metadat.add.childdir$se[grepl("male", metadat.add.childdir$cat)==TRUE & grepl("reading", metadat.add.childdir$att)==TRUE]
meta.addcd.mfrm <- metagen(TE = metaeff.add.cd.mfrm, seTE = metaeff.add.cd.se.mfrm, sm = "MD") # using mean effect method
summary(meta.addcd.mfrm)
meta.addcd.mfrm$seTE.fixed


# Meta-analysis in adult-directed corpora
table(metadat.add$datacat)
metadat.add.adultdp <- metadat.add[metadat.add$datacat=="adultpd",]

# For Male-Female Good-Bad 
metaeff.add.adp.mfgb <- metadat.add.adultdp$eff.correct[grepl("male", metadat.add.adultdp$cat)==TRUE & grepl("good", metadat.add.adultdp$att)==TRUE]
metaeff.add.adp.se.mfgb <- metadat.add.adultdp$se[grepl("male", metadat.add.adultdp$cat)==TRUE & grepl("good", metadat.add.adultdp$att)==TRUE]
meta.addadp.mfgb <- metagen(TE = metaeff.add.adp.mfgb, seTE = metaeff.add.adp.se.mfgb, sm = "MD") # using mean effect method
summary(meta.addadp.mfgb)
meta.addadp.mfgb$seTE.fixed

# For Work-Home
metaeff.add.adp.mfwh <- metadat.add.adultdp$eff.correct[grepl("male", metadat.add.adultdp$cat)==TRUE & grepl("work", metadat.add.adultdp$att)==TRUE]
metaeff.add.adp.se.mfwh <- metadat.add.adultdp$se[grepl("male", metadat.add.adultdp$cat)==TRUE & grepl("work", metadat.add.adultdp$att)==TRUE]
meta.addadp.mfwh <- metagen(TE = metaeff.add.adp.mfwh, seTE = metaeff.add.adp.se.mfwh, sm = "MD") # using mean effect method
summary(meta.addadp.mfwh)
meta.addadp.mfwh$seTE.fixed

# For Science-Arts
metaeff.add.adp.mfsa <- metadat.add.adultdp$eff.correct[grepl("male", metadat.add.adultdp$cat)==TRUE & grepl("arts", metadat.add.adultdp$att)==TRUE]
metaeff.add.adp.se.mfsa <- metadat.add.adultdp$se[grepl("male", metadat.add.adultdp$cat)==TRUE & grepl("arts", metadat.add.adultdp$att)==TRUE]
meta.addadp.mfsa <- metagen(TE = metaeff.add.adp.mfsa, seTE = metaeff.add.adp.se.mfsa, sm = "MD") # using mean effect method
summary(meta.addadp.mfsa)
meta.addadp.mfsa$seTE.fixed

# For Reading-Math
metaeff.add.adp.mfrm <- metadat.add.adultdp$eff.correct[grepl("male", metadat.add.adultdp$cat)==TRUE & grepl("reading", metadat.add.adultdp$att)==TRUE]
metaeff.add.adp.se.mfrm <- metadat.add.adultdp$se[grepl("male", metadat.add.adultdp$cat)==TRUE & grepl("reading", metadat.add.adultdp$att)==TRUE]
meta.addadp.mfrm <- metagen(TE = metaeff.add.adp.mfrm, seTE = metaeff.add.adp.se.mfrm, sm = "MD") # using mean effect method
summary(meta.addadp.mfrm)
meta.addadp.mfrm$seTE.fixed

# Note: Results for each corpus/stereotype combination meta-analysis are reported in Table 1 in the main text.

## Meta-analysis across all stereotypes and across child-produced, child-directed, and adult-produced/directed corpora subsets
# Meta-analysis in all stereotypes and all corpora
metaeff.add.all <- metadat.add$eff.correct[metadat.add$cat != "weapons vs. instruments"]
metaeff.add.se.all <- metadat.add$se[metadat.add$cat != "weapons vs. instruments"]
meta.addall <- metagen(TE = metaeff.add.all, seTE = metaeff.add.se.all, sm = "MD") # using mean effect method
summary(meta.addall)
meta.addall$seTE.fixed

# Across child-produced
metaeff.add.chprod <- metadat.add$eff.correct[metadat.add$data=="childes_children" & metadat.add$cat != "weapons vs. instruments"]
metaeff.add.se.chprod <- metadat.add$se[metadat.add$data=="childes_children" & metadat.add$cat != "weapons vs. instruments"]
meta.addchprod <- metagen(TE = metaeff.add.chprod, seTE = metaeff.add.se.chprod, sm = "MD") # using mean effect method
summary(meta.addchprod)
meta.addchprod$seTE.fixed
# Across child-directed
metadat.add.childdir <- metadat.add[metadat.add$datacat=="childdir",]
metaeff.add.chdir <- metadat.add.childdir$eff.correct[metadat.add.childdir$cat != "weapons vs. instruments"]
metaeff.add.se.chdir <-metadat.add.childdir$se[metadat.add.childdir$cat != "weapons vs. instruments"]
meta.addchdir <- metagen(TE = metaeff.add.chdir, seTE = metaeff.add.se.chdir, sm = "MD") # using mean effect method
summary(meta.addchdir)
meta.addchdir$seTE.fixed
# Across adult-directed
metadat.add.adultpd <- metadat.add[metadat.add$datacat=="adultpd",]
metaeff.add.adultpd <- metadat.add.adultpd$eff.correct[metadat.add.adultpd$cat != "weapons vs. instruments"]
metaeff.add.se.adultpd <-metadat.add.adultpd$se[metadat.add.adultpd$cat != "weapons vs. instruments"]
meta.addadultpd <- metagen(TE = metaeff.add.adultpd, seTE = metaeff.add.se.adultpd, sm = "MD") # using mean effect method
summary(meta.addadultpd)
meta.addadultpd$seTE.fixed


# Child-directed speech
metaeff.add.chdirspeech <- metadat.add$eff.correct[metadat.add$data=="childes_parents" & metadat.add$cat != "weapons vs. instruments"]
metaeff.add.se.chdirspeech <- metadat.add$se[metadat.add$data=="childes_parents" & metadat.add$cat != "weapons vs. instruments"]
meta.addchdirspeech <- metagen(TE = metaeff.add.chdirspeech, seTE = metaeff.add.se.chdirspeech, sm = "MD") # using mean effect method
summary(meta.addchdirspeech)
meta.addchdirspeech$seTE.fixed
# Child-directed books
metaeff.add.chbooks <- metadat.add$eff.correct[metadat.add$data=="child_books" & metadat.add$cat != "weapons vs. instruments"]
metaeff.add.se.chbooks <- metadat.add$se[metadat.add$data=="child_books" & metadat.add$cat != "weapons vs. instruments"]
meta.addchbooks <- metagen(TE = metaeff.add.chbooks, seTE = metaeff.add.se.chbooks, sm = "MD") # using mean effect method
summary(meta.addchbooks)
meta.addchbooks$seTE.fixed
# Child-directed TV
metaeff.add.chtv <- metadat.add$eff.correct[metadat.add$data=="kids_tv_combined" & metadat.add$cat != "weapons vs. instruments"]
metaeff.add.se.chtv <- metadat.add$se[metadat.add$data=="kids_tv_combined" & metadat.add$cat != "weapons vs. instruments"]
meta.addchtv <- metagen(TE = metaeff.add.chtv, seTE = metaeff.add.se.chtv, sm = "MD") # using mean effect method
summary(meta.addchtv)
meta.addchtv$seTE.fixed
# Adult speech
metaeff.add.adspeech <- metadat.add$eff.correct[metadat.add$data=="adult_speech" & metadat.add$cat != "weapons vs. instruments"]
metaeff.add.se.adspeech <- metadat.add$se[metadat.add$data=="adult_speech" & metadat.add$cat != "weapons vs. instruments"]
meta.addadspeech <- metagen(TE = metaeff.add.adspeech, seTE = metaeff.add.se.adspeech, sm = "MD") # using mean effect method
summary(meta.addadspeech)
meta.addadspeech$seTE.fixed
# Adult books
metaeff.add.adbooks <- metadat.add$eff.correct[metadat.add$data=="gutenberg" & metadat.add$cat != "weapons vs. instruments"]
metaeff.add.se.adbooks <- metadat.add$se[metadat.add$data=="gutenberg" & metadat.add$cat != "weapons vs. instruments"]
meta.addadbooks <- metagen(TE = metaeff.add.adbooks, seTE = metaeff.add.se.adbooks, sm = "MD") # using mean effect method
summary(meta.addadbooks)
meta.addadbooks$seTE.fixed
# Adult TV
metaeff.add.adtv <- metadat.add$eff.correct[metadat.add$data=="simply_scripts" & metadat.add$cat != "weapons vs. instruments"]
metaeff.add.se.adtv <- metadat.add$se[metadat.add$data=="simply_scripts" & metadat.add$cat != "weapons vs. instruments"]
meta.addadtv <- metagen(TE = metaeff.add.adtv, seTE = metaeff.add.se.adtv, sm = "MD") # using mean effect method
summary(meta.addadtv)
meta.addadtv$seTE.fixed

## To get a print-out of the remaining results, pull them out by corpus
metadat.add[metadat.add$data=="childes_children",c("att", "eff.correct", "se", "p")]
metadat.add[metadat.add$data=="childes_parents",c("att", "eff.correct", "se", "p")]
metadat.add[metadat.add$data=="child_books",c("att", "eff.correct", "se", "p")]
metadat.add[metadat.add$data=="kids_tv_combined",c("att", "eff.correct", "se", "p")]
metadat.add[metadat.add$data=="adult_speech",c("att", "eff.correct", "se", "p")]
metadat.add[metadat.add$data=="gutenberg",c("att", "eff.correct", "se", "p")]
metadat.add[metadat.add$data=="simply_scripts",c("att", "eff.correct", "se", "p")]


## Plotting ----
metadat.addsum2 <- metadat.addsum[1:4,]
metadat.addsum2 # mfgb, mfwh, mfsa, mfmr
metadat.add.plot <- metadat.add[metadat.add$cat != "weapons vs. instruments",] # Remove instruments/weapons for main plot
metadat.add.plot$att2 <- factor(metadat.add.plot$att, 
                                levels = c("bad vs. good", "work vs. home", "science vs. arts", "math vs. reading"))
metadat.add.plot <- arrange(metadat.add.plot, att2)

pdf(file = "presentplot1.add.pdf", width = 12, height = 10)
op <- par(mar = c(5,6,2,2))
plot(1, type="n", xlab="", ylab="", 
     xlim=c(0.5, 4.5), ylim=c(-2.25, 2.25), axes = FALSE)
box()
mtext("Effect Size", side = 2, line = 3, cex = 2)
axis(1, at = c(1:4), labels = c("Male-Bad\nFemale-Good", "Male-Work\nFemale-Home", 
                                "Male-Science\nFemale-Arts", "Male-Math\nFemale-Reading"),
     cex.axis = 1.4, padj = 1)
axis(2, at = seq(-2,2, by = 0.5), labels = seq(-2,2, by = 0.5), las = 2, cex.axis = 1.5)
abline(h = 0, lty = 2)

points(seq(1, 4, by = 1),
       metadat.addsum2$metamean, 
       pch = 17, cex = 3, col = "red")
segments(x0 = seq(1, 4, by = 1), x1 = seq(1, 4, by = 1),
         y0 = metadat.addsum2$meta95low,
         y1 = metadat.addsum2$meta95high, 
         lty = 1, lwd = 3, col = "red")

legend("bottomleft", c("Child-produced speech", "Child-directed speech",
                       "Child-directed books", "Child-directed media",
                       "Adult-produced speech", "Adult-directed books", 
                       "Adult-directed media", "Meta Estimate"),
       col = c("chartreuse3", "cyan2", "cadetblue", "darkblue",
               "gold1", "darkorange", "plum2", "red"),
       pch = c(rep(20, 7), 17), 
       cex = 2, pt.cex = c(rep(3, 7), 2.5), bty = "n", ncol = 2)

par(op)
dev.off()


## + Individual corpora data
pdf(file = "presentplotall.add.pdf", width = 12, height = 10)
op <- par(mar = c(5,6,2,2))
plot(1, type="n", xlab="", ylab="", 
     xlim=c(0.5, 4.5), ylim=c(-2.25, 2.25), axes = FALSE)
box()
mtext("Effect Size", side = 2, line = 3, cex = 2)
axis(1, at = c(1:4), labels = c("Male-Bad\nFemale-Good", "Male-Work\nFemale-Home", 
                                "Male-Science\nFemale-Arts", "Male-Math\nFemale-Reading"),
     cex.axis = 1.4, padj = 1)
axis(2, at = seq(-2,2, by = 0.5), labels = seq(-2,2, by = 0.5), las = 2, cex.axis = 1.5)
abline(h = 0, lty = 2)

points(seq(1, 4, by = 1),
       metadat.addsum2$metamean, 
       pch = 17, cex = 3, col = "red")
segments(x0 = seq(1, 4, by = 1), x1 = seq(1, 4, by = 1),
         y0 = metadat.addsum2$meta95low,
         y1 = metadat.addsum2$meta95high, 
         lty = 1, lwd = 3, col = "red")

points(seq(0.8, 3.8, by = 1),
       metadat.add.plot$eff.correct[metadat.add.plot$data=="childes_children"], 
       pch = 18, cex = 3, col = "chartreuse3")
segments(x0 = seq(0.8, 3.8, by = 1), x1 = seq(0.8, 3.8, by = 1),
         y0 = metadat.add.plot$eff.correct[metadat.add.plot$data=="childes_children"] - 1.96*metadat.add.plot$se[metadat.add.plot$data=="childes_children"],
         y1 = metadat.add.plot$eff.correct[metadat.add.plot$data=="childes_children"] + 1.96*metadat.add.plot$se[metadat.add.plot$data=="childes_children"], 
         lty = 1, lwd = 3, col = "chartreuse3")

points(seq(0.85, 3.85, by = 1),
       metadat.add.plot$eff.correct[metadat.add.plot$data=="childes_parents"], 
       pch = 19, cex = 2, col = "cyan2")
segments(x0 = seq(0.85, 3.85, by = 1), x1 = seq(0.85, 3.85, by = 1),
         y0 = metadat.add.plot$eff.correct[metadat.add.plot$data=="childes_parents"] - 1.96*metadat.add.plot$se[metadat.add.plot$data=="childes_parents"],
         y1 = metadat.add.plot$eff.correct[metadat.add.plot$data=="childes_parents"] + 1.96*metadat.add.plot$se[metadat.add.plot$data=="childes_parents"], 
         lty = 1, lwd = 3, col = "cyan2")
points(seq(0.9, 3.9, by = 1),
       metadat.add.plot$eff.correct[metadat.add.plot$data=="child_books"], 
       pch = 19, cex = 2, col = "cadetblue")
segments(x0 = seq(0.9, 3.9, by = 1), x1 = seq(0.9, 3.9, by = 1),
         y0 = metadat.add.plot$eff.correct[metadat.add.plot$data=="child_books"] - 1.96*metadat.add.plot$se[metadat.add.plot$data=="child_books"],
         y1 = metadat.add.plot$eff.correct[metadat.add.plot$data=="child_books"] + 1.96*metadat.add.plot$se[metadat.add.plot$data=="child_books"], 
         lty = 1, lwd = 3, col = "cadetblue")
points(seq(0.95, 3.95, by = 1),
       metadat.add.plot$eff.correct[metadat.add.plot$data=="kids_tv_combined"], 
       pch = 19, cex = 2, col = "darkblue")
segments(x0 = seq(0.95, 3.95, by = 1), x1 = seq(0.95, 3.95, by = 1),
         y0 = metadat.add.plot$eff.correct[metadat.add.plot$data=="kids_tv_combined"] - 1.96*metadat.add.plot$se[metadat.add.plot$data=="kids_tv_combined"],
         y1 = metadat.add.plot$eff.correct[metadat.add.plot$data=="kids_tv_combined"] + 1.96*metadat.add.plot$se[metadat.add.plot$data=="kids_tv_combined"], 
         lty = 1, lwd = 3, col = "darkblue")

points(seq(1.05, 4.05, by = 1),
       metadat.add.plot$eff.correct[metadat.add.plot$data=="adult_speech"], 
       pch = 15, cex = 2, col = "gold1")
segments(x0 = seq(1.05, 4.05, by = 1), x1 = seq(1.05, 4.05, by = 1),
         y0 = metadat.add.plot$eff.correct[metadat.add.plot$data=="adult_speech"] - 1.96*metadat.add.plot$se[metadat.add.plot$data=="adult_speech"],
         y1 = metadat.add.plot$eff.correct[metadat.add.plot$data=="adult_speech"] + 1.96*metadat.add.plot$se[metadat.add.plot$data=="adult_speech"], 
         lty = 1, lwd = 3, col = "gold1")
points(seq(1.1, 4.1, by = 1),
       metadat.add.plot$eff.correct[metadat.add.plot$data=="gutenberg"], 
       pch = 15, cex = 2, col = "darkorange")
segments(x0 = seq(1.1, 4.1, by = 1), x1 = seq(1.1, 4.1, by = 1),
         y0 = metadat.add.plot$eff.correct[metadat.add.plot$data=="gutenberg"] - 1.96*metadat.add.plot$se[metadat.add.plot$data=="gutenberg"],
         y1 = metadat.add.plot$eff.correct[metadat.add.plot$data=="gutenberg"] + 1.96*metadat.add.plot$se[metadat.add.plot$data=="gutenberg"], 
         lty = 1, lwd = 3, col = "darkorange")
points(seq(1.15, 4.15, by = 1),
       metadat.add.plot$eff.correct[metadat.add.plot$data=="simply_scripts"], 
       pch = 15, cex = 2, col = "plum2")
segments(x0 = seq(1.15, 4.15, by = 1), x1 = seq(1.15, 4.15, by = 1),
         y0 = metadat.add.plot$eff.correct[metadat.add.plot$data=="simply_scripts"] - 1.96*metadat.add.plot$se[metadat.add.plot$data=="simply_scripts"],
         y1 = metadat.add.plot$eff.correct[metadat.add.plot$data=="simply_scripts"] + 1.96*metadat.add.plot$se[metadat.add.plot$data=="simply_scripts"], 
         lty = 1, lwd = 3, col = "plum2")


legend("bottomleft", c("Child-produced speech", "Child-directed speech",
                       "Child-directed books", "Child-directed media",
                       "Adult-produced speech", "Adult-directed books", 
                       "Adult-directed media", "Meta Estimate"),
       col = c("chartreuse3", "cyan2", "cadetblue", "darkblue",
               "gold1", "darkorange", "plum2", "red"),
       pch = c(18, rep(19, 3), rep(15, 3), 17), 
       cex = 2, pt.cex = c(3, rep(2.5, 6), 3), bty = "n", ncol = 2)

par(op)
dev.off()

## Meta-regression across results
metadat.add$chad <- car::recode(metadat.add$datacat, "'childdir' = 'child'; 'childpro' = 'child'; 'adultpd' = 'adult'")
metadat.add$time <- car::recode(metadat.add$data, 
                             "'adult_speech' = 'mid'; 'childes_children' = 'mid'; 'childes_parents' = 'mid';
                            'child_books' = 'early'; 'gutenberg' = 'early';
                            'simply_scripts' = 'late'; 'kids_tv_combined' = 'late'")

# Comparisons across four attitude/stereotype topics
table(metadat.add$att) # only including gender stereotypes
meta.study1.add <- metagen(TE = eff.correct, seTE = se, sm = "MD", data = metadat.add) # using mean effect method
metareg.study1.att.add <- metareg(meta.study1.add, att)
summary(metareg.study1.att.add) # no difference; intercept is significantly higher than zero

# Comparisons across two age subgroups (child and adult)
table(metadat.add$chad)
meta.study1.chad.add <- metagen(TE = eff.correct, seTE = se, sm = "MD", data = metadat.add) # using mean effect method
metareg.study1.chad.add <- metareg(meta.study1.chad.add, chad)
summary(metareg.study1.chad.add) # no significant differences

# Comparisons across three time subgroups (early, mid, late)
table(metadat.add$time)
meta.study1.time.add <- metagen(TE = eff, seTE = se, sm = "MD", data = metadat.add) # using mean effect method
metareg.study1.time.add <- metareg(meta.study1.time.add, paste(time))
summary(metareg.study1.time.add) # no significant differences

## (10) Supplementary materials: Study 2 with single traits only ----
# Read in all SC-WEAT 
traitdat <- read.csv("embeddings_singletraits_071220.csv") # provided in the .RData summary object as well
names(traitdat) <- c("data", "categories", "attributes", 
                     "effsize", "miss", "pleft", "pright", "ptot", "se", "cohesion")

# Data preparation
length(table(traitdat$attributes)) # currently = 542 traits in at least one corpus

# Make new data frame with only 7 primary corpora
traitdatsub <- traitdat[traitdat$data=="childes_children" | traitdat$data=="childes_parents" | traitdat$data=="child_books" | traitdat$data=="kids_tv_combined" | traitdat$data=="adult_speech" | traitdat$data=="gutenberg" | traitdat$data=="simply_scripts",]

traitdatsub$attributes <- factor(traitdatsub$attributes)
traitdatsub$data <- factor(traitdatsub$data)

length(table(traitdatsub$attributes)) # now = 541 traits in at least one corpus

# drop factors with < 5 obs 
traitdatnew <- traitdatsub[!(as.numeric(traitdatsub$attributes) %in% which(table(traitdatsub$attributes) < 5)),]
traitdatnew$attributes <- factor(traitdatnew$attributes)
length(table(traitdatnew$attributes)) # now = 170 traits
table(traitdatnew$data) # variability across corpora in the number of traits that are actually present
# drop factors with < 4 obs
traitdatnew2 <- traitdatsub[!(as.numeric(traitdatsub$attributes) %in% which(table(traitdatsub$attributes) < 4)),]
traitdatnew2$attributes <- factor(traitdatnew2$attributes)
length(table(traitdatnew2$attributes)) # now = 256 traits
# drop factors with < 3 obs
traitdatnew3 <- traitdatsub[!(as.numeric(traitdatsub$attributes) %in% which(table(traitdatsub$attributes) < 3)),]
traitdatnew3$attributes <- factor(traitdatnew3$attributes)
length(table(traitdatnew3$attributes)) # now = 337 traits
# include only factors with all obs
traitdatnew4 <- traitdatsub[!(as.numeric(traitdatsub$attributes) %in% which(table(traitdatsub$attributes) < 7)),]
traitdatnew4$attributes <- factor(traitdatnew4$attributes)
length(table(traitdatnew4$attributes)) # now = 54 traits

# start with most restrictive that is still above N = 100 (>5 corpora)
traitdatnew <- traitdatnew[c("data", "categories", "attributes", "effsize", "se", "ptot")]
colnames(traitdatnew) <- c("data", "cat", "att", "eff", "se", "p")


# Also make new data frame with only adult corpora
traitdatadult <- traitdatsub[traitdatsub$data=="adult_speech" | traitdatsub$data=="gutenberg" | traitdatsub$data=="simply_scripts",]
traitdatadultnew <- traitdatadult[!(as.numeric(traitdatadult$attributes) %in% which(table(traitdatadult$attributes) < 3)),]
traitdatadultnew$attributes <- factor(traitdatadultnew$attributes)
length(table(traitdatadultnew$attributes)) # now = 167 traits in all three adult corpora

traitdatadultnew <- traitdatadultnew[c("data", "categories", "attributes", "effsize", "se", "ptot")]
colnames(traitdatadultnew) <- c("data", "cat", "att", "eff", "se", "p")



# Descriptives

# Mean effect sizes across datasets and across attributes
mean(traitdatnew$eff, na.rm = TRUE) 
range(traitdatnew$eff, na.rm = TRUE) 
# Mean effect sizes across datasets
by(traitdatnew$eff, list(traitdatnew$data), mean, na.rm = TRUE)
# Mean effect sizes across attributes
by(traitdatnew$eff, list(traitdatnew$att), mean, na.rm = TRUE)

# Meta analysis of effect sizes for each attribute (170 traits)
table(traitdatnew$att)
traitdatnew$attnum <- as.numeric(traitdatnew$att)
table(traitdatnew$att)
table(traitdatnew$attnum)

meta.alltrait <- list()
meta.traitmeans <- vector()
meta.traitse <- vector()
meta.traitp <- vector()

for (i in 1:length(table(traitdatnew$att))){
  meta.alltrait[[i]] <- metagen(TE = traitdatnew$eff[traitdatnew$attnum==i], seTE = traitdatnew$se[traitdatnew$attnum==i], sm = "MD")
  meta.traitmeans[i] <- meta.alltrait[[i]]$TE.fixed
  meta.traitse[i] <- meta.alltrait[[i]]$seTE.fixed
  meta.traitp[i] <- meta.alltrait[[i]]$pval.fixed
}

names(meta.alltrait) <- levels(traitdatnew$att)
meta.traitsum <- as.data.frame(meta.traitmeans)
colnames(meta.traitsum) <- "mean"
meta.traitsum$se <- meta.traitse
meta.traitsum$p <- meta.traitp
meta.traitsum$att <- levels(traitdatnew$att)

# now add on vectors for each sub database
table(traitdatnew$data)
meta.traitsum2 <- merge(meta.traitsum, 
                        traitdatnew[traitdatnew$data=="childes_children", 
                                    c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.traitsum2)[5:6] <- c("effchchild", "sechchild")
meta.traitsum2 <- merge(meta.traitsum2, 
                        traitdatnew[traitdatnew$data=="childes_parents", 
                                    c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.traitsum2)[7:8] <- c("effchpar", "sechpar")
meta.traitsum2 <- merge(meta.traitsum2, 
                        traitdatnew[traitdatnew$data=="child_books", 
                                    c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.traitsum2)[9:10] <- c("effchbook", "sechbook")
meta.traitsum2 <- merge(meta.traitsum2, 
                        traitdatnew[traitdatnew$data=="gutenberg", 
                                    c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.traitsum2)[11:12] <- c("effguten", "seguten")
meta.traitsum2 <- merge(meta.traitsum2, 
                        traitdatnew[traitdatnew$data=="kids_tv_combined", 
                                    c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.traitsum2)[13:14] <- c("effchtv", "sechtv")
meta.traitsum2 <- merge(meta.traitsum2, 
                        traitdatnew[traitdatnew$data=="simply_scripts", 
                                    c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.traitsum2)[15:16] <- c("effadtv", "seadtv")
meta.traitsum2 <- merge(meta.traitsum2, 
                        traitdatnew[traitdatnew$data=="adult_speech", 
                                    c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.traitsum2)[17:18] <- c("effadspch", "seadspch")


## Descriptives of meta-analytic data ----
# Overall data
mean(meta.traitsum2$mean)
range(meta.traitsum2$mean) 
head(meta.traitsum2$att[order(meta.traitsum2$mean, decreasing = TRUE)])
head(meta.traitsum2$att[order(meta.traitsum2$mean, decreasing = FALSE)])

## How many are significant?
length(which(meta.traitsum2$p < .05))
meta.traitsum2$sig <- ifelse(meta.traitsum2$p < .05, "sig", "nsig")
table(meta.traitsum2$sig)
binom.test(table(meta.traitsum2$sig)[2],
           dim(meta.traitsum2)[1]) 
meta.traitsum2$sig.mf <- ifelse(meta.traitsum2$p < .05 & meta.traitsum2$mean < 0, "sig.f", 
                                ifelse(meta.traitsum2$p < .05 & meta.traitsum2$mean > 0, "sig.m", "nsig"))
table(meta.traitsum2$sig.mf)/dim(meta.traitsum2)[1]


## How many fall beyond [-0.1, 0.1]?
meta.traitsum2$effcut <- cut(meta.traitsum2$mean, breaks = c(-2, -0.1, 0.1, 2), labels = c("sig.f", "nsig", "sig.m"))
table(meta.traitsum2$effcut) 
table(meta.traitsum2$effcut)/dim(meta.traitsum2)[1] 
binom.test(sum(table(meta.traitsum2$effcut)[c(1, 3)]),
           dim(meta.traitsum2)[1]) 

## How many fall beyond [-0.2, 0.2]?
meta.traitsum2$effcut2 <- cut(meta.traitsum2$mean, breaks = c(-2, -0.2, 0.2, 2), labels = c("sig.f", "nsig", "sig.m"))
table(meta.traitsum2$effcut2) 
table(meta.traitsum2$effcut2)/dim(meta.traitsum2)[1]
binom.test(sum(table(meta.traitsum2$effcut2)[c(1, 3)]),
           dim(meta.traitsum2)[1]) 

## How many fall beyond [-0.3, 0.3]?
meta.traitsum2$effcut3 <- cut(meta.traitsum2$mean, breaks = c(-2, -0.3, 0.3, 2), labels = c("sig.f", "nsig", "sig.m"))
table(meta.traitsum2$effcut3) 
table(meta.traitsum2$effcut3)/dim(meta.traitsum2)[1] 
binom.test(sum(table(meta.traitsum2$effcut3)[c(1, 3)]),
           dim(meta.traitsum2)[1]) 


## Are traits more female or more male?
t.test(meta.traitsum2$mean, mu = 0)
meta.traitsum2$malfem <- ifelse(meta.traitsum2$mean > 0, "mal", "fem")
table(meta.traitsum2$malfem) 
binom.test(table(meta.traitsum2$malfem)[1],
           dim(meta.traitsum2)[1]) 
# Note: Result is reported in SM

# Note: All summary results below are reported in main text, Table 2
# Within child-produced speech
mean(meta.traitsum2$effchchild, na.rm = TRUE)
t.test(meta.traitsum2$effchchild, mu = 0)
head(meta.traitsum2$att[order(meta.traitsum2$effchchild, decreasing = TRUE)])
head(meta.traitsum2$att[order(meta.traitsum2$effchchild, decreasing = FALSE)])
# Within child-directed speech
mean(meta.traitsum2$effchpar, na.rm = TRUE)
t.test(meta.traitsum2$effchpar, mu = 0)
head(meta.traitsum2$att[order(meta.traitsum2$effchpar, decreasing = TRUE)])
head(meta.traitsum2$att[order(meta.traitsum2$effchpar, decreasing = FALSE)])
# Within child-directed books
mean(meta.traitsum2$effchbook, na.rm = TRUE)
t.test(meta.traitsum2$effchbook, mu = 0)
head(meta.traitsum2$att[order(meta.traitsum2$effchbook, decreasing = TRUE)])
head(meta.traitsum2$att[order(meta.traitsum2$effchbook, decreasing = FALSE)])
# Within child-directed TV
mean(meta.traitsum2$effchtv, na.rm = TRUE)
t.test(meta.traitsum2$effchtv, mu = 0)
head(meta.traitsum2$att[order(meta.traitsum2$effchtv, decreasing = TRUE)])
head(meta.traitsum2$att[order(meta.traitsum2$effchtv, decreasing = FALSE)])
# Within adult-directed speech
mean(meta.traitsum2$effadspch, na.rm = TRUE)
t.test(meta.traitsum2$effadspch, mu = 0)
head(meta.traitsum2$att[order(meta.traitsum2$effadspch, decreasing = TRUE)])
head(meta.traitsum2$att[order(meta.traitsum2$effadspch, decreasing = FALSE)])
# Within adult-directed books
mean(meta.traitsum2$effguten, na.rm = TRUE)
t.test(meta.traitsum2$effguten, mu = 0)
head(meta.traitsum2$att[order(meta.traitsum2$effguten, decreasing = TRUE)])
head(meta.traitsum2$att[order(meta.traitsum2$effguten, decreasing = FALSE)])
# Within adult-directed tv
mean(meta.traitsum2$effadtv, na.rm = TRUE)
t.test(meta.traitsum2$effadtv, mu = 0)
head(meta.traitsum2$att[order(meta.traitsum2$effadtv, decreasing = TRUE)])
head(meta.traitsum2$att[order(meta.traitsum2$effadtv, decreasing = FALSE)])

## Meta-regression across results ----
# combine all effects, collapsing across the content of the trait
metareg.traitsum <- as.data.frame(c(meta.traitsum2$effchchild, meta.traitsum2$effchpar, 
                                    meta.traitsum2$effchbook, meta.traitsum2$effguten,
                                    meta.traitsum2$effchtv, meta.traitsum2$effadtv,
                                    meta.traitsum2$effadspch))
metareg.traitsum$se <- c(meta.traitsum2$sechchild, meta.traitsum2$sechpar, 
                         meta.traitsum2$sechbook, meta.traitsum2$seguten,
                         meta.traitsum2$sechtv, meta.traitsum2$seadtv,
                         meta.traitsum2$seadspch)
metareg.traitsum$corpus <- rep(c("effchchild", "effchpar", "effchbook", 
                                 "effguten", "effchtv", "effadtv", "effadspch"), each = 170)
metareg.traitsum$corpus <- factor(metareg.traitsum$corpus, 
                                  levels = c("effchchild", "effchpar", "effchbook", "effchtv",
                                             "effadspch", "effguten", "effadtv"))
metareg.traitsum$chad <- car::recode(metareg.traitsum$corpus, 
                                     "'effchpar' = 'child'; 'effchbook' = 'child'; 'effchtv' = 'child'; 'effchchild' = 'child';
                          'effadspch' = 'adult'; 'effguten' = 'adult'; 'effadtv' = 'adult'")
metareg.traitsum$chad <- factor(metareg.traitsum$chad, levels = c("child", "adult"))
table(metareg.traitsum$chad)

metareg.traitsum$time <- car::recode(metareg.traitsum$corpus, "'effchpar' = 'mid'; 'effchbook' = 'early'; 
                          'effchtv' = 'late'; 'effchchild' = 'mid';
                          'effadspch' = 'mid'; 'effguten' = 'early'; 'effadtv' = 'late'")
metareg.traitsum$time <- factor(metareg.traitsum$time, levels = c("early", "mid", "late"))
table(metareg.traitsum$time)

colnames(metareg.traitsum) <- c("eff.correct", "se", "corpus", "chad", "time")

meta.study2 <- metagen(TE = eff.correct, seTE = se, sm = "MD", data = metareg.traitsum)

# Comparisons across two age subgroups (child and adult)
table(metareg.traitsum$chad)
metareg.study2.chad <- metareg(meta.study2, chad)
summary(metareg.study2.chad) # significant difference: adult is lower than child
bubble.metareg(metareg.study2.chad, ylim = c(-2, 2))

#  Comparisons across three time subgroups (early, mid, late)
table(metareg.traitsum$time)
metareg.study2.time <- metareg(meta.study2, time)
summary(metareg.study2.time) # no significant differences


# Different thresholds (7/7 corpora and 1/7 corpora) ----
# Note: Results are reported in supplementary materials (SM Tables S5.1 and S5.2)
## 7/7 corpora
length(table(traitdatnew4$attributes)) # 54 traits with all 7 corpora
traitdatnew.cons <- traitdatnew4[c("data", "categories", "attributes", "effsize", "se", "ptot")]
colnames(traitdatnew.cons) <- c("data", "cat", "att", "eff", "se", "p")
traitdatnew.cons$attnum <- as.numeric(traitdatnew.cons$att)
table(traitdatnew.cons$att)
table(traitdatnew.cons$attnum)

meta.cons.alltrait <- list()
meta.cons.traitmeans <- vector()
meta.cons.traitse <- vector()
meta.cons.traitp <- vector()
for (i in 1:length(table(traitdatnew.cons$att))){
  meta.cons.alltrait[[i]] <- metagen(TE = traitdatnew.cons$eff[traitdatnew.cons$attnum==i], 
                                     seTE = traitdatnew.cons$se[traitdatnew.cons$attnum==i], sm = "MD")
  meta.cons.traitmeans[i] <- meta.cons.alltrait[[i]]$TE.fixed
  meta.cons.traitse[i] <- meta.cons.alltrait[[i]]$seTE.fixed
  meta.cons.traitp[i] <- meta.cons.alltrait[[i]]$pval.fixed
}

names(meta.cons.alltrait) <- levels(traitdatnew.cons$att)
meta.cons.traitsum <- as.data.frame(meta.cons.traitmeans)
colnames(meta.cons.traitsum) <- "mean"
meta.cons.traitsum$se <- meta.cons.traitse
meta.cons.traitsum$p <- meta.cons.traitp
meta.cons.traitsum$att <- levels(traitdatnew.cons$att)

# now add on vectors for each sub database
table(traitdatnew.cons$data)
meta.cons.traitsum2 <- merge(meta.cons.traitsum, 
                             traitdatnew.cons[traitdatnew.cons$data=="childes_children", 
                                              c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.cons.traitsum2)[5:6] <- c("effchchild", "sechchild")
meta.cons.traitsum2 <- merge(meta.cons.traitsum2, 
                             traitdatnew.cons[traitdatnew.cons$data=="childes_parents", 
                                              c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.cons.traitsum2)[7:8] <- c("effchpar", "sechpar")
meta.cons.traitsum2 <- merge(meta.cons.traitsum2, 
                             traitdatnew.cons[traitdatnew.cons$data=="child_books", 
                                              c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.cons.traitsum2)[9:10] <- c("effchbook", "sechbook")
meta.cons.traitsum2 <- merge(meta.cons.traitsum2, 
                             traitdatnew.cons[traitdatnew.cons$data=="gutenberg", 
                                              c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.cons.traitsum2)[11:12] <- c("effguten", "seguten")
meta.cons.traitsum2 <- merge(meta.cons.traitsum2, 
                             traitdatnew.cons[traitdatnew.cons$data=="kids_tv_combined", 
                                              c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.cons.traitsum2)[13:14] <- c("effchtv", "sechtv")
meta.cons.traitsum2 <- merge(meta.cons.traitsum2, 
                             traitdatnew.cons[traitdatnew.cons$data=="simply_scripts", 
                                              c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.cons.traitsum2)[15:16] <- c("effadtv", "seadtv")
meta.cons.traitsum2 <- merge(meta.cons.traitsum2, 
                             traitdatnew.cons[traitdatnew.cons$data=="adult_speech", 
                                              c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.cons.traitsum2)[17:18] <- c("effadspch", "seadspch")

## Descriptives of conservative meta-analytic data
# Overall data
mean(meta.cons.traitsum2$mean)
range(meta.cons.traitsum2$mean) 
head(meta.cons.traitsum2$att[order(meta.cons.traitsum2$mean, decreasing = TRUE)])
head(meta.cons.traitsum2$att[order(meta.cons.traitsum2$mean, decreasing = FALSE)])

## How many are significant?
length(which(meta.cons.traitsum2$p < .05))
meta.cons.traitsum2$sig <- ifelse(meta.cons.traitsum2$p < .05, "sig", "nsig")
table(meta.cons.traitsum2$sig)
binom.test(table(meta.cons.traitsum2$sig)[2],
           dim(meta.cons.traitsum2)[1]) 

## Are traits more female or more male?
t.test(meta.cons.traitsum2$mean, mu = 0)
meta.cons.traitsum2$malfem <- ifelse(meta.cons.traitsum2$mean > 0, "mal", "fem")
table(meta.cons.traitsum2$malfem) 
binom.test(table(meta.cons.traitsum2$malfem)[1],
           dim(meta.cons.traitsum2)[1]) 

# Within child-produced speech
mean(meta.cons.traitsum2$effchchild, na.rm = TRUE)
t.test(meta.cons.traitsum2$effchchild, mu = 0)
head(meta.cons.traitsum2$att[order(meta.cons.traitsum2$effchchild, decreasing = TRUE)])
head(meta.cons.traitsum2$att[order(meta.cons.traitsum2$effchchild, decreasing = FALSE)])
# Within child-directed speech
mean(meta.cons.traitsum2$effchpar, na.rm = TRUE)
t.test(meta.cons.traitsum2$effchpar, mu = 0)
head(meta.cons.traitsum2$att[order(meta.cons.traitsum2$effchpar, decreasing = TRUE)])
head(meta.cons.traitsum2$att[order(meta.cons.traitsum2$effchpar, decreasing = FALSE)])
# Within child-directed books
mean(meta.cons.traitsum2$effchbook, na.rm = TRUE)
t.test(meta.cons.traitsum2$effchbook, mu = 0)
head(meta.cons.traitsum2$att[order(meta.cons.traitsum2$effchbook, decreasing = TRUE)])
head(meta.cons.traitsum2$att[order(meta.cons.traitsum2$effchbook, decreasing = FALSE)])
# Within child-directed TV
mean(meta.cons.traitsum2$effchtv, na.rm = TRUE)
t.test(meta.cons.traitsum2$effchtv, mu = 0)
head(meta.cons.traitsum2$att[order(meta.cons.traitsum2$effchtv, decreasing = TRUE)])
head(meta.cons.traitsum2$att[order(meta.cons.traitsum2$effchtv, decreasing = FALSE)])
# Within adult-directed speech
mean(meta.cons.traitsum2$effadspch, na.rm = TRUE)
t.test(meta.cons.traitsum2$effadspch, mu = 0)
head(meta.cons.traitsum2$att[order(meta.cons.traitsum2$effadspch, decreasing = TRUE)])
head(meta.cons.traitsum2$att[order(meta.cons.traitsum2$effadspch, decreasing = FALSE)])
# Within adult-directed books
mean(meta.cons.traitsum2$effguten, na.rm = TRUE)
t.test(meta.cons.traitsum2$effguten, mu = 0)
head(meta.cons.traitsum2$att[order(meta.cons.traitsum2$effguten, decreasing = TRUE)])
head(meta.cons.traitsum2$att[order(meta.cons.traitsum2$effguten, decreasing = FALSE)])
# Within adult-directed tv
mean(meta.cons.traitsum2$effadtv, na.rm = TRUE)
t.test(meta.cons.traitsum2$effadtv, mu = 0)
head(meta.cons.traitsum2$att[order(meta.cons.traitsum2$effadtv, decreasing = TRUE)])
head(meta.cons.traitsum2$att[order(meta.cons.traitsum2$effadtv, decreasing = FALSE)])

## 1/7 corpora 
length(table(traitdatsub$attributes)) # 541 traits with 1/7 corpora
traitdatnew.lib <- traitdatsub[c("data", "categories", "attributes", "effsize", "se", "ptot")]
colnames(traitdatnew.lib) <- c("data", "cat", "att", "eff", "se", "p")
traitdatnew.lib$attnum <- as.numeric(traitdatnew.lib$att)
table(traitdatnew.lib$att)
table(traitdatnew.lib$attnum)

meta.lib.alltrait <- list()
meta.lib.traitmeans <- vector()
meta.lib.traitse <- vector()
meta.lib.traitp <- vector()
for (i in 1:length(table(traitdatnew.lib$att))){
  meta.lib.alltrait[[i]] <- metagen(TE = traitdatnew.lib$eff[traitdatnew.lib$attnum==i], 
                                    seTE = traitdatnew.lib$se[traitdatnew.lib$attnum==i], sm = "MD")
  meta.lib.traitmeans[i] <- meta.lib.alltrait[[i]]$TE.fixed
  meta.lib.traitse[i] <- meta.lib.alltrait[[i]]$seTE.fixed
  meta.lib.traitp[i] <- meta.lib.alltrait[[i]]$pval.fixed
}

names(meta.lib.alltrait) <- levels(traitdatnew.lib$att)
meta.lib.traitsum <- as.data.frame(meta.lib.traitmeans)
colnames(meta.lib.traitsum) <- "mean"
meta.lib.traitsum$se <- meta.lib.traitse
meta.lib.traitsum$p <- meta.lib.traitp
meta.lib.traitsum$att <- levels(traitdatnew.lib$att)

# now add on vectors for each sub database
table(traitdatnew.lib$data)
meta.lib.traitsum2 <- merge(meta.lib.traitsum, 
                            traitdatnew.lib[traitdatnew.lib$data=="childes_children", 
                                            c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.lib.traitsum2)[5:6] <- c("effchchild", "sechchild")
meta.lib.traitsum2 <- merge(meta.lib.traitsum2, 
                            traitdatnew.lib[traitdatnew.lib$data=="childes_parents", 
                                            c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.lib.traitsum2)[7:8] <- c("effchpar", "sechpar")
meta.lib.traitsum2 <- merge(meta.lib.traitsum2, 
                            traitdatnew.lib[traitdatnew.lib$data=="child_books", 
                                            c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.lib.traitsum2)[9:10] <- c("effchbook", "sechbook")
meta.lib.traitsum2 <- merge(meta.lib.traitsum2, 
                            traitdatnew.lib[traitdatnew.lib$data=="gutenberg", 
                                            c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.lib.traitsum2)[11:12] <- c("effguten", "seguten")
meta.lib.traitsum2 <- merge(meta.lib.traitsum2, 
                            traitdatnew.lib[traitdatnew.lib$data=="kids_tv_combined", 
                                            c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.lib.traitsum2)[13:14] <- c("effchtv", "sechtv")
meta.lib.traitsum2 <- merge(meta.lib.traitsum2, 
                            traitdatnew.lib[traitdatnew.lib$data=="simply_scripts", 
                                            c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.lib.traitsum2)[15:16] <- c("effadtv", "seadtv")
meta.lib.traitsum2 <- merge(meta.lib.traitsum2, 
                            traitdatnew.lib[traitdatnew.lib$data=="adult_speech", 
                                            c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.lib.traitsum2)[17:18] <- c("effadspch", "seadspch")

## Descriptives of more liberal meta-analytic data
# Overall data
mean(meta.lib.traitsum2$mean)
range(meta.lib.traitsum2$mean) 
head(meta.lib.traitsum2$att[order(meta.lib.traitsum2$mean, decreasing = TRUE)])
head(meta.lib.traitsum2$att[order(meta.lib.traitsum2$mean, decreasing = FALSE)])

## How many are significant?
length(which(meta.lib.traitsum2$p < .05))
meta.lib.traitsum2$sig <- ifelse(meta.lib.traitsum2$p < .05, "sig", "nsig")
table(meta.lib.traitsum2$sig)
binom.test(table(meta.lib.traitsum2$sig)[2],
           dim(meta.lib.traitsum2)[1]) 

## Are traits more female or more male?
t.test(meta.lib.traitsum2$mean, mu = 0)
meta.lib.traitsum2$malfem <- ifelse(meta.lib.traitsum2$mean > 0, "mal", "fem")
table(meta.lib.traitsum2$malfem) 
binom.test(table(meta.lib.traitsum2$malfem)[1],
           dim(meta.lib.traitsum2)[1]) 

# Within child-produced speech
mean(meta.lib.traitsum2$effchchild, na.rm = TRUE)
t.test(meta.lib.traitsum2$effchchild, mu = 0)
head(meta.lib.traitsum2$att[order(meta.lib.traitsum2$effchchild, decreasing = TRUE)])
head(meta.lib.traitsum2$att[order(meta.lib.traitsum2$effchchild, decreasing = FALSE)])
# Within child-directed speech
mean(meta.lib.traitsum2$effchpar, na.rm = TRUE)
t.test(meta.lib.traitsum2$effchpar, mu = 0)
head(meta.lib.traitsum2$att[order(meta.lib.traitsum2$effchpar, decreasing = TRUE)])
head(meta.lib.traitsum2$att[order(meta.lib.traitsum2$effchpar, decreasing = FALSE)])
# Within child-directed books
mean(meta.lib.traitsum2$effchbook, na.rm = TRUE)
t.test(meta.lib.traitsum2$effchbook, mu = 0)
head(meta.lib.traitsum2$att[order(meta.lib.traitsum2$effchbook, decreasing = TRUE)])
head(meta.lib.traitsum2$att[order(meta.lib.traitsum2$effchbook, decreasing = FALSE)])
# Within child-directed TV
mean(meta.lib.traitsum2$effchtv, na.rm = TRUE)
t.test(meta.lib.traitsum2$effchtv, mu = 0)
head(meta.lib.traitsum2$att[order(meta.lib.traitsum2$effchtv, decreasing = TRUE)])
head(meta.lib.traitsum2$att[order(meta.lib.traitsum2$effchtv, decreasing = FALSE)])
# Within adult-directed speech
mean(meta.lib.traitsum2$effadspch, na.rm = TRUE)
t.test(meta.lib.traitsum2$effadspch, mu = 0)
head(meta.lib.traitsum2$att[order(meta.lib.traitsum2$effadspch, decreasing = TRUE)])
head(meta.lib.traitsum2$att[order(meta.lib.traitsum2$effadspch, decreasing = FALSE)])
# Within adult-directed books
mean(meta.lib.traitsum2$effguten, na.rm = TRUE)
t.test(meta.lib.traitsum2$effguten, mu = 0)
head(meta.lib.traitsum2$att[order(meta.lib.traitsum2$effguten, decreasing = TRUE)])
head(meta.lib.traitsum2$att[order(meta.lib.traitsum2$effguten, decreasing = FALSE)])
# Within adult-directed tv
mean(meta.lib.traitsum2$effadtv, na.rm = TRUE)
t.test(meta.lib.traitsum2$effadtv, mu = 0)
head(meta.lib.traitsum2$att[order(meta.lib.traitsum2$effadtv, decreasing = TRUE)])
head(meta.lib.traitsum2$att[order(meta.lib.traitsum2$effadtv, decreasing = FALSE)])



# Comparison of synonym groups to single words ----
meta.grp.merge <- meta.groups.traitsum[c("att", "mean", "p", "se")]
colnames(meta.grp.merge) <- c("att", "mean.grp", "p.grp", "se.grp")
meta.eff.all <- merge(meta.traitsum, meta.grp.merge, by = "att")

length(table(which(meta.eff.all$mean > meta.eff.all$mean.grp)))/dim(meta.eff.all)[1]
length(table(which(meta.eff.all$p < meta.eff.all$p.grp)))/dim(meta.eff.all)[1]
length(table(which(meta.eff.all$se > meta.eff.all$se.grp)))/dim(meta.eff.all)[1] 

cor.test(meta.eff.all$mean, meta.eff.all$mean.grp) # both approaches are significantly correlated with their means.

# Plotting ----
# Meta-analytic effects
# which traits are the ones on the cut-offs? 
# "reasonable" (effect size ~ -0.1) and "severe" (effect size ~ .1)
# "clumsy" (effect size ~ .2) and "strong" (effect size ~ .2)
# "independent" (effect size ~ .3) and "deep" (effect size ~ .3)
meta.traitsum[order(meta.traitsum$mean, decreasing = TRUE),]

# first page
pdf(file = "metatrait_singles_sm_page1.pdf", width = 45, height = 15)
op <- par(mar = c(18, 10, 2, 2))
plot(meta.traitsum$mean[order(meta.traitsum$mean, decreasing = TRUE)][1:85], ylim = c(-1.2, 0.8),
     xlab = "", ylab = "", axes = FALSE,
     pch = 20, cex = 4, col = "gray")
box()
axis(1, at = 1:85, 
     labels = meta.traitsum$att[order(meta.traitsum$mean, decreasing = TRUE)][1:85], las = 2, cex.axis = 3)
axis(2, at = seq(-1.4, 1.4, by = 0.2), labels = round(seq(-1.4, 1.4, by = 0.2), 2), las = 1, cex.axis = 3)
mtext("Meta-analytic Effect (Trait = Male)", line = 7, side = 2, cex = 3)
abline(h = 0, lty = 2, col = "darkgray", lwd = 4)
segments(x0 = c(1:85), x1 = c(1:85), 
         y0 = meta.traitsum$mean[order(meta.traitsum$mean, decreasing = TRUE)][1:85] - 1.96*meta.traitsum$se[order(meta.traitsum$mean, decreasing = TRUE)][1:85],
         y1 = meta.traitsum$mean[order(meta.traitsum$mean, decreasing = TRUE)][1:85] + 1.96*meta.traitsum$se[order(meta.traitsum$mean, decreasing = TRUE)][1:85],
         lty = 1, lwd = 3, col = "gray")
abline(v = which(meta.traitsum$att[order(meta.traitsum$mean, decreasing = TRUE)] == "deep"), 
       col = "black", lty = 3, lwd = 7)
abline(v = which(meta.traitsum$att[order(meta.traitsum$mean, decreasing = TRUE)] == "independent"),
       col = "black", lty = 3, lwd = 7)
abline(v = which(meta.traitsum$att[order(meta.traitsum$mean, decreasing = TRUE)] == "strong"), 
       col = "black", lty = 2, lwd = 7)
abline(v = which(meta.traitsum$att[order(meta.traitsum$mean, decreasing = TRUE)] == "clumsy"),
       col = "black", lty = 2, lwd = 7)
abline(v = which(meta.traitsum$att[order(meta.traitsum$mean, decreasing = TRUE)] == "severe"), 
       col = "black", lty = 1, lwd = 7)
abline(v = which(meta.traitsum$att[order(meta.traitsum$mean, decreasing = TRUE)] == "reasonable"),
       col = "black", lty = 1, lwd = 7)
legend("bottomleft", c("[-0.1, 0.1]", "[-0.2, 0.2]", "[-0.3, 0.3]"), title = "Effects beyond",
       col = "black", lty = c(1, 2, 3), lwd = 5, bty = "n", cex = 3.5)
par(op)
dev.off()

# second page
pdf(file = "metatrait_singles_sm_page2.pdf", width = 45, height = 15)
op <- par(mar = c(18, 10, 2, 2))
plot(meta.traitsum$mean[order(meta.traitsum$mean, decreasing = TRUE)][86:170], ylim = c(-1.2, 0.8),
     xlab = "", ylab = "", axes = FALSE,
     pch = 20, cex = 4, col = "gray")
box()
axis(1, at = 1:85, 
     labels = meta.traitsum$att[order(meta.traitsum$mean, decreasing = TRUE)][86:170], las = 2, cex.axis = 3)
axis(2, at = seq(-1.4, 1.4, by = 0.2), labels = round(seq(-1.4, 1.4, by = 0.2), 2), las = 1, cex.axis = 3)
mtext("Meta-analytic Effect (Trait = Male)", line = 7, side = 2, cex = 3)
abline(h = 0, lty = 2, col = "darkgray", lwd = 4)
segments(x0 = c(1:85), x1 = c(1:85), 
         y0 = meta.traitsum$mean[order(meta.traitsum$mean, decreasing = TRUE)][86:170] - 1.96*meta.traitsum$se[order(meta.traitsum$mean, decreasing = TRUE)][86:170],
         y1 = meta.traitsum$mean[order(meta.traitsum$mean, decreasing = TRUE)][86:170] + 1.96*meta.traitsum$se[order(meta.traitsum$mean, decreasing = TRUE)][86:170],
         lty = 1, lwd = 3, col = "gray")
abline(v = which(meta.traitsum$att[order(meta.traitsum$mean, decreasing = TRUE)][86:170] == "deep"), 
       col = "black", lty = 3, lwd = 7)
abline(v = which(meta.traitsum$att[order(meta.traitsum$mean, decreasing = TRUE)][86:170] == "independent"),
       col = "black", lty = 3, lwd = 7)
abline(v = which(meta.traitsum$att[order(meta.traitsum$mean, decreasing = TRUE)][86:170] == "strong"), 
       col = "black", lty = 2, lwd = 7)
abline(v = which(meta.traitsum$att[order(meta.traitsum$mean, decreasing = TRUE)][86:170] == "clumsy"),
       col = "black", lty = 2, lwd = 7)
abline(v = which(meta.traitsum$att[order(meta.traitsum$mean, decreasing = TRUE)][86:170] == "severe"), 
       col = "black", lty = 1, lwd = 7)
abline(v = which(meta.traitsum$att[order(meta.traitsum$mean, decreasing = TRUE)][86:170] == "reasonable"),
       col = "black", lty = 1, lwd = 7)
legend("bottomleft", c("[-0.1, 0.1]", "[-0.2, 0.2]", "[-0.3, 0.3]"), title = "Effects beyond",
       col = "black", lty = c(1, 2, 3), lwd = 5, bty = "n", cex = 3.5)
par(op)
dev.off()



# Meta-analytic effects for 7/7 corpora threshold
pdf(file = "metatrait_cons_plot2.pdf", width = 20, height = 10)
op <- par(mar = c(12, 5, 2, 2))
plot(meta.cons.traitsum$mean[order(meta.cons.traitsum$mean, decreasing = TRUE)], ylim = c(-1, 0.6),
     xlab = "", ylab = "Meta-analytic Effect (Trait = Male)", axes = FALSE,
     pch = 20, cex = 2, col = "gray")
box()
axis(1, at = 1:54, 
     labels = meta.cons.traitsum$att[order(meta.cons.traitsum$mean, decreasing = TRUE)], las = 2)
axis(2, at = seq(-1.4, 1.4, by = 0.2), labels = round(seq(-1.4, 1.4, by = 0.2), 2), las = 1)
abline(h = 0, lty = 2, col = "gray")
segments(x0 = c(1:54), x1 = c(1:54), 
         y0 = meta.cons.traitsum$mean[order(meta.cons.traitsum$mean, decreasing = TRUE)] - meta.cons.traitsum$se[order(meta.cons.traitsum$mean, decreasing = TRUE)],
         y1 = meta.cons.traitsum$mean[order(meta.cons.traitsum$mean, decreasing = TRUE)] + meta.cons.traitsum$se[order(meta.cons.traitsum$mean, decreasing = TRUE)],
         lty = 1, lwd = 2, col = "gray")
par(op)
dev.off()

# Meta-analytic effects for 1/7 corpora threshold
pdf(file = "metatrait_lib_plot3.pdf", width = 40, height = 10)
op <- par(mar = c(12, 5, 2, 2))
plot(meta.lib.traitsum$mean[order(meta.lib.traitsum$mean, decreasing = TRUE)], ylim = c(-1.1, 1),
     xlab = "", ylab = "Meta-analytic Effect (Trait = Male)", axes = FALSE,
     pch = 20, cex = 2, col = "gray")
box()
axis(1, at = 1:541, 
     labels = meta.lib.traitsum$att[order(meta.lib.traitsum$mean, decreasing = TRUE)], las = 2)
axis(2, at = seq(-1.4, 1.4, by = 0.2), labels = round(seq(-1.4, 1.4, by = 0.2), 2), las = 1)
abline(h = 0, lty = 2, col = "gray")
segments(x0 = c(1:541), x1 = c(1:541), 
         y0 = meta.lib.traitsum$mean[order(meta.lib.traitsum$mean, decreasing = TRUE)] - meta.lib.traitsum$se[order(meta.lib.traitsum$mean, decreasing = TRUE)],
         y1 = meta.lib.traitsum$mean[order(meta.lib.traitsum$mean, decreasing = TRUE)] + meta.lib.traitsum$se[order(meta.lib.traitsum$mean, decreasing = TRUE)],
         lty = 1, lwd = 2, col = "gray")
par(op)
dev.off()



## (11) Supplementary materials: Study 3 gender-occupation associations with different cut-offs ----
# Repeat meta-analysis with conservative threshold (7/7 corpora)
# Note: results are reported in supplementary materials (SM Table S5.3)
profdat.cons <- profdatnew4[c("data", "categories", "attributes", "effsize", "se", "ptot")]
colnames(profdat.cons) <- c("data", "cat", "att", "eff", "se", "p")
length(table(profdat.cons$att)) # 17 professions

profdat.cons$attnum <- as.numeric(profdat.cons$att)
table(profdat.cons$att)
table(profdat.cons$attnum)

meta.cons.allprof <- list()
meta.cons.profmeans <- vector()
meta.cons.profse <- vector()
meta.cons.profp <- vector()

for (i in 1:length(table(profdat.cons$att))){
  meta.cons.allprof[[i]] <- metagen(TE = profdat.cons$eff[profdat.cons$attnum==i], seTE = profdat.cons$se[profdat.cons$attnum==i], sm = "MD")
  meta.cons.profmeans[i] <- meta.cons.allprof[[i]]$TE.fixed
  meta.cons.profse[i] <- meta.cons.allprof[[i]]$seTE.fixed
  meta.cons.profp[i] <- meta.cons.allprof[[i]]$pval.fixed
}

names(meta.cons.allprof) <- levels(profdat.cons$att)

meta.cons.profsum <- as.data.frame(meta.cons.profmeans)
colnames(meta.cons.profsum) <- "mean"
meta.cons.profsum$se <- meta.cons.profse
meta.cons.profsum$p <- meta.cons.profp
meta.cons.profsum$att <- levels(profdat.cons$att)

# now add on vectors for each sub database
table(profdat.cons$data)

meta.cons.profsum2 <- merge(meta.cons.profsum, 
                            profdat.cons[profdat.cons$data=="childes_children", 
                                         c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.cons.profsum2)[5:6] <- c("effchchild", "sechchild")
meta.cons.profsum2 <- merge(meta.cons.profsum2, 
                            profdat.cons[profdat.cons$data=="childes_parents", 
                                         c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.cons.profsum2)[7:8] <- c("effchpar", "sechpar")
meta.cons.profsum2 <- merge(meta.cons.profsum2, 
                            profdat.cons[profdat.cons$data=="child_books", 
                                         c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.cons.profsum2)[9:10] <- c("effchbook", "sechbook")
meta.cons.profsum2 <- merge(meta.cons.profsum2, 
                            profdat.cons[profdat.cons$data=="gutenberg", 
                                         c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.cons.profsum2)[11:12] <- c("effguten", "seguten")
meta.cons.profsum2 <- merge(meta.cons.profsum2, 
                            profdat.cons[profdat.cons$data=="kids_tv_combined", 
                                         c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.cons.profsum2)[13:14] <- c("effchtv", "sechtv")
meta.cons.profsum2 <- merge(meta.cons.profsum2, 
                            profdat.cons[profdat.cons$data=="simply_scripts", 
                                         c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.cons.profsum2)[15:16] <- c("effadtv", "seadtv")
meta.cons.profsum2 <- merge(meta.cons.profsum2, 
                            profdat.cons[profdat.cons$data=="adult_speech", 
                                         c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.cons.profsum2)[17:18] <- c("effadspch", "seadspch")

## Descriptives of meta data
# Overall data
range(meta.cons.profsum2$mean) # smaller range from meta-analyses
head(meta.cons.profsum2$att[order(meta.cons.profsum2$mean, decreasing = TRUE)])
head(meta.cons.profsum2$att[order(meta.cons.profsum2$mean, decreasing = FALSE)])

## Are profs gendered (significant)?
meta.cons.profsum2$sig <- ifelse(meta.cons.profsum2$p < .05, "sig", "nsig")
table(meta.cons.profsum2$sig)
binom.test(table(meta.cons.profsum2$sig)[2],
           dim(meta.cons.profsum2)[1]) 

## Are profs more female or more male?
t.test(meta.cons.profsum2$mean, mu = 0)
meta.cons.profsum2$malfem <- ifelse(meta.cons.profsum2$mean > 0, "mal", "fem")
table(meta.cons.profsum2$malfem) 
binom.test(table(meta.cons.profsum2$malfem)[2],
           dim(meta.cons.profsum2)[1]) 

# Within child-produced speech
mean(meta.cons.profsum2$effchchild, na.rm = TRUE)
t.test(meta.cons.profsum2$effchchild, mu = 0)
head(meta.cons.profsum2$att[order(meta.cons.profsum2$effchchild, decreasing = TRUE)])
head(meta.cons.profsum2$att[order(meta.cons.profsum2$effchchild, decreasing = FALSE)])
# Within child-directed speech
mean(meta.cons.profsum2$effchpar, na.rm = TRUE)
t.test(meta.cons.profsum2$effchpar, mu = 0)
head(meta.cons.profsum2$att[order(meta.cons.profsum2$effchpar, decreasing = TRUE)])
head(meta.cons.profsum2$att[order(meta.cons.profsum2$effchpar, decreasing = FALSE)])
# Within child-directed books
mean(meta.cons.profsum2$effchbook, na.rm = TRUE)
t.test(meta.cons.profsum2$effchbook, mu = 0)
head(meta.cons.profsum2$att[order(meta.cons.profsum2$effchbook, decreasing = TRUE)])
head(meta.cons.profsum2$att[order(meta.cons.profsum2$effchbook, decreasing = FALSE)])
# Within child-directed TV
mean(meta.cons.profsum2$effchtv, na.rm = TRUE)
t.test(meta.cons.profsum2$effchtv, mu = 0)
head(meta.cons.profsum2$att[order(meta.cons.profsum2$effchtv, decreasing = TRUE)])
head(meta.cons.profsum2$att[order(meta.cons.profsum2$effchtv, decreasing = FALSE)])
# Within adult-directed speech
mean(meta.cons.profsum2$effadspch, na.rm = TRUE)
t.test(meta.cons.profsum2$effadspch, mu = 0)
head(meta.cons.profsum2$att[order(meta.cons.profsum2$effadspch, decreasing = TRUE)])
head(meta.cons.profsum2$att[order(meta.cons.profsum2$effadspch, decreasing = FALSE)])
# Within adult-directed books
mean(meta.cons.profsum2$effguten, na.rm = TRUE)
t.test(meta.cons.profsum2$effguten, mu = 0)
head(meta.cons.profsum2$att[order(meta.cons.profsum2$effguten, decreasing = TRUE)])
head(meta.cons.profsum2$att[order(meta.cons.profsum2$effguten, decreasing = FALSE)])
# Within adult-directed tv
mean(meta.cons.profsum2$effadtv, na.rm = TRUE)
t.test(meta.cons.profsum2$effadtv, mu = 0)
head(meta.cons.profsum2$att[order(meta.cons.profsum2$effadtv, decreasing = TRUE)])
head(meta.cons.profsum2$att[order(meta.cons.profsum2$effadtv, decreasing = FALSE)])

## With more conservative limits
pdf(file = "metaprof_cons_plot2.pdf", width = 20, height = 10)
op <- par(mar = c(12, 8, 2, 2))
plot(meta.cons.profsum$mean[order(meta.cons.profsum$mean, decreasing = TRUE)], ylim = c(-1.6, 1.6),
     xlab = "", ylab = "", axes = FALSE,
     pch = 20, cex = 2, col = "gray")
box()
axis(1, at = 1:dim(meta.cons.profsum)[1], 
     labels = meta.cons.profsum$att[order(meta.cons.profsum$mean, decreasing = TRUE)], las = 2, cex.axis = 1.7)
axis(2, at = seq(-1.6, 1.6, by = 0.4), labels = round(seq(-1.6, 1.6, by = 0.4), 2), las = 1, cex.axis = 1.7)
mtext("Meta-analytic Effect (Occupation = Male)", line = 4, side = 2, cex = 3)
abline(h = 0, lty = 2, col = "gray")
segments(x0 = c(1:dim(meta.cons.profsum)[1]), x1 = c(1:dim(meta.cons.profsum)[1]), 
         y0 = meta.cons.profsum$mean[order(meta.cons.profsum$mean, decreasing = TRUE)] - 1.96*meta.cons.profsum$se[order(meta.cons.profsum$mean, decreasing = TRUE)],
         y1 = meta.cons.profsum$mean[order(meta.cons.profsum$mean, decreasing = TRUE)] + 1.96*meta.cons.profsum$se[order(meta.cons.profsum$mean, decreasing = TRUE)],
         lty = 1, lwd = 2, col = "gray")
par(op)
dev.off()



## (12) Supplementary materials: All studies replicating on same corpora, with GloVe algorithm ----
## STUDY 1 ----
embdata.glove <- read.csv("embeddings_results_all_glovemain_071220.csv") # in .RData summary file
names(embdata.glove) <- c("dataset", "categories", "attributes",
                    "effsize", "miss", "pleft", "pright", "ptot",
                    "se", "coh_cat", "coh_att")

table(embdata.glove$categories)
embglovemain <- embdata.glove[embdata.glove$categories=="male vs. female" ,]
embglovemain <- embglovemain[grepl("vs.", embglovemain$attributes)==TRUE,]
embglovemain <- embglovemain[c("dataset", "categories", "attributes", "effsize", "se", "ptot")]
colnames(embglovemain) <- c("data", "cat", "att", "eff", "se", "p")

# To get all the expected effects in the positive direction...
# (a) Switch around the order of the labels for the double-difference scores
# The first attribute should be the one stereotypically-associated with males
# Should be "bad vs. good," "work vs. home," "math vs. reading," "science vs. arts" (already in the correct direction)
# (b) Then multiply the first four double-differences by -1
table(embglovemain$att)
embglovemain$att <- factor(embglovemain$att, labels = c("bad vs. good", "work vs. home", "math vs. reading", "science vs. arts"))
table(embglovemain$cat)
embglovemain$cat <- factor(embglovemain$cat, labels = c("male vs. female"))


# And correct the d-scores to align with positive = expected order.
# Multiply by -1 for all double-differences except instrumen/weapons and arts/science
embglovemain$eff.correct <- ifelse(grepl("science", embglovemain$att)==TRUE | grepl("weapons", embglovemain$cat)==TRUE, 
                             embglovemain$eff, embglovemain$eff*-1)
head(embglovemain)


# (12a) Meta analysis of all effect sizes
# Only keeping primary data (7 data sources)
table(embglovemain$data)
metadat.glovemain <- embglovemain[embglovemain$data=="childes_children" | embglovemain$data=="childes_parents" | embglovemain$data=="child_books" | embglovemain$data=="kids_tv_combined" | embglovemain$data=="adult_speech" | embglovemain$data=="gutenberg" | embglovemain$data=="simply_scripts",]

metadat.glovemain$data <- factor(metadat.glovemain$data)
table(metadat.glovemain$data)

# glovemain another factor for child-directed, child-produced, or adult-directed and produced
metadat.glovemain$datacat <- car::recode(metadat.glovemain$data, "'childes_parents' = 'childdir'; 'child_books' = 'childdir'; 
                          'kids_tv_combined' = 'childdir'; 'childes_children' = 'childpro';
                          'adult_speech' = 'adultpd'; 'gutenberg' = 'adultpd'; 'simply_scripts' = 'adultpd'")
table(metadat.glovemain$datacat)

# Perform meta-analyses for each attribute and category combination
# Meta-analysis for Male-Female Good-Bad
# vector of treatment effects
metaeff.glovemain.mfgb <- metadat.glovemain$eff.correct[grepl("male", metadat.glovemain$cat)==TRUE & grepl("good", metadat.glovemain$att)==TRUE]
metaeff.glovemain.se.mfgb <- metadat.glovemain$se[grepl("male", metadat.glovemain$cat)==TRUE & grepl("good", metadat.glovemain$att)==TRUE]
meta.glovemain.mfgb <- metagen(TE = metaeff.glovemain.mfgb, seTE = metaeff.glovemain.se.mfgb, sm = "MD") # using mean effect method
summary(meta.glovemain.mfgb)

# Meta-analysis for Male-Female Work-Home
# vector of treatment effects
metaeff.glovemain.mfwh <- metadat.glovemain$eff.correct[grepl("male", metadat.glovemain$cat)==TRUE & grepl("home", metadat.glovemain$att)==TRUE]
metaeff.glovemain.se.mfwh <- metadat.glovemain$se[grepl("male", metadat.glovemain$cat)==TRUE & grepl("home", metadat.glovemain$att)==TRUE]
meta.glovemain.mfwh <- metagen(TE = metaeff.glovemain.mfwh, seTE = metaeff.glovemain.se.mfwh, sm = "MD") # using mean effect method
summary(meta.glovemain.mfwh)

# Meta-analysis for Male-Female Science-Arts
# vector of treatment effects
metaeff.glovemain.mfsa <- metadat.glovemain$eff.correct[grepl("male", metadat.glovemain$cat)==TRUE & grepl("science", metadat.glovemain$att)==TRUE]
metaeff.glovemain.se.mfsa <- metadat.glovemain$se[grepl("male", metadat.glovemain$cat)==TRUE & grepl("science", metadat.glovemain$att)==TRUE]
meta.glovemain.mfsa <- metagen(TE = metaeff.glovemain.mfsa, seTE = metaeff.glovemain.se.mfsa, sm = "MD") # using mean effect method
summary(meta.glovemain.mfsa)

# Meta-analysis for Male-Female Math-Reading
# vector of treatment effects
metaeff.glovemain.mfmr <- metadat.glovemain$eff.correct[grepl("male", metadat.glovemain$cat)==TRUE & grepl("math", metadat.glovemain$att)==TRUE]
metaeff.glovemain.se.mfmr <- metadat.glovemain$se[grepl("male", metadat.glovemain$cat)==TRUE & grepl("math", metadat.glovemain$att)==TRUE]
meta.glovemain.mfmr <- metagen(TE = metaeff.glovemain.mfmr, seTE = metaeff.glovemain.se.mfmr, sm = "MD") # using mean effect method
summary(meta.glovemain.mfmr)


# Combine meta data (for fixed) 
metadat.glovemainsum <- as.data.frame(matrix(nrow = 5, ncol = 6))
colnames(metadat.glovemainsum) <- c("metamean", "metase", "meta95low", "meta95high", "metap", "attcat")
metadat.glovemainsum$metamean <- c(meta.glovemain.mfgb$TE.fixed, meta.glovemain.mfwh$TE.fixed, meta.glovemain.mfsa$TE.fixed, meta.glovemain.mfmr$TE.fixed, meta.instweap$TE.fixed)
metadat.glovemainsum$metase <- c(meta.glovemain.mfgb$seTE.fixed, meta.glovemain.mfwh$seTE.fixed, meta.glovemain.mfsa$seTE.fixed, meta.glovemain.mfmr$seTE.fixed, meta.instweap$seTE.fixed)
metadat.glovemainsum$meta95low <- c(meta.glovemain.mfgb$lower.fixed, meta.glovemain.mfwh$lower.fixed, meta.glovemain.mfsa$lower.fixed, 
                              meta.glovemain.mfmr$lower.fixed, meta.instweap$lower.fixed)
metadat.glovemainsum$meta95high <- c(meta.glovemain.mfgb$upper.fixed, meta.glovemain.mfwh$upper.fixed, meta.glovemain.mfsa$upper.fixed,
                               meta.glovemain.mfmr$upper.fixed, meta.instweap$upper.fixed)
metadat.glovemainsum$metap <- c(meta.glovemain.mfgb$pval.fixed, meta.glovemain.mfwh$pval.fixed, meta.glovemain.mfsa$pval.fixed, meta.glovemain.mfmr$pval.fixed, meta.instweap$pval.fixed)
metadat.glovemainsum$attcat <-c("mfgb", "mfwh", "mfsa", "mfmr", "iwgb")
# Overall meta-analysis summary
metadat.glovemainsum

## Meta-analysis across child-produced, child-directed, and adult-produced/directed corpora subsets
# Meta-analysis in child-directed corpora
table(metadat.glovemain$datacat)
metadat.glovemain.childdir <- metadat.glovemain[metadat.glovemain$datacat=="childdir",]

# For Male-Female Good-Bad 
metaeff.glovemain.cd.mfgb <- metadat.glovemain.childdir$eff.correct[grepl("male", metadat.glovemain.childdir$cat)==TRUE & grepl("good", metadat.glovemain.childdir$att)==TRUE]
metaeff.glovemain.cd.se.mfgb <- metadat.glovemain.childdir$se[grepl("male", metadat.glovemain.childdir$cat)==TRUE & grepl("good", metadat.glovemain.childdir$att)==TRUE]
meta.glovemaincd.mfgb <- metagen(TE = metaeff.glovemain.cd.mfgb, seTE = metaeff.glovemain.cd.se.mfgb, sm = "MD") # using mean effect method
summary(meta.glovemaincd.mfgb)
meta.glovemaincd.mfgb$seTE.fixed

# For Work-Home
metaeff.glovemain.cd.mfwh <- metadat.glovemain.childdir$eff.correct[grepl("male", metadat.glovemain.childdir$cat)==TRUE & grepl("work", metadat.glovemain.childdir$att)==TRUE]
metaeff.glovemain.cd.se.mfwh <- metadat.glovemain.childdir$se[grepl("male", metadat.glovemain.childdir$cat)==TRUE & grepl("work", metadat.glovemain.childdir$att)==TRUE]
meta.glovemaincd.mfwh <- metagen(TE = metaeff.glovemain.cd.mfwh, seTE = metaeff.glovemain.cd.se.mfwh, sm = "MD") # using mean effect method
summary(meta.glovemaincd.mfwh)
meta.glovemaincd.mfwh$seTE.fixed

# For Science-Arts
metaeff.glovemain.cd.mfsa <- metadat.glovemain.childdir$eff.correct[grepl("male", metadat.glovemain.childdir$cat)==TRUE & grepl("arts", metadat.glovemain.childdir$att)==TRUE]
metaeff.glovemain.cd.se.mfsa <- metadat.glovemain.childdir$se[grepl("male", metadat.glovemain.childdir$cat)==TRUE & grepl("arts", metadat.glovemain.childdir$att)==TRUE]
meta.glovemaincd.mfsa <- metagen(TE = metaeff.glovemain.cd.mfsa, seTE = metaeff.glovemain.cd.se.mfsa, sm = "MD") # using mean effect method
summary(meta.glovemaincd.mfsa)
meta.glovemaincd.mfsa$seTE.fixed

# For Reading-Math
metaeff.glovemain.cd.mfrm <- metadat.glovemain.childdir$eff.correct[grepl("male", metadat.glovemain.childdir$cat)==TRUE & grepl("reading", metadat.glovemain.childdir$att)==TRUE]
metaeff.glovemain.cd.se.mfrm <- metadat.glovemain.childdir$se[grepl("male", metadat.glovemain.childdir$cat)==TRUE & grepl("reading", metadat.glovemain.childdir$att)==TRUE]
meta.glovemaincd.mfrm <- metagen(TE = metaeff.glovemain.cd.mfrm, seTE = metaeff.glovemain.cd.se.mfrm, sm = "MD") # using mean effect method
summary(meta.glovemaincd.mfrm)
meta.glovemaincd.mfrm$seTE.fixed


# Meta-analysis in adult-directed corpora
table(metadat.glovemain$datacat)
metadat.glovemain.adultdp <- metadat.glovemain[metadat.glovemain$datacat=="adultpd",]

# For Male-Female Good-Bad 
metaeff.glovemain.adp.mfgb <- metadat.glovemain.adultdp$eff.correct[grepl("male", metadat.glovemain.adultdp$cat)==TRUE & grepl("good", metadat.glovemain.adultdp$att)==TRUE]
metaeff.glovemain.adp.se.mfgb <- metadat.glovemain.adultdp$se[grepl("male", metadat.glovemain.adultdp$cat)==TRUE & grepl("good", metadat.glovemain.adultdp$att)==TRUE]
meta.glovemainadp.mfgb <- metagen(TE = metaeff.glovemain.adp.mfgb, seTE = metaeff.glovemain.adp.se.mfgb, sm = "MD") # using mean effect method
summary(meta.glovemainadp.mfgb)
meta.glovemainadp.mfgb$seTE.fixed

# For Work-Home
metaeff.glovemain.adp.mfwh <- metadat.glovemain.adultdp$eff.correct[grepl("male", metadat.glovemain.adultdp$cat)==TRUE & grepl("work", metadat.glovemain.adultdp$att)==TRUE]
metaeff.glovemain.adp.se.mfwh <- metadat.glovemain.adultdp$se[grepl("male", metadat.glovemain.adultdp$cat)==TRUE & grepl("work", metadat.glovemain.adultdp$att)==TRUE]
meta.glovemainadp.mfwh <- metagen(TE = metaeff.glovemain.adp.mfwh, seTE = metaeff.glovemain.adp.se.mfwh, sm = "MD") # using mean effect method
summary(meta.glovemainadp.mfwh)
meta.glovemainadp.mfwh$seTE.fixed

# For Science-Arts
metaeff.glovemain.adp.mfsa <- metadat.glovemain.adultdp$eff.correct[grepl("male", metadat.glovemain.adultdp$cat)==TRUE & grepl("arts", metadat.glovemain.adultdp$att)==TRUE]
metaeff.glovemain.adp.se.mfsa <- metadat.glovemain.adultdp$se[grepl("male", metadat.glovemain.adultdp$cat)==TRUE & grepl("arts", metadat.glovemain.adultdp$att)==TRUE]
meta.glovemainadp.mfsa <- metagen(TE = metaeff.glovemain.adp.mfsa, seTE = metaeff.glovemain.adp.se.mfsa, sm = "MD") # using mean effect method
summary(meta.glovemainadp.mfsa)
meta.glovemainadp.mfsa$seTE.fixed

# For Reading-Math
metaeff.glovemain.adp.mfrm <- metadat.glovemain.adultdp$eff.correct[grepl("male", metadat.glovemain.adultdp$cat)==TRUE & grepl("reading", metadat.glovemain.adultdp$att)==TRUE]
metaeff.glovemain.adp.se.mfrm <- metadat.glovemain.adultdp$se[grepl("male", metadat.glovemain.adultdp$cat)==TRUE & grepl("reading", metadat.glovemain.adultdp$att)==TRUE]
meta.glovemainadp.mfrm <- metagen(TE = metaeff.glovemain.adp.mfrm, seTE = metaeff.glovemain.adp.se.mfrm, sm = "MD") # using mean effect method
summary(meta.glovemainadp.mfrm)
meta.glovemainadp.mfrm$seTE.fixed

# Note: Results for each corpus/stereotype combination meta-analysis are reported in Table 1 in the main text.

## Meta-analysis across all stereotypes and across child-produced, child-directed, and adult-produced/directed corpora subsets
# Meta-analysis in all stereotypes and all corpora
metaeff.glovemain.all <- metadat.glovemain$eff.correct[metadat.glovemain$cat != "weapons vs. instruments"]
metaeff.glovemain.se.all <- metadat.glovemain$se[metadat.glovemain$cat != "weapons vs. instruments"]
meta.glovemainall <- metagen(TE = metaeff.glovemain.all, seTE = metaeff.glovemain.se.all, sm = "MD") # using mean effect method
summary(meta.glovemainall)
meta.glovemainall$seTE.fixed

# Across child-produced
metaeff.glovemain.chprod <- metadat.glovemain$eff.correct[metadat.glovemain$data=="childes_children" & metadat.glovemain$cat != "weapons vs. instruments"]
metaeff.glovemain.se.chprod <- metadat.glovemain$se[metadat.glovemain$data=="childes_children" & metadat.glovemain$cat != "weapons vs. instruments"]
meta.glovemainchprod <- metagen(TE = metaeff.glovemain.chprod, seTE = metaeff.glovemain.se.chprod, sm = "MD") # using mean effect method
summary(meta.glovemainchprod)
meta.glovemainchprod$seTE.fixed
# Across child-directed
metadat.glovemain.childdir <- metadat.glovemain[metadat.glovemain$datacat=="childdir",]
metaeff.glovemain.chdir <- metadat.glovemain.childdir$eff.correct[metadat.glovemain.childdir$cat != "weapons vs. instruments"]
metaeff.glovemain.se.chdir <-metadat.glovemain.childdir$se[metadat.glovemain.childdir$cat != "weapons vs. instruments"]
meta.glovemainchdir <- metagen(TE = metaeff.glovemain.chdir, seTE = metaeff.glovemain.se.chdir, sm = "MD") # using mean effect method
summary(meta.glovemainchdir)
meta.glovemainchdir$seTE.fixed
# Across adult-directed
metadat.glovemain.adultpd <- metadat.glovemain[metadat.glovemain$datacat=="adultpd",]
metaeff.glovemain.adultpd <- metadat.glovemain.adultpd$eff.correct[metadat.glovemain.adultpd$cat != "weapons vs. instruments"]
metaeff.glovemain.se.adultpd <-metadat.glovemain.adultpd$se[metadat.glovemain.adultpd$cat != "weapons vs. instruments"]
meta.glovemainadultpd <- metagen(TE = metaeff.glovemain.adultpd, seTE = metaeff.glovemain.se.adultpd, sm = "MD") # using mean effect method
summary(meta.glovemainadultpd)
meta.glovemainadultpd$seTE.fixed


# Child-directed speech
metaeff.glovemain.chdirspeech <- metadat.glovemain$eff.correct[metadat.glovemain$data=="childes_parents" & metadat.glovemain$cat != "weapons vs. instruments"]
metaeff.glovemain.se.chdirspeech <- metadat.glovemain$se[metadat.glovemain$data=="childes_parents" & metadat.glovemain$cat != "weapons vs. instruments"]
meta.glovemainchdirspeech <- metagen(TE = metaeff.glovemain.chdirspeech, seTE = metaeff.glovemain.se.chdirspeech, sm = "MD") # using mean effect method
summary(meta.glovemainchdirspeech)
meta.glovemainchdirspeech$seTE.fixed
# Child-directed books
metaeff.glovemain.chbooks <- metadat.glovemain$eff.correct[metadat.glovemain$data=="child_books" & metadat.glovemain$cat != "weapons vs. instruments"]
metaeff.glovemain.se.chbooks <- metadat.glovemain$se[metadat.glovemain$data=="child_books" & metadat.glovemain$cat != "weapons vs. instruments"]
meta.glovemainchbooks <- metagen(TE = metaeff.glovemain.chbooks, seTE = metaeff.glovemain.se.chbooks, sm = "MD") # using mean effect method
summary(meta.glovemainchbooks)
meta.glovemainchbooks$seTE.fixed
# Child-directed TV
metaeff.glovemain.chtv <- metadat.glovemain$eff.correct[metadat.glovemain$data=="kids_tv_combined" & metadat.glovemain$cat != "weapons vs. instruments"]
metaeff.glovemain.se.chtv <- metadat.glovemain$se[metadat.glovemain$data=="kids_tv_combined" & metadat.glovemain$cat != "weapons vs. instruments"]
meta.glovemainchtv <- metagen(TE = metaeff.glovemain.chtv, seTE = metaeff.glovemain.se.chtv, sm = "MD") # using mean effect method
summary(meta.glovemainchtv)
meta.glovemainchtv$seTE.fixed
# Adult speech
metaeff.glovemain.adspeech <- metadat.glovemain$eff.correct[metadat.glovemain$data=="adult_speech" & metadat.glovemain$cat != "weapons vs. instruments"]
metaeff.glovemain.se.adspeech <- metadat.glovemain$se[metadat.glovemain$data=="adult_speech" & metadat.glovemain$cat != "weapons vs. instruments"]
meta.glovemainadspeech <- metagen(TE = metaeff.glovemain.adspeech, seTE = metaeff.glovemain.se.adspeech, sm = "MD") # using mean effect method
summary(meta.glovemainadspeech)
meta.glovemainadspeech$seTE.fixed
# Adult books
metaeff.glovemain.adbooks <- metadat.glovemain$eff.correct[metadat.glovemain$data=="gutenberg" & metadat.glovemain$cat != "weapons vs. instruments"]
metaeff.glovemain.se.adbooks <- metadat.glovemain$se[metadat.glovemain$data=="gutenberg" & metadat.glovemain$cat != "weapons vs. instruments"]
meta.glovemainadbooks <- metagen(TE = metaeff.glovemain.adbooks, seTE = metaeff.glovemain.se.adbooks, sm = "MD") # using mean effect method
summary(meta.glovemainadbooks)
meta.glovemainadbooks$seTE.fixed
# Adult TV
metaeff.glovemain.adtv <- metadat.glovemain$eff.correct[metadat.glovemain$data=="simply_scripts" & metadat.glovemain$cat != "weapons vs. instruments"]
metaeff.glovemain.se.adtv <- metadat.glovemain$se[metadat.glovemain$data=="simply_scripts" & metadat.glovemain$cat != "weapons vs. instruments"]
meta.glovemainadtv <- metagen(TE = metaeff.glovemain.adtv, seTE = metaeff.glovemain.se.adtv, sm = "MD") # using mean effect method
summary(meta.glovemainadtv)
meta.glovemainadtv$seTE.fixed

## To get a print-out of the remaining results, pull them out by corpus
metadat.glovemain[metadat.glovemain$data=="childes_children",c("att", "eff.correct", "se", "p")]
metadat.glovemain[metadat.glovemain$data=="childes_parents",c("att", "eff.correct", "se", "p")]
metadat.glovemain[metadat.glovemain$data=="child_books",c("att", "eff.correct", "se", "p")]
metadat.glovemain[metadat.glovemain$data=="kids_tv_combined",c("att", "eff.correct", "se", "p")]
metadat.glovemain[metadat.glovemain$data=="adult_speech",c("att", "eff.correct", "se", "p")]
metadat.glovemain[metadat.glovemain$data=="gutenberg",c("att", "eff.correct", "se", "p")]
metadat.glovemain[metadat.glovemain$data=="simply_scripts",c("att", "eff.correct", "se", "p")]

## Correlations across algorithms
cor.test(metadat2$eff.correct, metadat.glovemain$eff.correct) # marginally significant



## Plotting ----
metadat.glovemainsum2 <- metadat.glovemainsum[1:4,]
metadat.glovemainsum2 # mfgb, mfwh, mfsa, mfmr
metadat.glovemain.plot <- metadat.glovemain[metadat.glovemain$cat != "weapons vs. instruments",] # Remove instruments/weapons for main plot
metadat.glovemain.plot$att2 <- factor(metadat.glovemain.plot$att, 
                                levels = c("bad vs. good", "work vs. home", "science vs. arts", "math vs. reading"))
metadat.glovemain.plot <- arrange(metadat.glovemain.plot, att2)


## w/ Individual corpora data
pdf(file = "presentplotall.glovemain.pdf", width = 12, height = 10)
op <- par(mar = c(5,6,2,2))
plot(1, type="n", xlab="", ylab="", 
     xlim=c(0.5, 4.5), ylim=c(-2.25, 2.25), axes = FALSE)
box()
mtext("Effect Size", side = 2, line = 3, cex = 2)
axis(1, at = c(1:4), labels = c("Male-Bad\nFemale-Good", "Male-Work\nFemale-Home", 
                                "Male-Science\nFemale-Arts", "Male-Math\nFemale-Reading"),
     cex.axis = 1.4, padj = 1)
axis(2, at = seq(-2,2, by = 0.5), labels = seq(-2,2, by = 0.5), las = 2, cex.axis = 1.5)
abline(h = 0, lty = 2)

points(seq(1, 4, by = 1),
       metadat.glovemainsum2$metamean, 
       pch = 17, cex = 3, col = "red")
segments(x0 = seq(1, 4, by = 1), x1 = seq(1, 4, by = 1),
         y0 = metadat.glovemainsum2$meta95low,
         y1 = metadat.glovemainsum2$meta95high, 
         lty = 1, lwd = 3, col = "red")

points(seq(0.8, 3.8, by = 1),
       metadat.glovemain.plot$eff.correct[metadat.glovemain.plot$data=="childes_children"], 
       pch = 18, cex = 3, col = "chartreuse3")
segments(x0 = seq(0.8, 3.8, by = 1), x1 = seq(0.8, 3.8, by = 1),
         y0 = metadat.glovemain.plot$eff.correct[metadat.glovemain.plot$data=="childes_children"] - 1.96*metadat.glovemain.plot$se[metadat.glovemain.plot$data=="childes_children"],
         y1 = metadat.glovemain.plot$eff.correct[metadat.glovemain.plot$data=="childes_children"] + 1.96*metadat.glovemain.plot$se[metadat.glovemain.plot$data=="childes_children"], 
         lty = 1, lwd = 3, col = "chartreuse3")

points(seq(0.85, 3.85, by = 1),
       metadat.glovemain.plot$eff.correct[metadat.glovemain.plot$data=="childes_parents"], 
       pch = 19, cex = 2, col = "cyan2")
segments(x0 = seq(0.85, 3.85, by = 1), x1 = seq(0.85, 3.85, by = 1),
         y0 = metadat.glovemain.plot$eff.correct[metadat.glovemain.plot$data=="childes_parents"] - 1.96*metadat.glovemain.plot$se[metadat.glovemain.plot$data=="childes_parents"],
         y1 = metadat.glovemain.plot$eff.correct[metadat.glovemain.plot$data=="childes_parents"] + 1.96*metadat.glovemain.plot$se[metadat.glovemain.plot$data=="childes_parents"], 
         lty = 1, lwd = 3, col = "cyan2")
points(seq(0.9, 3.9, by = 1),
       metadat.glovemain.plot$eff.correct[metadat.glovemain.plot$data=="child_books"], 
       pch = 19, cex = 2, col = "cadetblue")
segments(x0 = seq(0.9, 3.9, by = 1), x1 = seq(0.9, 3.9, by = 1),
         y0 = metadat.glovemain.plot$eff.correct[metadat.glovemain.plot$data=="child_books"] - 1.96*metadat.glovemain.plot$se[metadat.glovemain.plot$data=="child_books"],
         y1 = metadat.glovemain.plot$eff.correct[metadat.glovemain.plot$data=="child_books"] + 1.96*metadat.glovemain.plot$se[metadat.glovemain.plot$data=="child_books"], 
         lty = 1, lwd = 3, col = "cadetblue")
points(seq(0.95, 3.95, by = 1),
       metadat.glovemain.plot$eff.correct[metadat.glovemain.plot$data=="kids_tv_combined"], 
       pch = 19, cex = 2, col = "darkblue")
segments(x0 = seq(0.95, 3.95, by = 1), x1 = seq(0.95, 3.95, by = 1),
         y0 = metadat.glovemain.plot$eff.correct[metadat.glovemain.plot$data=="kids_tv_combined"] - 1.96*metadat.glovemain.plot$se[metadat.glovemain.plot$data=="kids_tv_combined"],
         y1 = metadat.glovemain.plot$eff.correct[metadat.glovemain.plot$data=="kids_tv_combined"] + 1.96*metadat.glovemain.plot$se[metadat.glovemain.plot$data=="kids_tv_combined"], 
         lty = 1, lwd = 3, col = "darkblue")

points(seq(1.05, 4.05, by = 1),
       metadat.glovemain.plot$eff.correct[metadat.glovemain.plot$data=="adult_speech"], 
       pch = 15, cex = 2, col = "gold1")
segments(x0 = seq(1.05, 4.05, by = 1), x1 = seq(1.05, 4.05, by = 1),
         y0 = metadat.glovemain.plot$eff.correct[metadat.glovemain.plot$data=="adult_speech"] - 1.96*metadat.glovemain.plot$se[metadat.glovemain.plot$data=="adult_speech"],
         y1 = metadat.glovemain.plot$eff.correct[metadat.glovemain.plot$data=="adult_speech"] + 1.96*metadat.glovemain.plot$se[metadat.glovemain.plot$data=="adult_speech"], 
         lty = 1, lwd = 3, col = "gold1")
points(seq(1.1, 4.1, by = 1),
       metadat.glovemain.plot$eff.correct[metadat.glovemain.plot$data=="gutenberg"], 
       pch = 15, cex = 2, col = "darkorange")
segments(x0 = seq(1.1, 4.1, by = 1), x1 = seq(1.1, 4.1, by = 1),
         y0 = metadat.glovemain.plot$eff.correct[metadat.glovemain.plot$data=="gutenberg"] - 1.96*metadat.glovemain.plot$se[metadat.glovemain.plot$data=="gutenberg"],
         y1 = metadat.glovemain.plot$eff.correct[metadat.glovemain.plot$data=="gutenberg"] + 1.96*metadat.glovemain.plot$se[metadat.glovemain.plot$data=="gutenberg"], 
         lty = 1, lwd = 3, col = "darkorange")
points(seq(1.15, 4.15, by = 1),
       metadat.glovemain.plot$eff.correct[metadat.glovemain.plot$data=="simply_scripts"], 
       pch = 15, cex = 2, col = "plum2")
segments(x0 = seq(1.15, 4.15, by = 1), x1 = seq(1.15, 4.15, by = 1),
         y0 = metadat.glovemain.plot$eff.correct[metadat.glovemain.plot$data=="simply_scripts"] - 1.96*metadat.glovemain.plot$se[metadat.glovemain.plot$data=="simply_scripts"],
         y1 = metadat.glovemain.plot$eff.correct[metadat.glovemain.plot$data=="simply_scripts"] + 1.96*metadat.glovemain.plot$se[metadat.glovemain.plot$data=="simply_scripts"], 
         lty = 1, lwd = 3, col = "plum2")


legend("bottomleft", c("Child-produced speech", "Child-directed speech",
                       "Child-directed books", "Child-directed media",
                       "Adult-produced speech", "Adult-directed books", 
                       "Adult-directed media", "Meta Estimate"),
       col = c("chartreuse3", "cyan2", "cadetblue", "darkblue",
               "gold1", "darkorange", "plum2", "red"),
       pch = c(18, rep(19, 3), rep(15, 3), 17), 
       cex = 2, pt.cex = c(3, rep(2.5, 6), 3), bty = "n", ncol = 2)

par(op)
dev.off()


## STUDY 2 -----
# Read in all SC-WEAT
# Note: using single trait words only (not trait synonyms)
traitdat.glove <- read.csv("sm_embeddings_singletraits_glove_071220.csv") # Also in .RData
names(traitdat.glove) <- c("data", "categories", "attributes", 
                     "effsize", "miss", "pleft", "pright", "ptot", "se", "cohesion")
length(table(traitdat.glove$attributes)) # currently = 541 traits in at least one corpus

# Make new data frame with only 7 primary corpora
traitdat.glovesub <- traitdat.glove[traitdat.glove$data=="childes_children" | traitdat.glove$data=="childes_parents" | traitdat.glove$data=="child_books" | traitdat.glove$data=="kids_tv_combined" | traitdat.glove$data=="adult_speech" | traitdat.glove$data=="gutenberg" | traitdat.glove$data=="simply_scripts",]

traitdat.glovesub$attributes <- factor(traitdat.glovesub$attributes)
traitdat.glovesub$data <- factor(traitdat.glovesub$data)
length(table(traitdat.glovesub$attributes)) # now = 541 traits in at least one corpus

# drop factors with < 5 obs 
traitdat.glovenew <- traitdat.glovesub[!(as.numeric(traitdat.glovesub$attributes) %in% which(table(traitdat.glovesub$attributes) < 5)),]
traitdat.glovenew$attributes <- factor(traitdat.glovenew$attributes)
length(table(traitdat.glovenew$attributes)) # now = 170 traits
table(traitdat.glovenew$data) # variability across corpora in the number of traits that are actually present
traitdat.glovenew <- traitdat.glovenew[c("data", "categories", "attributes", "effsize", "se", "ptot")]
colnames(traitdat.glovenew) <- c("data", "cat", "att", "eff", "se", "p")

# Meta analysis of effect sizes for each attribute (170 traits)
table(traitdat.glovenew$att)
traitdat.glovenew$attnum <- as.numeric(traitdat.glovenew$att)
table(traitdat.glovenew$att)
table(traitdat.glovenew$attnum)

meta.glove.alltrait <- list()
meta.glove.traitmeans <- vector()
meta.glove.traitse <- vector()
meta.glove.traitp <- vector()

for (i in 1:length(table(traitdat.glovenew$att))){
  meta.glove.alltrait[[i]] <- metagen(TE = traitdat.glovenew$eff[traitdat.glovenew$attnum==i], seTE = traitdat.glovenew$se[traitdat.glovenew$attnum==i], sm = "MD")
  meta.glove.traitmeans[i] <- meta.glove.alltrait[[i]]$TE.fixed
  meta.glove.traitse[i] <- meta.glove.alltrait[[i]]$seTE.fixed
  meta.glove.traitp[i] <- meta.glove.alltrait[[i]]$pval.fixed
}

names(meta.glove.alltrait) <- levels(traitdat.glovenew$att)
meta.glove.traitsum <- as.data.frame(meta.glove.traitmeans)
colnames(meta.glove.traitsum) <- "mean"
meta.glove.traitsum$se <- meta.glove.traitse
meta.glove.traitsum$p <- meta.glove.traitp
meta.glove.traitsum$att <- levels(traitdat.glovenew$att)

# now add on vectors for each sub database
table(traitdat.glovenew$data)
meta.glove.traitsum2 <- merge(meta.glove.traitsum, 
                        traitdat.glovenew[traitdat.glovenew$data=="childes_children", 
                                    c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.glove.traitsum2)[5:6] <- c("effchchild", "sechchild")
meta.glove.traitsum2 <- merge(meta.glove.traitsum2, 
                        traitdat.glovenew[traitdat.glovenew$data=="childes_parents", 
                                    c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.glove.traitsum2)[7:8] <- c("effchpar", "sechpar")
meta.glove.traitsum2 <- merge(meta.glove.traitsum2, 
                        traitdat.glovenew[traitdat.glovenew$data=="child_books", 
                                    c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.glove.traitsum2)[9:10] <- c("effchbook", "sechbook")
meta.glove.traitsum2 <- merge(meta.glove.traitsum2, 
                        traitdat.glovenew[traitdat.glovenew$data=="gutenberg", 
                                    c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.glove.traitsum2)[11:12] <- c("effguten", "seguten")
meta.glove.traitsum2 <- merge(meta.glove.traitsum2, 
                        traitdat.glovenew[traitdat.glovenew$data=="kids_tv_combined", 
                                    c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.glove.traitsum2)[13:14] <- c("effchtv", "sechtv")
meta.glove.traitsum2 <- merge(meta.glove.traitsum2, 
                        traitdat.glovenew[traitdat.glovenew$data=="simply_scripts", 
                                    c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.glove.traitsum2)[15:16] <- c("effadtv", "seadtv")
meta.glove.traitsum2 <- merge(meta.glove.traitsum2, 
                        traitdat.glovenew[traitdat.glovenew$data=="adult_speech", 
                                    c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.glove.traitsum2)[17:18] <- c("effadspch", "seadspch")


## Descriptives of meta-analytic data 
# Overall data
mean(meta.glove.traitsum2$mean)
range(meta.glove.traitsum2$mean) 
head(meta.glove.traitsum2$att[order(meta.glove.traitsum2$mean, decreasing = TRUE)])
head(meta.glove.traitsum2$att[order(meta.glove.traitsum2$mean, decreasing = FALSE)])

## How many are significant?
length(which(meta.glove.traitsum2$p < .05))
meta.glove.traitsum2$sig <- ifelse(meta.glove.traitsum2$p < .05, "sig", "nsig")
table(meta.glove.traitsum2$sig)
binom.test(table(meta.glove.traitsum2$sig)[2],
           dim(meta.glove.traitsum2)[1]) 
meta.glove.traitsum2$sig.mf <- ifelse(meta.glove.traitsum2$p < .05 & meta.glove.traitsum2$mean < 0, "sig.f", 
                                ifelse(meta.glove.traitsum2$p < .05 & meta.glove.traitsum2$mean > 0, "sig.m", "nsig"))
table(meta.glove.traitsum2$sig.mf)/dim(meta.glove.traitsum2)[1]


## How many fall beyond [-0.1, 0.1]?
meta.glove.traitsum2$effcut <- cut(meta.glove.traitsum2$mean, breaks = c(-2, -0.1, 0.1, 2), labels = c("sig.f", "nsig", "sig.m"))
table(meta.glove.traitsum2$effcut) 
table(meta.glove.traitsum2$effcut)/dim(meta.glove.traitsum2)[1] 
binom.test(sum(table(meta.glove.traitsum2$effcut)[c(1, 3)]),
           dim(meta.glove.traitsum2)[1]) 

## How many fall beyond [-0.2, 0.2]?
meta.glove.traitsum2$effcut2 <- cut(meta.glove.traitsum2$mean, breaks = c(-2, -0.2, 0.2, 2), labels = c("sig.f", "nsig", "sig.m"))
table(meta.glove.traitsum2$effcut2) 
table(meta.glove.traitsum2$effcut2)/dim(meta.glove.traitsum2)[1] 
binom.test(sum(table(meta.glove.traitsum2$effcut2)[c(1, 3)]),
           dim(meta.glove.traitsum2)[1])

## How many fall beyond [-0.3, 0.3]?
meta.glove.traitsum2$effcut3 <- cut(meta.glove.traitsum2$mean, breaks = c(-2, -0.3, 0.3, 2), labels = c("sig.f", "nsig", "sig.m"))
table(meta.glove.traitsum2$effcut3) 
table(meta.glove.traitsum2$effcut3)/dim(meta.glove.traitsum2)[1] 
binom.test(sum(table(meta.glove.traitsum2$effcut3)[c(1, 3)]),
           dim(meta.glove.traitsum2)[1]) 


# Within child-produced speech
mean(meta.glove.traitsum2$effchchild, na.rm = TRUE)
t.test(meta.glove.traitsum2$effchchild, mu = 0)
head(meta.glove.traitsum2$att[order(meta.glove.traitsum2$effchchild, decreasing = TRUE)])
head(meta.glove.traitsum2$att[order(meta.glove.traitsum2$effchchild, decreasing = FALSE)])
# Within child-directed speech
mean(meta.glove.traitsum2$effchpar, na.rm = TRUE)
t.test(meta.glove.traitsum2$effchpar, mu = 0)
head(meta.glove.traitsum2$att[order(meta.glove.traitsum2$effchpar, decreasing = TRUE)])
head(meta.glove.traitsum2$att[order(meta.glove.traitsum2$effchpar, decreasing = FALSE)])
# Within child-directed books
mean(meta.glove.traitsum2$effchbook, na.rm = TRUE)
t.test(meta.glove.traitsum2$effchbook, mu = 0)
head(meta.glove.traitsum2$att[order(meta.glove.traitsum2$effchbook, decreasing = TRUE)])
head(meta.glove.traitsum2$att[order(meta.glove.traitsum2$effchbook, decreasing = FALSE)])
# Within child-directed TV
mean(meta.glove.traitsum2$effchtv, na.rm = TRUE)
t.test(meta.glove.traitsum2$effchtv, mu = 0)
head(meta.glove.traitsum2$att[order(meta.glove.traitsum2$effchtv, decreasing = TRUE)])
head(meta.glove.traitsum2$att[order(meta.glove.traitsum2$effchtv, decreasing = FALSE)])
# Within adult-directed speech
mean(meta.glove.traitsum2$effadspch, na.rm = TRUE)
t.test(meta.glove.traitsum2$effadspch, mu = 0)
head(meta.glove.traitsum2$att[order(meta.glove.traitsum2$effadspch, decreasing = TRUE)])
head(meta.glove.traitsum2$att[order(meta.glove.traitsum2$effadspch, decreasing = FALSE)])
# Within adult-directed books
mean(meta.glove.traitsum2$effguten, na.rm = TRUE)
t.test(meta.glove.traitsum2$effguten, mu = 0)
head(meta.glove.traitsum2$att[order(meta.glove.traitsum2$effguten, decreasing = TRUE)])
head(meta.glove.traitsum2$att[order(meta.glove.traitsum2$effguten, decreasing = FALSE)])
# Within adult-directed tv
mean(meta.glove.traitsum2$effadtv, na.rm = TRUE)
t.test(meta.glove.traitsum2$effadtv, mu = 0)
head(meta.glove.traitsum2$att[order(meta.glove.traitsum2$effadtv, decreasing = TRUE)])
head(meta.glove.traitsum2$att[order(meta.glove.traitsum2$effadtv, decreasing = FALSE)])

## Plotting ----

pdf(file = "metatrait_glove.pdf", width = 35, height = 20)
op <- par(mar = c(12, 8, 2, 2))
plot(meta.glove.traitsum$mean[order(meta.glove.traitsum$mean, decreasing = TRUE)], ylim = c(-1.2, 0.8),
     xlab = "", ylab = "", axes = FALSE,
     pch = 20, cex = 2, col = "gray")
box()
axis(1, at = 1:177, 
     labels = meta.glove.traitsum$att[order(meta.glove.traitsum$mean, decreasing = TRUE)], las = 2, cex.axis = 1.7)
axis(2, at = seq(-1.4, 1.4, by = 0.2), labels = round(seq(-1.4, 1.4, by = 0.2), 2), las = 1, cex.axis = 1.7)
mtext("Meta-analytic Effect (Trait = Male)", line = 4, side = 2, cex = 3)
abline(h = 0, lty = 2, col = "darkgray", lwd = 3)
segments(x0 = c(1:177), x1 = c(1:177), 
         y0 = meta.glove.traitsum$mean[order(meta.glove.traitsum$mean, decreasing = TRUE)] - 1.96*meta.glove.traitsum$se[order(meta.glove.traitsum$mean, decreasing = TRUE)],
         y1 = meta.glove.traitsum$mean[order(meta.glove.traitsum$mean, decreasing = TRUE)] + 1.96*meta.glove.traitsum$se[order(meta.glove.traitsum$mean, decreasing = TRUE)],
         lty = 1, lwd = 2, col = "gray")
par(op)
dev.off()


## STUDY 3 ----
profdat.glove <- read.csv("sm_embeddings_singleprofs_glove_071220.csv") # Also in .RData
names(profdat.glove) <- c("data", "categories", "attributes",
                    "effsize", "miss", "pleft", "pright", "ptot", "se", "cohesion")
length(table(profdat.glove$attributes)) # currently = 82 profs in at least one corpus

# Make new data frame with only 7 primary corpora
profdat.glovesub <- profdat.glove[profdat.glove$data=="childes_children" | profdat.glove$data=="childes_parents" | profdat.glove$data=="child_books" | profdat.glove$data=="kids_tv_combined" | profdat.glove$data=="adult_speech" | profdat.glove$data=="gutenberg" | profdat.glove$data=="simply_scripts",]
profdat.glovesub$attributes <- factor(profdat.glovesub$attributes)
profdat.glovesub$data <- factor(profdat.glovesub$data)
length(table(profdat.glovesub$attributes)) # now = 82 profs in at least one corpus
profdat.glovenew <- profdat.glovesub[c("data", "categories", "attributes", "effsize", "se", "ptot")]
colnames(profdat.glovenew) <- c("data", "cat", "att", "eff", "se", "p")

# Meta analysis of effect sizes for each attribute (82 profs) 
table(profdat.glovenew$att)
table(profdat.glovesub$att)
profdat.glovenew$attnum <- as.numeric(profdat.glovenew$att)
table(profdat.glovenew$att)
table(profdat.glovenew$attnum)

meta.glove.allprof <- list()
meta.glove.profmeans <- vector()
meta.glove.profse <- vector()
meta.glove.profp <- vector()

for (i in 1:length(table(profdat.glovenew$att))){
  meta.glove.allprof[[i]] <- metagen(TE = profdat.glovenew$eff[profdat.glovenew$attnum==i], seTE = profdat.glovenew$se[profdat.glovenew$attnum==i], sm = "MD")
  meta.glove.profmeans[i] <- meta.glove.allprof[[i]]$TE.fixed
  meta.glove.profse[i] <- meta.glove.allprof[[i]]$seTE.fixed
  meta.glove.profp[i] <- meta.glove.allprof[[i]]$pval.fixed
}

names(meta.glove.allprof) <- levels(profdat.glovenew$att)
meta.glove.profsum <- as.data.frame(meta.glove.profmeans)
colnames(meta.glove.profsum) <- "mean"
meta.glove.profsum$se <- meta.glove.profse
meta.glove.profsum$p <- meta.glove.profp
meta.glove.profsum$att <- levels(profdat.glovenew$att)

# now add on vectors for each sub database
table(profdat.glovenew$data)
meta.glove.profsum2 <- merge(meta.glove.profsum, 
                       profdat.glovenew[profdat.glovenew$data=="childes_children", 
                                  c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.glove.profsum2)[5:6] <- c("effchchild", "sechchild")
meta.glove.profsum2 <- merge(meta.glove.profsum2, 
                       profdat.glovenew[profdat.glovenew$data=="childes_parents", 
                                  c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.glove.profsum2)[7:8] <- c("effchpar", "sechpar")
meta.glove.profsum2 <- merge(meta.glove.profsum2, 
                       profdat.glovenew[profdat.glovenew$data=="child_books", 
                                  c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.glove.profsum2)[9:10] <- c("effchbook", "sechbook")
meta.glove.profsum2 <- merge(meta.glove.profsum2, 
                       profdat.glovenew[profdat.glovenew$data=="gutenberg", 
                                  c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.glove.profsum2)[11:12] <- c("effguten", "seguten")
meta.glove.profsum2 <- merge(meta.glove.profsum2, 
                       profdat.glovenew[profdat.glovenew$data=="kids_tv_combined", 
                                  c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.glove.profsum2)[13:14] <- c("effchtv", "sechtv")
meta.glove.profsum2 <- merge(meta.glove.profsum2, 
                       profdat.glovenew[profdat.glovenew$data=="simply_scripts", 
                                  c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.glove.profsum2)[15:16] <- c("effadtv", "seadtv")
meta.glove.profsum2 <- merge(meta.glove.profsum2, 
                       profdat.glovenew[profdat.glovenew$data=="adult_speech", 
                                  c("att", "eff", "se")], by = "att", all = TRUE)
colnames(meta.glove.profsum2)[17:18] <- c("effadspch", "seadspch")


## Descriptives of meta data
# Overall data
head(meta.glove.profsum2$att[order(meta.glove.profsum2$mean, decreasing = TRUE)])
head(meta.glove.profsum2$att[order(meta.glove.profsum2$mean, decreasing = FALSE)])

## How many are significant?
length(which(meta.glove.profsum2$p < .05))
meta.glove.profsum2$sig <- ifelse(meta.glove.profsum2$p < .05, "sig", "nsig")
table(meta.glove.profsum2$sig)
binom.test(table(meta.glove.profsum2$sig)[2],
           dim(meta.glove.profsum2)[1]) 
meta.glove.profsum2$sig.mf <- ifelse(meta.glove.profsum2$p < .05 & meta.glove.profsum2$mean < 0, "sig.f", 
                               ifelse(meta.glove.profsum2$p < .05 & meta.glove.profsum2$mean > 0, "sig.m", "nsig"))
table(meta.glove.profsum2$sig.mf)/dim(meta.glove.profsum2)[1]


## How many fall beyond [-0.1, 0.1]?
meta.glove.profsum2$effcut <- cut(meta.glove.profsum2$mean, breaks = c(-2, -0.1, 0.1, 2), labels = c("sig.f", "nsig", "sig.m"))
table(meta.glove.profsum2$effcut) 
table(meta.glove.profsum2$effcut)/dim(meta.glove.profsum2)[1] 
binom.test(sum(table(meta.glove.profsum2$effcut)[c(1, 3)]),
           dim(meta.glove.profsum2)[1]) 

## How many fall beyond [-0.2, 0.2]?
meta.glove.profsum2$effcut2 <- cut(meta.glove.profsum2$mean, breaks = c(-2, -0.2, 0.2, 2), labels = c("sig.f", "nsig", "sig.m"))
table(meta.glove.profsum2$effcut2) 
table(meta.glove.profsum2$effcut2)/dim(meta.glove.profsum2)[1]
binom.test(sum(table(meta.glove.profsum2$effcut2)[c(1, 3)]),
           dim(meta.glove.profsum2)[1]) 

## How many fall beyond [-0.3, 0.3]?
meta.glove.profsum2$effcut3 <- cut(meta.glove.profsum2$mean, breaks = c(-2, -0.3, 0.3, 2), labels = c("sig.f", "nsig", "sig.m"))
table(meta.glove.profsum2$effcut3) 
table(meta.glove.profsum2$effcut3)/dim(meta.glove.profsum2)[1] 
binom.test(sum(table(meta.glove.profsum2$effcut3)[c(1, 3)]),
           dim(meta.glove.profsum2)[1]) 

# Within child-produced speech
mean(meta.glove.profsum2$effchchild, na.rm = TRUE)
t.test(meta.glove.profsum2$effchchild, mu = 0)
head(meta.glove.profsum2$att[order(meta.glove.profsum2$effchchild, decreasing = TRUE)])
head(meta.glove.profsum2$att[order(meta.glove.profsum2$effchchild, decreasing = FALSE)])
# Within child-directed speech
mean(meta.glove.profsum2$effchpar, na.rm = TRUE)
t.test(meta.glove.profsum2$effchpar, mu = 0)
head(meta.glove.profsum2$att[order(meta.glove.profsum2$effchpar, decreasing = TRUE)])
head(meta.glove.profsum2$att[order(meta.glove.profsum2$effchpar, decreasing = FALSE)])
# Within child-directed books
mean(meta.glove.profsum2$effchbook, na.rm = TRUE)
t.test(meta.glove.profsum2$effchbook, mu = 0)
head(meta.glove.profsum2$att[order(meta.glove.profsum2$effchbook, decreasing = TRUE)])
head(meta.glove.profsum2$att[order(meta.glove.profsum2$effchbook, decreasing = FALSE)])
# Within child-directed TV
mean(meta.glove.profsum2$effchtv, na.rm = TRUE)
t.test(meta.glove.profsum2$effchtv, mu = 0)
head(meta.glove.profsum2$att[order(meta.glove.profsum2$effchtv, decreasing = TRUE)])
head(meta.glove.profsum2$att[order(meta.glove.profsum2$effchtv, decreasing = FALSE)])
# Within adult-directed speech
mean(meta.glove.profsum2$effadspch, na.rm = TRUE)
t.test(meta.glove.profsum2$effadspch, mu = 0)
head(meta.glove.profsum2$att[order(meta.glove.profsum2$effadspch, decreasing = TRUE)])
head(meta.glove.profsum2$att[order(meta.glove.profsum2$effadspch, decreasing = FALSE)])
# Within adult-directed books
mean(meta.glove.profsum2$effguten, na.rm = TRUE)
t.test(meta.glove.profsum2$effguten, mu = 0)
head(meta.glove.profsum2$att[order(meta.glove.profsum2$effguten, decreasing = TRUE)])
head(meta.glove.profsum2$att[order(meta.glove.profsum2$effguten, decreasing = FALSE)])
# Within adult-directed tv
mean(meta.glove.profsum2$effadtv, na.rm = TRUE)
t.test(meta.glove.profsum2$effadtv, mu = 0)
head(meta.glove.profsum2$att[order(meta.glove.profsum2$effadtv, decreasing = TRUE)])
head(meta.glove.profsum2$att[order(meta.glove.profsum2$effadtv, decreasing = FALSE)])

#correlation BLS
profdat.bls <- read.csv("professions_bls.csv") # included in .RData summary
colnames(profdat.bls) <- c("att", "perc.f")

meta.glove.profsum.bls <- merge(meta.glove.profsum2, profdat.bls, by = "att")
meta.glove.profsum.bls$perc.m <- 100 - meta.glove.profsum.bls$perc.f

## Average distribution of females in a profession?
mean(meta.glove.profsum.bls$perc.f)
cor.test(meta.glove.profsum.bls$mean, meta.glove.profsum.bls$perc.m)

## Plotting  ----
pdf(file = "metaprof_glove.pdf", width = 35, height = 20)
op <- par(mar = c(12, 8, 2, 2))
plot(meta.glove.profsum$mean[order(meta.glove.profsum$mean, decreasing = TRUE)], ylim = c(-1.6, 1.6),
     xlab = "", ylab = "", axes = FALSE,
     pch = 20, cex = 2, col = "gray")
box()
axis(1, at = 1:dim(meta.glove.profsum)[1], 
     labels = meta.glove.profsum$att[order(meta.glove.profsum$mean, decreasing = TRUE)], las = 2, cex.axis = 1.7)
axis(2, at = seq(-1.6, 1.6, by = 0.4), labels = round(seq(-1.6, 1.6, by = 0.4), 2), las = 1, cex.axis = 1.7)
mtext("Meta-analytic Effect (Occupation = Male)", line = 4, side = 2, cex = 3)
abline(h = 0, lty = 2, col = "gray", lwd = 4)
segments(x0 = c(1:dim(meta.glove.profsum)[1]), x1 = c(1:dim(meta.glove.profsum)[1]), 
         y0 = meta.glove.profsum$mean[order(meta.glove.profsum$mean, decreasing = TRUE)] - 1.96*meta.glove.profsum$se[order(meta.glove.profsum$mean, decreasing = TRUE)],
         y1 = meta.glove.profsum$mean[order(meta.glove.profsum$mean, decreasing = TRUE)] + 1.96*meta.glove.profsum$se[order(meta.glove.profsum$mean, decreasing = TRUE)],
         lty = 1, lwd = 2, col = "gray")
par(op)
dev.off()




## (13) Supplementary materials: Study 1 common Crawl data w/ Fasttext ----
# Read in SM WEAT results
ccdata <- read.csv("sm_embeddings_results_commoncrawl.csv") # Also in .RData
names(ccdata) <- c("dataset", "categories", "attributes",
                   "effsize", "miss", "pleft", "pright", "ptot",
                   "se", "coh_cat", "coh_att")

# (13a) Data preparation 
# For all double differences
table(ccdata$categories)
ccdoub <- ccdata[ccdata$categories=="female vs. male" | ccdata$categories=="male vs. female" ,]
ccdoub <- ccdoub[grepl("vs.", ccdoub$attributes)==TRUE,]
ccdoub <- ccdoub[c("dataset", "categories", "attributes", "effsize", "se", "ptot")]
colnames(ccdoub) <- c("data", "cat", "att", "eff", "se", "p")
# now add on the instruments-weapons effect sizes
instweap.cc <- ccdata[ccdata$categories=="instruments vs. weapons" & grepl("vs.", ccdata$attributes)==TRUE,]
instweap.cc <- instweap.cc[c("dataset", "categories", "attributes", "effsize", "se", "ptot")]
colnames(instweap.cc) <- c("data", "cat", "att", "eff", "se", "p")
ccdoub <- rbind(ccdoub, instweap.cc)

# Get all the expected effects in the positive direction...
table(ccdoub$att)
ccdoub$att <- factor(ccdoub$att, labels = c("bad vs. good", "work vs. home", "math vs. reading", "science vs. arts"))
table(ccdoub$cat)
ccdoub$cat <- factor(ccdoub$cat, labels = c("weapons vs. instruments", "male vs. female"))

# And correct the d-scores to align with positive = expected order.
# Multiply by -1 for all double-differences except instrumen/weapons and arts/science
ccdoub$eff.correct <- ifelse(grepl("science", ccdoub$att)==TRUE | grepl("weapons", ccdoub$cat)==TRUE, 
                             ccdoub$eff, ccdoub$eff*-1)

# (13b) Summary of all effect sizes
ccdoub



## (14) Supplementary materials: Study 1 common Crawl data w/ Glove ----
# Read in SM WEAT results
ccglovedata <- read.csv("sm_embeddings_results_glove_cc.csv") # Also in .RData summary file
names(ccglovedata) <- c("dataset", "categories", "attributes",
                   "effsize", "miss", "pleft", "pright", "ptot",
                   "se", "coh_cat", "coh_att")

# (14a) Data preparation 
# For all double differences
table(ccglovedata$categories)
ccglovedoub <- ccglovedata[ccglovedata$categories=="female vs. male" | ccglovedata$categories=="male vs. female" ,]
ccglovedoub <- ccglovedoub[grepl("vs.", ccglovedoub$attributes)==TRUE,]
ccglovedoub <- ccglovedoub[c("dataset", "categories", "attributes", "effsize", "se", "ptot")]
colnames(ccglovedoub) <- c("data", "cat", "att", "eff", "se", "p")
# now add on the instruments-weapons effect sizes
instweap.ccglove <- ccglovedata[ccglovedata$categories=="instruments vs. weapons" & grepl("vs.", ccglovedata$attributes)==TRUE,]
instweap.ccglove <- instweap.ccglove[c("dataset", "categories", "attributes", "effsize", "se", "ptot")]
colnames(instweap.ccglove) <- c("data", "cat", "att", "eff", "se", "p")
ccglovedoub <- rbind(ccglovedoub, instweap.ccglove)

# Get all the expected effects in the positive direction...
table(ccglovedoub$att)
ccglovedoub$att <- factor(ccglovedoub$att, labels = c("bad vs. good", "work vs. home", "math vs. reading", "science vs. arts"))
table(ccglovedoub$cat)
ccglovedoub$cat <- factor(ccglovedoub$cat, labels = c("weapons vs. instruments", "male vs. female"))

# And correct the d-scores to align with positive = expected order.
# Multiply by -1 for all double-differences except instrumen/weapons and arts/science
ccglovedoub$eff.correct <- ifelse(grepl("science", ccglovedoub$att)==TRUE | grepl("weapons", ccglovedoub$cat)==TRUE, 
                             ccglovedoub$eff, ccglovedoub$eff*-1)

# (14b) Summary of all effect sizes
ccglovedoub


## Plotting  ----
# alongside meta-analytic results 
ccdoub.plot <- ccdoub[1:4,]
ccdoub.plot$att <- factor(ccdoub.plot$att, levels = c("bad vs. good", "work vs. home", 
                                                      "science vs. arts", "math vs. reading"))
table(ccdoub.plot$att)
ccdoub.plot <- arrange(ccdoub.plot, att)
ccglovedoub.plot <- ccglovedoub[1:4,]
ccglovedoub.plot$att <- factor(ccglovedoub.plot$att, levels = c("bad vs. good", "work vs. home", 
                                                      "science vs. arts", "math vs. reading"))
table(ccglovedoub.plot$att)
ccglovedoub.plot <- arrange(ccglovedoub.plot, att)

pdf(file = "sm.repplot.pdf", width = 20, height = 10)
op <- par(mar = c(5,6,2,2))
plot(1, type="n", xlab="", ylab="", 
     xlim=c(0.5, 4.5), ylim=c(-2, 2), axes = FALSE)
box()
mtext("Effect Size", side = 2, line = 3, cex = 2)
axis(1, at = c(1:4), labels = c("Male-Bad\nFemale-Good", "Male-Work\nFemale-Home", 
                                "Male-Science\nFemale-Arts", "Male-Math\nFemale-Reading"),
     cex.axis = 2, padj = 1)
axis(2, at = seq(-1.5, 1.5, by = 0.5), labels = seq(-1.5, 1.5, by = 0.5), las = 2, cex.axis = 1.7)
abline(h = 0, lty = 2)

points(seq(1, 4, by = 1),
       metadatsum2$metamean, 
       pch = 17, cex = 3, col = "red")
segments(x0 = seq(1, 4, by = 1), x1 = seq(1, 4, by = 1),
         y0 = metadatsum2$meta95low,
         y1 = metadatsum2$meta95high, 
         lty = 1, lwd = 3, col = "red")

points(seq(0.8, 3.8, by = 1),
       ccdoub.plot$eff.correct, 
       pch = 20, cex = 3, col = "darkgreen")
segments(x0 = seq(0.8, 3.8, by = 1), x1 = seq(0.8, 3.8, by = 1),
         y0 = ccdoub.plot$eff.correct - 1.96*ccdoub.plot$se,
         y1 = ccdoub.plot$eff.correct + 1.96*ccdoub.plot$se, 
         lty = 1, lwd = 3, col = "darkgreen")

points(seq(1.1, 4.1, by = 1),
       ccglovedoub.plot$eff.correct, 
       pch = 20, cex = 3, col = "darkblue")
segments(x0 = seq(1.1, 4.1, by = 1), x1 = seq(1.1, 4.1, by = 1),
         y0 = ccglovedoub.plot$eff.correct - 1.96*ccglovedoub.plot$se,
         y1 = ccglovedoub.plot$eff.correct + 1.96*ccglovedoub.plot$se, 
         lty = 1, lwd = 3, col = "darkblue")

legend("bottomleft", c("fastText + Common Crawl", 
                       "GloVe + Common Crawl", "Meta estimate"),
       col = c("darkgreen", "darkblue",  "red"),
       pch = c(rep(20, 4), 20), 
       cex = 3, pt.cex = c(rep(4, 4), 3.5), bty = "n", ncol = 3)

par(op)
dev.off()




## (15) Supplementary materials: Studies 2 and 3 PCA of trait and occupation dimensions ----
## Create correlation matrices for TRAITS (** just adult data and using trait synonyms)
cor.traits <- meta.groups.traitsum2[c("effadtv", "effadspch", "effguten")]
rownames(cor.traits) <- meta.groups.traitsum2$att
cor.traits.mat <- rcorr(as.matrix(cor.traits))
cor.traits.mat$r
cor.traits.matav <- cor.traits.mat$r
cor.traits.matavp <- cor.traits.mat$P
diag(cor.traits.matav) <- NA 
diag(cor.traits.matavp) <- NA 

mean(cor.traits.matav, na.rm = TRUE)
table(cor.traits.matavp < .05)

## Create correlation matrices for OCCUPATIONS (** just adult data)
head(meta.profsum2)
cor.profs <- meta.profsum2[c("effadtv", "effadspch", "effguten")]
rownames(cor.profs) <- meta.profsum2$att
cor.profs.mat <- rcorr(as.matrix(cor.profs))
cor.profs.matav <- cor.profs.mat$r
cor.profs.matavp <- cor.profs.mat$P
diag(cor.profs.matav) <- NA 
diag(cor.profs.matavp) <- NA 

mean(cor.profs.matav, na.rm = TRUE)
table(cor.profs.matavp < .05)

## PCA for traits
cor.traits.sub <- cor.traits[complete.cases(cor.traits),]
pca.traits <- prcomp(cor.traits.sub, scale. = TRUE, center = TRUE)
summary(pca.traits)
pcaassign.traits <- as.data.frame(pca.traits$x)
screeplot(pca.traits, type = "lines")

head(rownames(pcaassign.traits)[order(pcaassign.traits$PC1, decreasing = TRUE)])
head(rownames(pcaassign.traits)[order(pcaassign.traits$PC1, decreasing = FALSE)])


pdf(file = "sm_pca_traits.pdf", width = 10, height = 10)
op <- par(mar = c(6, 6, 2, 2))
plot(pcaassign.traits$PC1, pcaassign.traits$PC2, ylim = c(-4, 4), xlim = c(-4, 4),
     axes = FALSE, ylab = "", xlab = "",
     pch = 20, col = "darkgrey", cex = 2)
box()
axis(1, at = seq(-4, 4, by = 1), labels = round(seq(-4, 4, by = 1), 2), las = 1, cex.axis = 1.5)
axis(2, at = seq(-4, 4, by = 1), labels = round(seq(-4, 4, by = 1), 2), las = 1, cex.axis = 1.5)
mtext("Principal Component 2 (29%)", side = 2, line = 3, cex = 2)
mtext("Principal Component 1 (48%)", side = 1, line = 3, cex = 2)
abline(h = 0, lty = 2, col = "gray")
abline(v = 0, lty = 2, col = "gray")
with(pcaassign.traits, text(pcaassign.traits$PC2 ~ pcaassign.traits$PC1, labels = rownames(pcaassign.traits), pos = 1, cex = 2, col = "darkgrey"))
par(op)
dev.off()

## PCA for professions
cor.profs.sub <- cor.profs[complete.cases(cor.profs),]
pca.profs <- prcomp(cor.profs.sub, scale. = TRUE, center = TRUE)
summary(pca.profs)
pcaassign.profs <- as.data.frame(pca.profs$x)

head(rownames(pcaassign.profs)[order(pcaassign.profs$PC1, decreasing = TRUE)])
head(rownames(pcaassign.profs)[order(pcaassign.profs$PC1, decreasing = FALSE)])


pdf(file = "sm_pca_profs.pdf", width = 10, height = 10)
op <- par(mar = c(6, 6, 2, 2))
plot(pcaassign.profs$PC1, pcaassign.profs$PC2, ylim = c(-4, 4), xlim = c(-4, 4),
     axes = FALSE, ylab = "", xlab = "",
     pch = 20, col = "darkgrey", cex = 2)
box()
axis(1, at = seq(-4, 4, by = 1), labels = round(seq(-4, 4, by = 1), 2), las = 1, cex.axis = 1.5)
axis(2, at = seq(-4, 4, by = 1), labels = round(seq(-4, 4, by = 1), 2), las = 1, cex.axis = 1.5)
mtext("Principal Component 2 (24%)", side = 2, line = 3, cex = 2)
mtext("Principal Component 1 (62%)", side = 1, line = 3, cex = 2)
abline(h = 0, lty = 2, col = "gray")
abline(v = 0, lty = 2, col = "gray")
with(pcaassign.profs, text(pcaassign.profs$PC2 ~ pcaassign.profs$PC1, labels = rownames(pcaassign.profs), pos = 1, cex = 0.8, col = "darkgrey"))
par(op)
dev.off()


# (16) Supplementary materials: Studies 2 and 3 word norms on masculinity/femininity ----
traitdat.gennorms <- read.csv("childtraitnorms.csv") # Also in .RData summary file
# traitdat.gennorms --- Trait word norms of masculininity/femininity from children
## Imported word norm data from:
# Powlishta, K. K. (1995). Gender bias in children’s perceptions of personality traits. Sex Roles, 32(1–2), 17–28. https://doi.org/10.1007/BF01544755

traitdat.gennorms$trait <- as.character(traitdat.gennorms$trait)
colnames(traitdat.gennorms)[1] <- "att"
traitdat.gennorms$mean.male <- traitdat.gennorms$mean.male * -1
traitdat.gennorms$mean.female <- traitdat.gennorms$mean.female * -1
# to compute combined mean, use N of females = 42, and N of males = 39 (retrieved from data reported in Powlishta, 1995)
nmal <- 39; nfemal <- 42
traitdat.gennorms$mean.comb <- (traitdat.gennorms$mean.male*nmal + traitdat.gennorms$mean.female*nfemal)/(nmal + nfemal)
which(traitdat.gennorms$att %in% meta.groups.traitsum2$att) # note: merging with synonym data

trait.gennormmeta <- merge(meta.groups.traitsum2, traitdat.gennorms, by = "att")

cor.test(trait.gennormmeta$mean, trait.gennormmeta$mean.male)
cor.test(trait.gennormmeta$mean, trait.gennormmeta$mean.female)
cor.test(trait.gennormmeta$mean, trait.gennormmeta$mean.comb)

# Plotting
pdf(file = "gennorms.traits.pdf", width = 18, height = 5)
op <- par(mfrow = c(1, 3), mar = c(5,6,4,4))
plot(trait.gennormmeta$mean, trait.gennormmeta$mean.comb, ylab = "Word norm of masculinity/femininity", xlab = "WEAT masculinity/feminity",
     pch = 20, ylim = c(-2,2), xlim = c(-0.9, 0.4), cex.lab = 1.3, main = "a. Combined ratings")
with(trait.gennormmeta, text(mean.comb ~ mean, labels = att, pos = 1, cex = 0.9, col = "darkgrey"))
abline(lm(mean.comb ~ mean, data = trait.gennormmeta), col = "cadetblue", lwd = 2)
plot(trait.gennormmeta$mean, trait.gennormmeta$mean.female, ylab = "Word norm of masculinity/femininity", xlab = "WEAT masculinity/feminity",
     pch = 20, ylim = c(-2,2), xlim = c(-0.9, 0.4), cex.lab = 1.3, main = "b. Girls' ratings only")
with(trait.gennormmeta, text(mean.female ~ mean, labels = att, pos = 1, cex = 0.9, col = "darkgrey"))
abline(lm(mean.female ~ mean, data = trait.gennormmeta), col = "cadetblue", lwd = 2)
plot(trait.gennormmeta$mean, trait.gennormmeta$mean.male, ylab = "Word norm of masculinity/femininity", xlab = "WEAT masculinity/feminity",
     pch = 20, ylim = c(-2,2), xlim = c(-0.9, 0.4), cex.lab = 1.3, main = "c. Boys' ratings only")
with(trait.gennormmeta, text(mean.male ~ mean, labels = att, pos = 1, cex = 0.9, col = "darkgrey"))
abline(lm(mean.male ~ mean, data = trait.gennormmeta), col = "cadetblue", lwd = 2)
par(op)
dev.off()

# traitdat.gennorms.ad -- Trait word norms of masculininity/femininity from adults
## Imported word norm data from:
# Williams, J. E., & Bennett, S. M. (1975). The definition of sex stereotypes via the adjective check list. Sex Roles, 1(4), 327–337. https://doi.org/10.1007/BF00287224
# traitdat.gennorms.ad <- read.csv("adulttraitnorms.csv") # Also in .RData summary file

traitdat.gennorms.ad$trait <- as.character(traitdat.gennorms.ad$trait)
colnames(traitdat.gennorms.ad)[1] <- "att"
traitdat.gennorms.ad$avp <- (traitdat.gennorms.ad$pmale + traitdat.gennorms.ad$pfemale)/2
traitdat.gennorms.ad$avp.male <- ifelse(traitdat.gennorms.ad$assoc == "men", traitdat.gennorms.ad$avp, 100 - traitdat.gennorms.ad$avp)
which(traitdat.gennorms.ad$att %in% meta.groups.traitsum2$att)
trait.gennormmeta.ad <- merge(meta.groups.traitsum2, traitdat.gennorms.ad, by = "att")

cor.test(trait.gennormmeta.ad$mean, trait.gennormmeta.ad$avp.male)

pdf(file = "gennorms.ad.traits.pdf", width = 18, height = 5)
op <- par(mar = c(5,6,4,4))
plot(trait.gennormmeta.ad$mean, trait.gennormmeta.ad$avp.male, ylab = "Boys' ratings of masculinity/femininity", xlab = "WEAT masculinity/feminity",
     pch = 20, ylim = c(0,100), xlim = c(-0.9, 0.4), cex.lab = 1.3)
with(trait.gennormmeta.ad, text(avp.male ~ mean, labels = att, pos = 1, cex = 1.1, col = "darkgrey"))
par(op)
dev.off()

# profdat.wordnorms -- Profession label norms by gender 
## Import word norm data from:
# Ute, G., Gygax, P., Sarrasin, O., Garnham, A., & Oakhill, J. (2008). Au pairs are rarely male: Norms on the gender perception of role names across English, French, and German. Behavior Research Methods, 40(1), 206–212. https://doi.org/10.3758/BRM.40.1.206
# profdat.gennorms <- read.csv("professionnorms.csv") # Also in .RData summary file

colnames(profdat.gennorms) <- c("att", "gennorm", "sd")
profdat.gennorms$att <- tolower(profdat.gennorms$att)
profdat.gennorms$att <- substr(profdat.gennorms$att, 1, nchar(profdat.gennorms$att)-1) # remove the last character (s) from all strings
profdat.gennorms$sd <- profdat.gennorms$sd * -1

## Merge the dataframes to examine relationships
which(profdat.gennorms$att %in% meta.profsum2$att)
prof.normmeta <- merge(meta.profsum2, profdat.gennorms, by = "att")

# Correlation between word norms and profession labels.
cor.test(prof.normmeta$mean, prof.normmeta$gennorm)

pdf(file = "gennorms.profs.pdf", width = 20, height = 5)
op <- par(mar = c(5,6,4,4))
plot(prof.normmeta$mean, prof.normmeta$gennorm, ylab = "Masculinity/femininity norm", xlab = "WEAT masculinity/femininity",
     pch = 20, ylim = c(0,100), cex.lab = 2)
with(prof.normmeta, text(gennorm ~ mean, labels = att, pos = 1, cex = 1, col = "darkgrey"))
abline(lm(gennorm ~ mean, data = prof.normmeta), col = "darkgrey", lwd = 2)
par(op)
dev.off()


# (17) Supplementary materials: Studies 2 and 3 word norms and PCA correlations ----
head(pcaassign.traits)
pcaassign.traits2 <- pcaassign.traits[c("PC1", "PC2")]
pcaassign.traits2$att <- rownames(pcaassign.traits2)
head(trait.gennormmeta.ad)
trait.gennormmeta.ad2 <- trait.gennormmeta.ad[c("att", "avp.male")]
length(which(trait.gennormmeta.ad2$att %in% pcaassign.traits2$att))

pca.trait.cor <- merge(pcaassign.traits2, trait.gennormmeta.ad2, by = "att")
cor.test(pca.trait.cor$PC1*-1, pca.trait.cor$avp.male) 
# note: multiplying PC1 loadings by -1 to get them in a more interpretable direction (PCs are interpretable regardless of sign)

pdf(file = "pca.gennorms.traits.pdf", width = 10, height = 10)
op <- par(mar = c(5,6,4,4))
plot(pca.trait.cor$avp.male, pca.trait.cor$PC1, 
     ylab = "PCA loading of PC1", xlab = "Adult's ratings of trait's masculinity/femininity",
     pch = 20, cex.lab = 1.3, xlim = c(-5, 105), ylim = c(-3, 2.5))
with(pca.trait.cor, text(PC1 ~ avp.male, labels = att, pos = 1, cex = 1.1, col = "darkgrey"))
abline(v = 50, lty = 2, lwd = 2, col = "darkgrey")
abline(h = 0, lty = 2, lwd = 2, col = "darkgrey")
par(op)
dev.off()


head(pcaassign.profs)
pcaassign.profs2 <- pcaassign.profs[c("PC1", "PC2")]
pcaassign.profs2$att <- rownames(pcaassign.profs2)
head(prof.normmeta)
prof.normmeta2 <- prof.normmeta[c("att", "gennorm")]
length(which(prof.normmeta2$att %in% pcaassign.profs2$att))

pca.prof.cor <- merge(pcaassign.profs2, prof.normmeta2, by = "att")
cor.test(pca.prof.cor$PC1*-1, pca.prof.cor$gennorm) # note: multiplied PCA loadings by -1 to align interpretations with above.

pdf(file = "pca.gennorms.profs.pdf", width = 10, height = 10)
op <- par(mar = c(5,6,4,4))
plot(pca.prof.cor$gennorm, pca.prof.cor$PC1*-1, 
     ylab = "PCA loading of PC1", xlab = "Adult's ratings of occupations's masculinity/femininity",
     pch = 20, cex.lab = 1.3, xlim = c(-5, 105), ylim = c(-3, 3))
with(pca.prof.cor, text(PC1*-1 ~ gennorm, labels = att, pos = 1, cex = 1.1, col = "darkgrey"))
abline(v = 50, lty = 2, lwd = 2, col = "darkgrey")
abline(h = 0, lty = 2, lwd = 2, col = "darkgrey")
par(op)
dev.off()

# (18) Supplementary materials: Study 2 adult-only copora meta-analyzed for trait analyses ----
table(traitdatadultnew$att)
traitdatadultnew$attnum <- as.numeric(traitdatadultnew$att)
table(traitdatadultnew$att)
table(traitdatadultnew$attnum)

meta.adult.alltrait <- list()
meta.adult.traitmeans <- vector()
meta.adult.traitse <- vector()
meta.adult.traitp <- vector()

for (i in 1:length(table(traitdatadultnew$att))){
  meta.adult.alltrait[[i]] <- metagen(TE = traitdatadultnew$eff[traitdatadultnew$attnum==i],
                                      seTE = traitdatadultnew$se[traitdatadultnew$attnum==i], sm = "MD")
  meta.adult.traitmeans[i] <- meta.adult.alltrait[[i]]$TE.fixed
  meta.adult.traitse[i] <- meta.adult.alltrait[[i]]$seTE.fixed
  meta.adult.traitp[i] <- meta.adult.alltrait[[i]]$pval.fixed
}

names(meta.adult.alltrait) <- levels(traitdatadultnew$att)
meta.adult.traitsum <- as.data.frame(meta.adult.traitmeans)
colnames(meta.adult.traitsum) <- "mean"
meta.adult.traitsum$se <- meta.adult.traitse
meta.adult.traitsum$p <- meta.adult.traitp
meta.adult.traitsum$att <- levels(traitdatadultnew$att)

# Overall data
mean(meta.adult.traitsum$mean)
range(meta.adult.traitsum$mean) 
head(meta.adult.traitsum$att[order(meta.adult.traitsum$mean, decreasing = TRUE)])
head(meta.adult.traitsum$att[order(meta.adult.traitsum$mean, decreasing = FALSE)])

## How many are significant?
length(which(meta.adult.traitsum$p < .05))
meta.adult.traitsum$sig <- ifelse(meta.adult.traitsum$p < .05, "sig", "nsig")
table(meta.adult.traitsum$sig)
binom.test(table(meta.adult.traitsum$sig)[2],
           dim(meta.adult.traitsum)[1]) 
table(meta.adult.traitsum$sig)/dim(meta.adult.traitsum)[1]
meta.adult.traitsum$sig.mf <- ifelse(meta.adult.traitsum$p < .05 & meta.adult.traitsum$mean < 0, "sig.f", 
                                ifelse(meta.adult.traitsum$p < .05 & meta.adult.traitsum$mean > 0, "sig.m", "nsig"))
table(meta.adult.traitsum$sig.mf)/dim(meta.adult.traitsum)[1]

## How many fall beyond [-0.1, 0.1]?
meta.adult.traitsum$effcut <- cut(meta.adult.traitsum$mean, breaks = c(-2, -0.1, 0.1, 2), labels = c("sig.f", "nsig", "sig.m"))
table(meta.adult.traitsum$effcut)/dim(meta.adult.traitsum)[1]
binom.test(sum(table(meta.adult.traitsum$effcut)[c(1, 3)]),
           dim(meta.adult.traitsum)[1])

## How many fall beyond [-0.2, 0.2]?
meta.adult.traitsum$effcut2 <- cut(meta.adult.traitsum$mean, breaks = c(-2, -0.2, 0.2, 2), labels = c("sig.f", "nsig", "sig.m"))
table(meta.adult.traitsum$effcut2)/dim(meta.adult.traitsum)[1]
binom.test(sum(table(meta.adult.traitsum$effcut2)[c(1, 3)]),
           dim(meta.adult.traitsum)[1]) 

## How many fall beyond [-0.3, 0.3]?
meta.adult.traitsum$effcut3 <- cut(meta.adult.traitsum$mean, breaks = c(-2, -0.3, 0.3, 2), labels = c("sig.f", "nsig", "sig.m"))
table(meta.adult.traitsum$effcut3)/dim(meta.adult.traitsum)[1]
binom.test(sum(table(meta.adult.traitsum$effcut3)[c(1, 3)]),
           dim(meta.adult.traitsum)[1])

## Are traits more female or more male?
t.test(meta.adult.traitsum$mean, mu = 0)
meta.adult.traitsum$malfem <- ifelse(meta.adult.traitsum$mean > 0, "mal", "fem")
table(meta.adult.traitsum$malfem) 
binom.test(table(meta.adult.traitsum$malfem)[1],
           dim(meta.adult.traitsum)[1]) 

# top five and bottom five
head(meta.adult.traitsum$att[order(meta.adult.traitsum$mean, decreasing = TRUE)])
head(meta.adult.traitsum$att[order(meta.adult.traitsum$mean, decreasing = FALSE)])

## Plotting
meta.adult.traitsum[order(meta.adult.traitsum$mean, decreasing = TRUE),]
# cut-off = "creative" (~ -.1) and "critical" (~ .1)
# cut-off = "precise" (~ -.2) and "honest" (~ .2)
# cut-off = "forward" (~ -.3) and rigid (~ .3)

pdf(file = "metatrait_plot_adults_efver.pdf", width = 35, height = 20)
op <- par(mar = c(12, 8, 2, 2))
plot(meta.adult.traitsum$mean[order(meta.adult.traitsum$mean, decreasing = TRUE)], ylim = c(-1.2, 0.8),
     xlab = "", ylab = "", axes = FALSE,
     pch = 20, cex = 2, col = "gray")
box()
axis(1, at = 1:167, 
     labels = meta.adult.traitsum$att[order(meta.adult.traitsum$mean, decreasing = TRUE)], las = 2, cex.axis = 1.7)
axis(2, at = seq(-1.4, 1.4, by = 0.2), labels = round(seq(-1.4, 1.4, by = 0.2), 2), las = 1, cex.axis = 1.7)
mtext("Meta-analytic Effect (Trait = Male)", line = 4, side = 2, cex = 3)
abline(h = 0, lty = 2, col = "gray", lwd = 4)
segments(x0 = c(1:167), x1 = c(1:167), 
         y0 = meta.adult.traitsum$mean[order(meta.adult.traitsum$mean, decreasing = TRUE)] - 1.96*meta.adult.traitsum$se[order(meta.adult.traitsum$mean, decreasing = TRUE)],
         y1 = meta.adult.traitsum$mean[order(meta.adult.traitsum$mean, decreasing = TRUE)] + 1.96*meta.adult.traitsum$se[order(meta.adult.traitsum$mean, decreasing = TRUE)],
         lty = 1, lwd = 2, col = "gray")
abline(v = which(meta.adult.traitsum$att[order(meta.adult.traitsum$mean, decreasing = TRUE)] == "creative"), 
       col = "red", lty = 2, lwd = 3)
abline(v = which(meta.adult.traitsum$att[order(meta.adult.traitsum$mean, decreasing = TRUE)] == "critical"),
       col = "red", lty = 2, lwd = 3)
abline(v = which(meta.adult.traitsum$att[order(meta.adult.traitsum$mean, decreasing = TRUE)] == "precise"), 
       col = "orange", lty = 2, lwd = 3)
abline(v = which(meta.adult.traitsum$att[order(meta.adult.traitsum$mean, decreasing = TRUE)] == "honest"),
       col = "orange", lty = 2, lwd = 3)
abline(v = which(meta.adult.traitsum$att[order(meta.adult.traitsum$mean, decreasing = TRUE)] == "forward"), 
       col = "seagreen", lty = 2, lwd = 3)
abline(v = which(meta.adult.traitsum$att[order(meta.adult.traitsum$mean, decreasing = TRUE)] == "rigid"),
       col = "seagreen", lty = 2, lwd = 3)
legend("bottomright", c("[-0.3, 0.3]", "[-0.2, 0.2]", "[-0.1, 0.1]"), col = c("seagreen", "orange", "red"), lty = 2, lwd = 4, bty = "n", cex = 4)
par(op)
dev.off()


# (19) Supplementary materials: Study 3 adult-only copora meta-analyzed for occupation analyses ----
table(profdatadultnew$att)
profdatadultnew$attnum <- as.numeric(profdatadultnew$att)
table(profdatadultnew$att)
table(profdatadultnew$attnum)

meta.adult.allprof <- list()
meta.adult.profmeans <- vector()
meta.adult.profse <- vector()
meta.adult.profp <- vector()

for (i in 1:length(table(profdatadultnew$att))){
  meta.adult.allprof[[i]] <- metagen(TE = profdatadultnew$eff[profdatadultnew$attnum==i],
                                      seTE = profdatadultnew$se[profdatadultnew$attnum==i], sm = "MD")
  meta.adult.profmeans[i] <- meta.adult.allprof[[i]]$TE.fixed
  meta.adult.profse[i] <- meta.adult.allprof[[i]]$seTE.fixed
  meta.adult.profp[i] <- meta.adult.allprof[[i]]$pval.fixed
}

names(meta.adult.allprof) <- levels(profdatadultnew$att)
meta.adult.profsum <- as.data.frame(meta.adult.profmeans)
colnames(meta.adult.profsum) <- "mean"
meta.adult.profsum$se <- meta.adult.profse
meta.adult.profsum$p <- meta.adult.profp
meta.adult.profsum$att <- levels(profdatadultnew$att)

# Overall data
mean(meta.adult.profsum$mean)
range(meta.adult.profsum$mean) 
head(meta.adult.profsum$att[order(meta.adult.profsum$mean, decreasing = TRUE)])
head(meta.adult.profsum$att[order(meta.adult.profsum$mean, decreasing = FALSE)])
# Note: Result is reported in main text, Table 2

## How many are significant?
length(which(meta.adult.profsum$p < .05))
meta.adult.profsum$sig <- ifelse(meta.adult.profsum$p < .05, "sig", "nsig")
table(meta.adult.profsum$sig)
binom.test(table(meta.adult.profsum$sig)[2],
           dim(meta.adult.profsum)[1]) 
table(meta.adult.profsum$sig)/dim(meta.adult.profsum)[1]
meta.adult.profsum$sig.mf <- ifelse(meta.adult.profsum$p < .05 & meta.adult.profsum$mean < 0, "sig.f", 
                                     ifelse(meta.adult.profsum$p < .05 & meta.adult.profsum$mean > 0, "sig.m", "nsig"))
table(meta.adult.profsum$sig.mf)/dim(meta.adult.profsum)[1]

## How many fall beyond [-0.1, 0.1]?
meta.adult.profsum$effcut <- cut(meta.adult.profsum$mean, breaks = c(-2, -0.1, 0.1, 2), labels = c("sig.f", "nsig", "sig.m"))
table(meta.adult.profsum$effcut)/dim(meta.adult.profsum)[1]
binom.test(sum(table(meta.adult.profsum$effcut)[c(1, 3)]),
           dim(meta.adult.profsum)[1]) 

## How many fall beyond [-0.2, 0.2]?
meta.adult.profsum$effcut2 <- cut(meta.adult.profsum$mean, breaks = c(-2, -0.2, 0.2, 2), labels = c("sig.f", "nsig", "sig.m"))
table(meta.adult.profsum$effcut2)/dim(meta.adult.profsum)[1]
binom.test(sum(table(meta.adult.profsum$effcut2)[c(1, 3)]),
           dim(meta.adult.profsum)[1]) 

## How many fall beyond [-0.3, 0.3]?
meta.adult.profsum$effcut3 <- cut(meta.adult.profsum$mean, breaks = c(-2, -0.3, 0.3, 2), labels = c("sig.f", "nsig", "sig.m"))
table(meta.adult.profsum$effcut3)/dim(meta.adult.profsum)[1]
binom.test(sum(table(meta.adult.profsum$effcut3)[c(1, 3)]),
           dim(meta.adult.profsum)[1]) 

## Are profs more female or more male?
t.test(meta.adult.profsum$mean, mu = 0)
meta.adult.profsum$malfem <- ifelse(meta.adult.profsum$mean > 0, "mal", "fem")
table(meta.adult.profsum$malfem) 
binom.test(table(meta.adult.profsum$malfem)[1],
           dim(meta.adult.profsum)[1]) 

# top five and bottom five
head(meta.adult.profsum$att[order(meta.adult.profsum$mean, decreasing = TRUE)])
head(meta.adult.profsum$att[order(meta.adult.profsum$mean, decreasing = FALSE)])


## Plotting
# cut-off = fisher (~ -.1) and "author" (~ .1)
# cut-off = "photographer" (~ -.2) and "technician" (~ .2)
# cut-off = "therapist" (~ -.3) and "designer" (~ .3)
meta.adult.profsum[order(meta.adult.profsum$mean, decreasing = TRUE),]

pdf(file = "metaprof_plot_adults_efver.pdf", width = 35, height = 20)
op <- par(mar = c(12, 8, 2, 2))
plot(meta.adult.profsum$mean[order(meta.adult.profsum$mean, decreasing = TRUE)], ylim = c(-1.4, 1.4),
     xlab = "", ylab = "", axes = FALSE,
     pch = 20, cex = 2, col = "gray")
box()
axis(1, at = 1:81, 
     labels = meta.adult.profsum$att[order(meta.adult.profsum$mean, decreasing = TRUE)], las = 2, cex.axis = 1.7)
axis(2, at = seq(-1.6, 1.6, by = 0.4), labels = round(seq(-1.6, 1.6, by = 0.4), 2), las = 1, cex.axis = 1.7)
mtext("Meta-analytic Effect (Occupation = Male)", line = 4, side = 2, cex = 3)
abline(h = 0, lty = 2, col = "gray", lwd = 4)
segments(x0 = c(1:81), x1 = c(1:81), 
         y0 = meta.adult.profsum$mean[order(meta.adult.profsum$mean, decreasing = TRUE)] - 1.96*meta.adult.profsum$se[order(meta.adult.profsum$mean, decreasing = TRUE)],
         y1 = meta.adult.profsum$mean[order(meta.adult.profsum$mean, decreasing = TRUE)] + 1.96*meta.adult.profsum$se[order(meta.adult.profsum$mean, decreasing = TRUE)],
         lty = 1, lwd = 2, col = "gray")
abline(v = which(meta.adult.profsum$att[order(meta.adult.profsum$mean, decreasing = TRUE)] == "fisher"), 
       col = "red", lty = 2, lwd = 3)
abline(v = which(meta.adult.profsum$att[order(meta.adult.profsum$mean, decreasing = TRUE)] == "author"),
       col = "red", lty = 2, lwd = 3)
abline(v = which(meta.adult.profsum$att[order(meta.adult.profsum$mean, decreasing = TRUE)] == "photographer"), 
       col = "orange", lty = 2, lwd = 3)
abline(v = which(meta.adult.profsum$att[order(meta.adult.profsum$mean, decreasing = TRUE)] == "technician"),
       col = "orange", lty = 2, lwd = 3)
abline(v = which(meta.adult.profsum$att[order(meta.adult.profsum$mean, decreasing = TRUE)] == "therapist"), 
       col = "seagreen", lty = 2, lwd = 3)
abline(v = which(meta.adult.profsum$att[order(meta.adult.profsum$mean, decreasing = TRUE)] == "designer"),
       col = "seagreen", lty = 2, lwd = 3)
legend("bottomright", c("[-0.3, 0.3]", "[-0.2, 0.2]", "[-0.1, 0.1]"), col = c("seagreen", "orange", "red"), lty = 2, lwd = 4, bty = "n", cex = 4)
par(op)
dev.off()


