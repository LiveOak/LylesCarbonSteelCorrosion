#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.

############################
## @knitr LoadPackages
require(knitr) #For literate programming
require(testit) #For convenient asserts
require(RColorBrewer) #For the color palettes
require(plyr) #For manipulating datsets
require(grid) #For graphing
require(gridExtra) #For graphing
require(ggplot2) #For graphing
require(boot) #For bootstrapped CIs
require(lme4) #For multilevel modeling (MLM)
require(arm) #For manipulating MLM standard errors
require(MCMCglmm)
# require(quantreg) #For quantile regression

############################
## @knitr DeclareGlobals
pathInputSummaryBinAll <- "./Data/Derived/SummaryBinAll.rds"
pathInputProbeAll <- "./Data/Derived/ProbeAll.rds"
pathMcmcResults <-  "./Code/EstimateMlmMcmc/mcmcMlmResults.RData"


bootSpread <- function( scores, weights=NULL, conf=.68 ) {
  plugin <- function( d, i ) { mean(d[i]) }  
  distribution <- boot(data=scores, plugin, weights=weights, R=99)
  
  # I'm using the percentile instead of the BCa, because the acceleration can't be estimated with these scores and weights for some reason.  I'm not using them in the real anlaysis anyway.
  ci <- boot.ci(distribution, type = c("perc"), conf=conf)
  return( ci$percent[4:5] )
}

SummarizeCorrosion <- function( d ) {  
  weightedModel <- lm(ProbeDepth ~ 1, data=d, weights=ProportionAtDepth)
  
  meanV1 <- sum(d$ProbeDepth * d$ProportionAtDepth) / sum(d$ProportionAtDepth)
  meanV2 <- coef(summary(weightedModel))["(Intercept)", "Estimate"]
  se <- coef(summary(weightedModel))["(Intercept)", "Std. Error"]  
  parametricCI <- meanV2 + c(-1, 1) * se
  
  bootCI <- bootSpread(scores=d$ProbeDepth, weights=dsSummaryAll$ProportionAtDepth)
  data.frame(
    Top = max(d$ProbeDepth),
    MeanDepth = meanV1,
    MeanDepthV2 = meanV2,
    Bottom = min(d$ProbeDepth),
    ParametricSELower = parametricCI[1],
    ParametricSEUpper = parametricCI[2],
    BootSELower = bootCI[1],
    BootSEUpper = bootCI[2],
    BinCount = length(d$ProbeDepth)
  )
}

reportTheme <- theme_bw() +
  theme(axis.text = element_text(colour="gray40")) +
  theme(axis.title = element_text(colour="gray40")) +
  theme(panel.border = element_rect(colour="gray80")) +
  theme(axis.ticks = element_blank()) +
  theme(legend.text = element_text(colour="gray40")) +
  theme(legend.background = element_blank()) +
  theme(legend.key = element_blank()) +
  theme(legend.position=c(1,0), legend.justification=c(1,0))

############################
## @knitr LoadData
dsSummaryAll <- readRDS(pathInputSummaryBinAll)
dsProbeAll <- readRDS(pathInputProbeAll)
mcmcResultsAllChains <- load(pathMcmcResults)

############################
## @knitr TweakData

###
### Syncronize factor levels with aspects of graphing.
###
#For outliers
outlierLevels <- levels(dsSummaryAll$OutlierStatus)
shapeOutlier <- c(21, 24, 25)
testit::assert("The number of `OutlierStatus` levels should equal the number of assigned shapes (for plotting)", length(outlierLevels)==length(shapeOutlier))
names(shapeOutlier) <- outlierLevels
#For treatments

treatmentLevels <- levels(dsSummaryAll$Treatment)
treatmentLabelsLine <- gsub("\\B([A-Z])", "\n\\1", treatmentLevels, perl=TRUE);
treatmentLabelsSpace <- gsub("\\B([A-Z])", " \\1", treatmentLevels, perl=TRUE);
dsSummaryAll$TreatmentPretty <- factor(dsSummaryAll$Treatment, levels=treatmentLevels, labels=treatmentLabelsSpace)

testit::assert("The number of `Treatment` levels should equal the expected number of 5 (for plotting colors)", length(treatmentLevels)==5L)
treatmentPalette <- RColorBrewer::brewer.pal(n=length(treatmentLevels), name="Dark2")[c(2,1,3,4,5)]
#names(treatmentPalette) <- treatmentLevels
names(treatmentPalette) <- treatmentLabelsSpace
treatmentPaletteLight <- adjustcolor(treatmentPalette, alpha.f=.2)


###
### Exclude the outliers from the main datasets
###
dsSummary <- dsSummaryAll[dsSummaryAll$OutlierStatus == "Normal", ]
dsProbe <- dsProbeAll[dsProbeAll$OutlierStatus == "Normal", ]

#Examine individual treatments closely: dsSummary <- dsSummaryAll[dsSummaryAll$Treatment=="MediaControls", ] 
#Examine individual coupons closely: dsSummary <- dsSummaryAll[dsSummaryAll$CouponID==31, ] 

###
### Derive datasets where each record represents a coupon/treatment.
###
dsCouponAll <- plyr::ddply(dsSummaryAll, c("Treatment", "TreatmentPretty", "CouponID", "OutlierStatus"), SummarizeCorrosion)
dsCoupon <- plyr::ddply(dsSummary, c("Treatment", "TreatmentPretty", "CouponID", "OutlierStatus"), SummarizeCorrosion)
dsTreatment <- plyr::ddply(dsSummary, c("Treatment"), SummarizeCorrosion) #There will be six warning messages immediately below because the coupon weights aren't what hte bootstrap expects.
#Double-check ddply call above that uses weights: dsTreatment2 <- plyr::ddply(dsProbe, c("Treatment"), summarize, M=mean(ProbeDepth))

dsCouponAll <- dsCouponAll[order(dsCouponAll$OutlierStatus, dsCouponAll$CouponID, decreasing=F), ] #Sort so the jitter isn't affected when the outliers are removed
dsCoupon <- dsCoupon[order(dsCoupon$OutlierStatus, dsCoupon$CouponID, decreasing=F), ] #Sort so the jitter isn't affected when the outliers are removed
###
### Estimate the MLM coefficients
###
coefficients <- mcmcMlmCondensed$statistics[, "Mean"]

#Remember that the SD of the chain is equivalent to the SE of the parameter.
dsMlm <- data.frame(Treatment = NA_character_,
                    Effect = coefficients, 
                    SE = mcmcMlmCondensed$statistics[, "SD"],
                    SELower = mcmcMlmCondensed$quantiles[, "15.87%"],
                    SEUpper = mcmcMlmCondensed$quantiles[, "84.13%"]
                    )
dsMlm$Coefficient <- dsMlm$Effect
dsMlm[2:5, c("Effect", "SELower", "SEUpper")] <- dsMlm$Effect[1] + dsMlm[2:5, c("Effect", "SELower", "SEUpper")]

rownames(dsMlm)[rownames(dsMlm)=="(Intercept)"] <- "TreatmentMediaControls"
dsMlm$Treatment <- gsub("(Treatment)", replacement="", rownames(dsMlm), perl=TRUE)
dsMlm$Treatment <- factor(dsMlm$Treatment, levels=treatmentLevels, labels=treatmentLevels)
dsMlm$TreatmentPretty <- factor(dsMlm$Treatment, levels=treatmentLevels, labels=treatmentLabelsSpace)
# dsMlm

# grep("(?<=Treatment)(\\w+)", rownames(dsMlm), perl=TRUE, value=T);
# regmatches(rownames(dsMlm), regexpr("(?<=Treatment)(\\w+)", rownames(dsMlm), perl=TRUE));

# bootSpread(scores=dsSummary$ProbeDepth, weights=dsSummary$ProportionAtDepth)
# bootSpread(scores=dsSummary$ProbeDepth, weights=dsSummary$PercentageAtDepth)
# bootSpread(scores=dsProbe$ProbeDepth)

############################
## @knitr HistogramOverlay
g1 <- ggplot(dsSummary, aes(x=ProbeDepth, y=ProportionAtDepth, color=TreatmentPretty, group=CouponID)) +
#   geom_vline(data=dsTreatment, mapping=aes(xintercept=MeanDepth, color=Treatment, group=NULL)) +
#   geom_point(data=dsMlm, mapping=aes(x=Effect, y=.25, color=TreatmentPretty, fill=TreatmentPretty, group=NULL), shape=23, size=8) +
  geom_point(alpha=.2) +
  geom_line(alpha=.5) +
#   geom_rug(data=dsCoupon, mapping=aes(x=MeanDepth, y=NULL), sides="r") +
  scale_y_continuous(label=scales::percent) +
  scale_color_manual(values=treatmentPalette) +
  scale_fill_manual(values=treatmentPaletteLight) +
  coord_flip(xlim=c(-20, 0)) + #coord_flip() +
  reportTheme +
  guides(colour=guide_legend(title=NULL, override.aes=list(alpha=1, size=5))) +
  labs(x=expression(Probe*phantom(1)*Depth*phantom(1)*(mu*M)), y="Percent of Coupon's Probes at Depth")

g2 <- g1 + 
  geom_errorbarh(data=dsMlm, mapping=aes(x=Effect, xmin=SELower, xmax=SEUpper, y=.25, color=TreatmentPretty, group=NULL), size=1, alpha=.5, height=.05) +
  facet_grid(. ~ TreatmentPretty) +
  guides(color="none", fill="none") 

g3 <- g2 + coord_flip() 

g1
g2
g3

############################
## @knitr CouponSummaryBoxplot

set.seed(seed=9789) #Set a seed so the jittered graphs are consistent across renders.
gBoxAll <- ggplot(dsCouponAll, aes(x=TreatmentPretty, y=MeanDepth, color=TreatmentPretty, fill=TreatmentPretty, shape=OutlierStatus, label=CouponID)) +
#   geom_text(position=position_jitter(w = 0.2, h = 0), size=5, alpha=.5) +
  geom_boxplot(mapping=aes(shape=NULL), outlier.colour=NA, alpha=.1) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=8, fill="white", na.rm=T) + #See Chang (2013), Recipe 6.8.
  geom_point(position=position_jitter(w = 0.2, h = 0), size=5) +
  scale_x_discrete(labels=treatmentLabelsLine) +
  scale_color_manual(values=treatmentPalette) +
  scale_fill_manual(values=treatmentPaletteLight) +
  scale_shape_manual(values=shapeOutlier) +
  reportTheme +
  guides(color="none", fill="none", shape="none") +
  labs(title="Includes Five Outliers", x=NULL, y=expression(Probe*phantom(1)*Depth*phantom(1)*(mu*M)))
# gBoxAll

set.seed(seed=9789) #Set a seed so the jittered graphs are consistent across renders.
gBoxMost <- gBoxAll %+% 
  dsCoupon +
  labs(title="Excludes Five Outliers", y="")

gridExtra::grid.arrange(gBoxAll, gBoxMost, ncol=2, sub="Treatment")

############################
## @knitr MlmEstimates
#The MLM that estimates an intercept for the controls, and then offsets for the four treatments.
summary.MCMCglmm(resultsMCMCglmm[[1]])

mcmcMlmCondensed$statistics

mcmcMlmCondensed$quantiles

############################
## @knitr MlmCoefficients
cat("\n\n")
kable(dsMlm)
cat("\n\n")

############################
## @knitr OtherModels

### 
### This single-level model most closely resembles the reported MLM, and closely supports the MLM results.
### Instead of considering *each probe* on a coupon (which the MLM does), it considers only the mean of a coupon.
### 
#The MLM that estimates an intercept for the controls, and then offsets for the four treatments.
mFrequentist <- lmer(ProbeDepth ~ 1 + Treatment + (1 | CouponID), data=dsProbe) #
summary(mFrequentist) # fixef(mFrequentist) # ranef(mFrequentist) #seFixedEffects <- arm::se.fixef(mFrequentist)
# 
#The MLM that estimates each treatment mean separately (the zero indicates no grand intercept).
# mFrequentist0 <- lmer(ProbeDepth ~ 0 + Treatment + (1 | CouponID), data=dsSummary, weights=ProportionAtDepth) 
# summary(mFrequentist0)

### 
### This single-level model most closely resembles the reported MLM, and closely supports the MLM results.
### Instead of considering *each probe* on a coupon (which the MLM does), it considers only the mean of a coupon.
### 
mSingle <- lm(MeanDepth ~ 1 + Treatment, data=dsCoupon)
summary(mSingle)

### 
### This single-level model ignores the treatment variable (and thus the model is essentially the grand-mean).
### 
mNoTreatmentSingle <- lm(MeanDepth ~ 1, data=dsCoupon)
summary(mNoTreatmentSingle)

### 
### This multi-level model ignores the treatment variable (and becomes the grand-mean, but with intercepts estiamted for each coupon).
### 
mNoTreatmentMlm <- lmer(ProbeDepth ~ 1 + (1 | CouponID), data=dsProbe)
summary(mNoTreatmentMlm)
anova(m, mNoTreatmentMlm)

### 
### For the next two models, notice the dataset changes so that each probe has its own record (not each coupon)
### This 'No Pooling' model incorrectly assumes there's no dependencies between probes on the same coupon
### The standard errors are inappropriately small (Gelman & Hill, 2007, section 12).
### 
mNoPooling <- lm(ProbeDepth ~ 1 + Treatment + CouponID, data=dsProbe)
summary(mNoPooling)

###
### This 'Complete Pooling' model ignores any variabtion in depths between coupons (Gelman & Hill, 2007, subsection 12.3).
###
mCompletePooling <- lm(ProbeDepth ~ 1 + Treatment, data=dsProbe)
summary(mCompletePooling)
anova(mCompletePooling, mNoPooling)
