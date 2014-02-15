#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.

############################
## @knitr LoadPackages
require(knitr) #For literate programming
require(RColorBrewer) #For the color palettes
require(plyr) #For manipulating datsets
require(ggplot2) #For graphing
require(boot) #For bootstrapped CIs
require(lme4) #For multilevel modeling (MLM)
require(arm) #For manipulating MLM standard errors
# require(quantreg) #For quantile regression

############################
## @knitr DeclareGlobals
pathInput <- "./Data/Raw/CouponPitDepth.csv"
couponIDDifferentMachine <- c(1, 18, 21, 43) #These coupons were processed by ConocoPhillips with a different machine.
couponIDExtreme <- c(50) 

outlierLevels <- c("Normal", "Conoco", "Extreme")
shapeOutlier <- c(21, 24, 25)
names(shapeOutlier) <- outlierLevels

txLabels <- c("MediaControls", "AcetateOnly", "Methane", "SulfideAcetate", "SulfideOnly")
txPalette <- RColorBrewer::brewer.pal(n=length(txLabels), name="Dark2")[c(2,1,3,4,5)]
names(txPalette) <- txLabels
txPaletteLight <- adjustcolor(txPalette, alpha.f=.2)

bootSpread <- function( scores, weights=NULL, conf=.68 ) {
  plugin <- function( d, i ) { mean(d[i]) }  
  distribution <- boot(data=scores, plugin, weights=weights, R=99)
  
  # I'm using the percentile instead of the BCa, because the acceleration can't be estimated with these scores and weights for some reason.  I'm not using them in the real anlaysis anyway.
  ci <- boot.ci(distribution, type = c("perc"), conf=conf)
  return( ci$percent[4:5] )
}
# seSpread <- function( scores ) { return( mean(scores) + c(-1, 1)*sd(scores)/sqrt(length(scores)) ) }
SummarizeCorrosion <- function( d ) {
#   mostlyTop <- quantreg::rq(ProbeDepth ~ 1, weights=ProportionAtDepth, tau=.97, data=d)
  
  weightedModel <- lm(ProbeDepth ~ 1, data=d, weights=ProportionAtDepth)
  
  meanV1 <- sum(d$ProbeDepth * d$ProportionAtDepth) / sum(d$ProportionAtDepth)
  meanV2 <- coef(summary(weightedModel))["(Intercept)", "Estimate"]
  se <- coef(summary(weightedModel))["(Intercept)", "Std. Error"]
  
  parametricCI <- meanV2 + c(-1, 1) * se
  
  bootCI <- bootSpread(scores=d$ProbeDepth, weights=dsSummaryAll$ProportionAtDepth)
#   summary(lm(ProbeDepth ~ 1, data=d, weights=ProportionAtDepth))
#   seCI <- seSpread()
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

############################
## @knitr LoadData
dsSummaryAll <- read.csv(pathInput, stringsAsFactors=FALSE)
# summary(dsSummaryAll)
# sapply(dsSummaryAll, class)
# sapply(dsSummaryAll, FUN=function(x){sum(is.na(x))})

############################
## @knitr TweakData
dsSummaryAll$ProbeDepth <- -dsSummaryAll$ProbeDepth
dsSummaryAll$CouponBinID <- seq_along(dsSummaryAll$CouponID)
dsSummaryAll$ProportionAtDepth <- dsSummaryAll$PercentageAtDepth / 100
dsSummaryAll$Treatment <- factor(dsSummaryAll$Treatment, labels=txLabels, levels=txLabels) # dput(levels(dsSummaryAll$Treatment))
dsSummaryAll$ConocoMachine <- (dsSummaryAll$CouponID %in% couponIDDifferentMachine)
dsSummaryAll$ExtremeCoupon <- (dsSummaryAll$CouponID %in% couponIDExtreme)
dsSummaryAll$CouponID <- factor(dsSummaryAll$CouponID)

dsSummaryAll$OutlierStatus <- factor(1L, levels=seq_along(outlierLevels), labels=outlierLevels)
dsSummaryAll$OutlierStatus[dsSummaryAll$ConocoMachine] <- "Conoco"
dsSummaryAll$OutlierStatus[dsSummaryAll$ExtremeCoupon] <- "Extreme"

dsSummary <- dsSummaryAll[!dsSummaryAll$ConocoMachine & !dsSummaryAll$ExtremeCoupon, ]
#Examine individual treatments closely: dsSummary <- dsSummary[dsSummary$Treatment=="MediaControls", ] 
#Examine individual coupons closely: dsSummary <- dsSummary[dsSummary$CouponID==31, ] 

ExpandSummary <- function( d ) {
  probeCount <- round(d$ProportionAtDepth * 2501L)
  data.frame(
    Treatment = rep(d$Treatment, each=probeCount) ,
    CouponID = rep(d$CouponID, each=probeCount),
    ConocoMachine = rep(d$ConocoMachine, each=probeCount),
    ProbeDepth = rep(d$ProbeDepth, each=probeCount)
  )
}
dsProbe <- plyr::ddply(dsSummary, .variables="CouponBinID", ExpandSummary)
# sapply(dsProbe, class)
# dsSummary$PercentageAtDepth * 2501

# weightedModel <- lm(ProbeDepth ~ 1, data=dsSummary, weights=ProportionAtDepth)
# coef(summary(weightedModel))["(Intercept)", "Std. Error"]
# sum(dsSummary$ProbeDepth * dsSummary$ProportionAtDepth) / sum(dsSummary$ProportionAtDepth)

#The MLM that estimates an intercept for the controls, and then offsets for the four treatments.
m <- lmer(ProbeDepth ~ 1 + Treatment + (1 | CouponID), data=dsProbe) # summary(m) # fixef(m) # ranef(m)
seFixedEffects <- arm::se.fixef(m)


#The MLM that estimates each treatment mean separately (the zero indicates no grand intercept).
m0 <- lmer(ProbeDepth ~ 0 + Treatment + (1 | CouponID), data=dsSummary, weights=ProportionAtDepth) # summary(m0)
#The model below is slower than the weighted version; identical means, but different the standard errors
# m0 <- lmer(ProbeDepth ~ 0 + Treatment + (1 | CouponID), data=dsProbe) # summary(m0)
# dsMlmCoupon <- data.frame(CouponID=rownames(ranef(m0)$CouponID), MeanMlm=ranef(m0)$CouponID[, 1]) 
# str(m0@resp)
# str(m0@frame)
# str(m0@pp)
# 
# m0@frame$CouponID
# 
# coef(m)$CouponID[, 1]

dsMlm <- data.frame(Effect=fixef(m0), SE=seFixedEffects)
dsMlm$Treatment <- gsub("(Treatment)", replacement="", rownames(dsMlm), perl=TRUE)
dsMlm$Treatment <- factor(dsMlm$Treatment, labels=txLabels, levels=txLabels)
dsMlm$SELower <- dsMlm$Effect - dsMlm$SE
dsMlm$SEUpper <- dsMlm$Effect + dsMlm$SE
# rownames(dsMlm) <- seq_along(rownames(dsMlm))

dsCouponAll <- plyr::ddply(dsSummaryAll, c("Treatment", "CouponID", "OutlierStatus"), SummarizeCorrosion)
dsCoupon <- plyr::ddply(dsSummary, c("Treatment", "CouponID", "OutlierStatus"), SummarizeCorrosion)
# dsCoupon <- merge(x=dsCoupon, y=dsMlmCoupon, by="CouponID")
# dsCoupon$MeanDepth #unique(m0@resp$mu)

# sapply(dsCoupon, class)
dsTreatment <- plyr::ddply(dsSummary, c("Treatment"), SummarizeCorrosion)
#Double-check ddply call above that uses weights: dsTreatment2 <- plyr::ddply(dsProbe, c("Treatment"), summarize, M=mean(ProbeDepth))

# bootSpread(scores=dsSummary$ProbeDepth, weights=dsSummary$ProportionAtDepth)
# bootSpread(scores=dsSummary$ProbeDepth, weights=dsSummary$PercentageAtDepth)
# bootSpread(scores=dsProbe$ProbeDepth)

############################
## @knitr HistogramOverlay
g1 <- ggplot(dsSummary, aes(x=ProbeDepth, y=ProportionAtDepth, color=Treatment, group=CouponID)) +
  geom_point(data=dsMlm, mapping=aes(x=Effect, y=.25, color=Treatment, fill=Treatment, group=NULL), shape=23, size=8) +
  geom_point(alpha=.2) +
  geom_line(alpha=.5) +
  geom_rug(data=dsCoupon, mapping=aes(x=MeanDepth, y=NULL), sides="r") +
  scale_y_continuous(label=scales::percent) +
  scale_color_manual(values=txPalette) +
  scale_fill_manual(values=txPaletteLight) +
  coord_flip(xlim=c(-20, 0)) + #coord_flip() +
  theme_bw() +
  theme(legend.position=c(1,0), legend.justification=c(1,0)) +
  guides(colour=guide_legend(override.aes=list(alpha=1, size=5))) +
  labs(x=expression(Probe*phantom(1)*Depth*phantom(1)*(mu*M)), y="Percent of Coupon's Probes at Depth")
g2 <- g1 + 
  geom_errorbarh(data=dsMlm, mapping=aes(x=Effect, xmin=SELower, xmax=SEUpper, y=.25, color=Treatment, group=NULL), size=1, alpha=.5, height=.05) +
  facet_grid(. ~ Treatment) +
  guides(color="none", fill="none") 
g3 <- g2 + coord_flip() 

g1
g2
g3

# ggplot(dsSummary, aes(x=ProbeDepth, y=ProportionAtDepth, color=CouponID, group=CouponID)) +
#   geom_point(alpha=.2) +
#   geom_line(alpha=.5) +
#   geom_rug(data=dsCoupon, mapping=aes(x=MeanDepth, y=NULL), sides="r") +
# #   coord_flip(xlim=c(-20, 0)) +
#   coord_flip() +
#   theme_bw() +
#   theme(legend.position="none")

# g + 
#   geom_point(data=dsTreatment, mapping=aes(x=MeanDepth, y=Inf, color=Treatment, group=NULL), hjust=2) +
#   geom_vline(data=dsTreatment, mapping=aes(xintercept=MeanDepth, color=Treatment, group=NULL)) +
#   facet_grid(. ~ Treatment) +
#   guides(color="none", fill="none") 
#   

############################
## @knitr CouponSummaryBoxplot

set.seed(seed=9789) #Set a seed so the jittered graphs are consistent across renders.
gBox <- ggplot(dsCouponAll, aes(x=Treatment, y=MeanDepth, color=Treatment, fill=Treatment, shape=OutlierStatus, label=CouponID)) +
#   geom_text(position=position_jitter(w = 0.2, h = 0), size=5, alpha=.5) +
  geom_boxplot(mapping=aes(shape=NULL), outlier.colour=NA, alpha=.1) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=8, fill="white", na.rm=T) + #See Chang (2013), Recipe 6.8.
  geom_point(position=position_jitter(w = 0.2, h = 0), size=5) +
  scale_color_manual(values=txPalette) +
  scale_fill_manual(values=txPaletteLight) +
  scale_shape_manual(values=shapeOutlier) +
  theme_bw() +
  guides(color="none", fill="none", shape="none") +
  labs(y=expression(Probe*phantom(1)*Depth*phantom(1)*(mu*M)))
gBox

set.seed(seed=9789) #Set a seed so the jittered graphs are consistent across renders.
gBox %+% dsCoupon

############################
## @knitr MlmEstimates
#The MLM that estimates an intercept for the controls, and then offsets for the four treatments.
summary(m)

############################
## @knitr MlmCoefficients
cat("\n\n")
kable(dsMlm)
cat("\n\n")

############################
## @knitr OtherModels

### 
### This single-level model most closely resembles the reported MLM, and closely supports the MLM results.
### Instead of considering each probe on a coupon (which the MLM does), it considers only the coupon's mean.
### 
mSingle <- lm(MeanDepth ~ 1 + Treatment, data=dsCoupon)
summary(mSingle)

### 
### This single-level model ignores the treatment variable (and thus the model is essentially the grand-mean).
### 
mNoTreatmentSingle <- lm(MeanDepth ~ 1, data=dsCoupon)
summary(mNoTreatmentSingle)
anova(mCompletePooling, mNoTreatmentSingle)

### 
### This multi-level model ignores the treatment variable (and becomes the grand-mean, but with intercepts estiamted for each coupon).
### 
mNoTreatmentMlm <- lmer(ProbeDepth ~ 1 + (1 | CouponID), data=dsProbe)
summary(mNoTreatmentMlm)
anova(m, mNoTreatmentMlm)

### 
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
