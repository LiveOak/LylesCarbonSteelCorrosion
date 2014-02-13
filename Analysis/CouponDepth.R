#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.

############################
## @knitr LoadPackages
require(RODBC)
require(plyr)
require(ggplot2)
require(quantreg)
require(boot)
require(lme4)
############################
## @knitr DeclareGlobals
pathInput <- "./Data/Raw/CouponPitDepth.csv"

SummarizeCoupon <- function( d ) {
  mostlyTop <- quantreg::rq(ProbeDepth ~ 1, weights=ProportionAtDepth, tau=.97, data=d)
  
#   bootCI <- bootSpread(scores=d$ProbeDepth, weights=d$ProportionAtDepth)
#   seCI <- seSpread()
  data.frame(
    MinDepth = min(d$WeightedDepth),
    MeanDepth = sum(d$WeightedDepth) / sum(d$ProportionAtDepth),
    MaxDepth = max(d$WeightedDepth),

#     SELower
#     BootCILower = bootCI[1],
#     BootCIUpper = bootCI[2],
    Count = length(d$WeightedDepth)
  )
}
# seSpread <- function( scores ) { return( mean(scores) + c(-1, 1)*sd(scores)/sqrt(length(scores)) ) }
# bootSpread <- function( scores, weights=NULL, conf=.68 ) {
#   plugin <- function( d, i ) { mean(d[i]) }
#   
#   distribution <- boot(data=scores, plugin, R=99, weights=weights)
#   ci <- boot.ci(distribution, type = c("bca"), conf=conf)
#   return( ci$bca[4:5] )
# }
############################
## @knitr LoadData
dsSummary <- read.csv(pathInput, stringsAsFactors=FALSE)
# summary(dsSummary)
# sapply(dsSummary, class)
# sapply(dsSummary, FUN=function(x){sum(is.na(x))})
############################
## @knitr TweakData
dsSummary$ID <- seq_along(dsSummary$CouponID)
dsSummary$ProportionAtDepth <- dsSummary$PercentageAtDepth / 100
dsSummary$Treatment <- factor(dsSummary$Treatment)
dsSummary$CouponID <- factor(dsSummary$CouponID)

ExpandSummary <- function( d ) {
  probeCount <- round(d$ProportionAtDepth * 2501)
  data.frame(
    Treatment = rep(d$Treatment, each=probeCount),
    CouponID = rep(d$CouponID, each=probeCount),
    ProbeDepth = rep(d$ProbeDepth, each=probeCount)
  )
}
dsProbe <- plyr::ddply(dsSummary, .variables="ID", ExpandSummary)
# ds$PercentageAtDepth * 2501

dsSummary$WeightedDepth <- dsSummary$ProbeDepth * dsSummary$ProportionAtDepth

# bootSpread(scores=ds$ProbeDepth, weights=round(2500*ds$ProportionAtDepth))

# mean(ds$ProbeDepth)
# mean(ds$ProbeDepth, weights=ds$ProportionAtDepth)
# 
# coef(lm(ProbeDepth ~ 1, data=ds, weights=ProportionAtDepth))
# summary(lm(ProbeDepth ~ 1 + Treatment, data=ds, weights=ProportionAtDepth))
# 
m <- lmer(ProbeDepth ~ 1 + Treatment + (1|CouponID), data=dsProbe)
summary(m)
seFixedEffects <- arm::se.fixef(m)

# m <-lmer(ProbeDepth ~ 1 + Treatment + (1|CouponID), data=dsSummary, weights=ProportionAtDepth)
# summary(m)
# arm::se.fixef(m)
m0 <- lmer(ProbeDepth ~ 0 + Treatment + (1|CouponID), data=dsSummary, weights=ProportionAtDepth)


dsMlm <- data.frame(Effect=fixef(m0), SE=seFixedEffects)
dsMlm$Treatment <- gsub("(Treatment)", replacement="", rownames(dsMlm), perl=TRUE);
dsMlm$CILower <- dsMlm$Effect - dsMlm$SE
dsMlm$CIUpper <- dsMlm$Effect + dsMlm$SE

# grep("(?<=Treatment)(\\w+)\\b", names(f), perl=TRUE, value=TRUE);
# gsub("Treatment(\\w+)", replacement="\\2", names(f), perl=TRUE);
  
dsCoupon <- plyr::ddply(dsSummary, c("Treatment", "CouponID"), SummarizeCoupon)
dsTreatment <- plyr::ddply(dsSummary, c("Treatment"), SummarizeCoupon)

# dsTreatment2 <- plyr::ddply(dsProbe, c("Treatment"), summarize, M=mean(ProbeDepth))


############################
## @knitr HistogramOverlay
g <- ggplot(dsSummary, aes(x=ProbeDepth, y=ProportionAtDepth, color=Treatment, group=CouponID)) +
  geom_point(data=dsMlm, mapping=aes(x=Effect, y=.25, color=Treatment, group=NULL), shape=23, size=4, fill="white", alpha=.5) +
  geom_point(alpha=.2) +
  geom_line(alpha=.5) +
  geom_rug(data=dsCoupon, mapping=aes(x=MeanDepth, y=NULL), sides="r") +
  scale_y_continuous(label=scales::percent) +
  scale_color_brewer(palette="Dark2") +
  coord_flip(xlim=c(0, 75)) +
  theme_bw() +
  theme(legend.position=c(1,1), legend.justification=c(1,1)) +
  guides(colour=guide_legend(override.aes=list(alpha=1, size=5))) +
  labs(x=expression(Probe*phantom(1)*Depth*phantom(1)*(mu*M)), y="Percent of Coupon's Probes at Depth")
g

g + 
  geom_errorbarh(data=dsMlm, mapping=aes(x=Effect, xmin=CILower, xmax=CIUpper, y=.25, color=Treatment, group=NULL), size=1, alpha=.5, height=.05) +
  facet_grid(. ~ Treatment) +
  guides(color="none", fill="none") 


# g + 
#   geom_point(data=dsTreatment, mapping=aes(x=MeanDepth, y=Inf, color=Treatment, group=NULL), hjust=2) +
#   geom_vline(data=dsTreatment, mapping=aes(xintercept=MeanDepth, color=Treatment, group=NULL)) +
#   facet_grid(. ~ Treatment) +
#   guides(color="none", fill="none") 
#   

############################
## @knitr CouponSummaryBoxplot
set.seed(seed=9789) #Set a seed so the jittered graphs are consistent across renders.
ggplot(dsCoupon, aes(x=Treatment, y=MeanDepth, color=Treatment, fill=Treatment)) +
  geom_boxplot(outlier.colour=NA, alpha=.1) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=8, fill="white", na.rm=T) + #See Chang (2013), Recipe 6.8.
  geom_point(position=position_jitter(w = 0.2, h = 0), shape=1, size=5, alpha=.5) +
  scale_color_brewer(palette="Dark2") +
  scale_fill_brewer(palette="Dark2") +
  theme_bw() +
  guides(color="none", fill="none") +
  labs(y=expression(Probe*phantom(1)*Depth*phantom(1)*(mu*M)))


############################
## @knitr AnalysisChunk03

