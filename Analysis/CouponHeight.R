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
  mostlyTop <- quantreg::rq(ProbeHeight ~ 1, weights=ProportionAtHeight, tau=.97, data=d)
  
#   bootCI <- bootSpread(scores=d$ProbeHeight, weights=d$ProportionAtHeight)
#   seCI <- seSpread()
  data.frame(
    MinHeight = min(d$WeightedHeight),
    MeanHeight = sum(d$WeightedHeight) / sum(d$ProportionAtHeight),
    MaxHeight = max(d$WeightedHeight),

#     SELower
#     BootCILower = bootCI[1],
#     BootCIUpper = bootCI[2],
    Count = length(d$WeightedHeight)
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
ds <- read.csv(pathInput, stringsAsFactors=FALSE)

############################
## @knitr TweakData
ds$Treatment <- factor(ds$Treatment)
ds$CouponID <- factor(ds$CouponID)

# ds$PercentageAtHeight * 2501
ds$ProportionAtHeight <- ds$PercentageAtHeight / 100
ds$WeightedHeight <- ds$ProbeHeight * ds$ProportionAtHeight

# bootSpread(scores=ds$ProbeHeight, weights=round(2500*ds$ProportionAtHeight))

# mean(ds$ProbeHeight)
# mean(ds$ProbeHeight, weights=ds$ProportionAtHeight)
# 
# coef(lm(ProbeHeight ~ 1, data=ds, weights=ProportionAtHeight))
# summary(lm(ProbeHeight ~ 1 + Treatment, data=ds, weights=ProportionAtHeight))
# 
summary(lmer(ProbeHeight ~ 1 + Treatment + (1|CouponID), data=ds, weights=ds$ProportionAtHeight))

dsCoupon <- plyr::ddply(ds, c("Treatment", "CouponID"), SummarizeCoupon)

dsTreatment <- plyr::ddply(ds, c("Treatment"), SummarizeCoupon)


############################
## @knitr HistogramOverlay
g <- ggplot(ds, aes(x=ProbeHeight, y=ProportionAtHeight, color=Treatment, group=CouponID)) +
  geom_point(alpha=.2) +
  geom_line(alpha=.5) +
  scale_y_continuous(label=scales::percent) +
  scale_color_brewer(palette="Dark2") +
  coord_flip(xlim=c(0, 75)) +
  theme_bw() +
  theme(legend.position=c(1,1), legend.justification=c(1,1)) +
  guides(colour=guide_legend(override.aes=list(alpha=1, size=5))) +
  labs(x=expression(Probe*phantom(1)*Height*phantom(1)*(mu*M)), y="Percent of Coupon's Probes at Height")
g

g + 
  geom_point(data=dsTreatment, mapping=aes(x=MeanHeight, y=Inf, color=Treatment, group=NULL), hjust=2) +
  geom_vline(data=dsTreatment, mapping=aes(xintercept=MeanHeight, color=Treatment, group=NULL)) +
  facet_grid(. ~ Treatment) +
  guides(color="none", fill="none") 
  

############################
## @knitr CouponSummaryBoxplot
set.seed(seed=9789) #Set a seed so the jittered graphs are consistent across renders.
ggplot(dsCoupon, aes(x=Treatment, y=MeanHeight, color=Treatment, fill=Treatment)) +
  geom_boxplot(outlier.colour=NA, alpha=.1) +
  stat_summary(fun.y="mean", geom="point", shape=23, size=8, fill="white", na.rm=T) + #See Chang (2013), Recipe 6.8.
  geom_point(position=position_jitter(w = 0.2, h = 0), shape=1, size=5, alpha=.5) +
  scale_color_brewer(palette="Dark2") +
  scale_fill_brewer(palette="Dark2") +
  theme_bw() +
  guides(color="none", fill="none") +
  labs(y=expression(Probe*phantom(1)*Height*phantom(1)*(mu*M)))


############################
## @knitr AnalysisChunk03

