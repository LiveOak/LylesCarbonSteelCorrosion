#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.

############################
## @knitr LoadPackages
require(RODBC)
require(plyr)

############################
## @knitr DeclareGlobals
pathInputProfilometer <- "./Data/Raw/CouponPitDepth.csv"
pathOutputSummaryBinAll <- "./Data/Derived/SummaryBinAll.rds"
pathOutputProbeAll <- "./Data/Derived/ProbeAll.rds"

probesPerCoupon <- 2501L
couponIDDifferentMachine <- c(1, 18, 21, 43) #These coupons were processed by ConocoPhillips with a different machine.
couponIDExtreme <- c(50) 

# Their order determine how the lmer models compare the treaments
txLabels <- c("MediaControls", "AcetateOnly", "Methane", "SulfideAcetate", "SulfideOnly") 

outlierLevels <- c("Normal", "Conoco", "Extreme")
############################
## @knitr LoadData

dsSummaryAll <- read.csv(pathInputProfilometer, stringsAsFactors=FALSE)
# summary(dsSummaryAll)
# sapply(dsSummaryAll, class)
# sapply(dsSummaryAll, FUN=function(x){sum(is.na(x))})

############################
## @knitr TweakData

###
### Augment the dataset where each record represents a histogram bin from the profilometer.
###
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

#Remove some unnecessary variables
dsSummaryAll$PercentageAtDepth <- NULL
dsSummaryAll$ConocoMachine <- NULL
dsSummaryAll$ExtremeCoupon <- NULL

columnOrder <- c("CouponBinID", "CouponID", "Treatment", "ProbeDepth", "ProportionAtDepth", "OutlierStatus")
dsSummaryAll <- dsSummaryAll[, columnOrder]

###
### Augment the dataset where each record represents a histogram bin from the profilometer.
###
ExpandSummary <- function( d ) {
  probeCount <- round(d$ProportionAtDepth * probesPerCoupon)
  data.frame(
    CouponID = rep(d$CouponID, each=probeCount),
    Treatment = rep(d$Treatment, each=probeCount),
    ProbeDepth = rep(d$ProbeDepth, each=probeCount),
    OutlierStatus = rep(d$OutlierStatus, each=probeCount)
  )
}
dsProbe <- plyr::ddply(dsSummaryAll, .variables="CouponBinID", ExpandSummary)

############################
## @knitr WriteToDisk
saveRDS(object=dsSummaryAll, file=pathOutputSummaryBinAll, compress="xz")
saveRDS(object=dsProbe, file=pathOutputProbeAll, compress="xz")
