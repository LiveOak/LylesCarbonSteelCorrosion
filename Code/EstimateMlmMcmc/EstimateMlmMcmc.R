#This next line is run when the whole file is executed, but not when knitr calls individual chunks.
rm(list=ls(all=TRUE)) #Clear the memory for any variables set from any previous runs.

############################
## @knitr LoadPackages
require(coda, quietly=TRUE)
require(MCMCglmm, quietly=TRUE)
require(ggmcmc, quietly=TRUE) #Great new package: http://xavier-fim.net/packages/ggmcmc/

############################
## @knitr DeclareGlobals
pathInputProbeAll <- "./Data/Derived/ProbeAll.rds"
pathMcmcResults <-  "./Code/EstimateMlmMcmc/mcmcMlmResults.RData"

chainCount <- 8L #4L
iterationCount <- 10000L #5000L #The number used for estimate (ie, it doesn't include burn-in)
burninCount <- iterationCount / 4L #This quantity affects the adapation length (and hense time duration) of `jags.model`.
thinCount <- 1

############################
## @knitr LoadData
if( !file.exists(pathInputProbeAll) ) stop(paste0("The file '", pathInputProbeAll, "' could not be found.  Check the path.  For this to work correctly, the 'MReporting.Rproj' needed to be opend in RStudio.  Otherwise the working directory would not be set correctly."))
dsProbeAll <- readRDS(pathInputProbeAll)
rm(pathInputProbeAll)

############################
## @knitr TweakData
dsProbe <- dsProbeAll[dsProbeAll$OutlierStatus == "Normal", ]

############################
## @knitr PrepareMcmc

############################
## @knitr RunMCMCglmm

startTime1 <- Sys.time()
fMcmc <- function( x ) {
  MCMCglmm(fixed = ProbeDepth ~ 1 + Treatment,
           random = ~ CouponID,
           data = dsProbe, 
           nitt = iterationCount + burninCount,
           burnin = burninCount,
           thin = thinCount)
}
resultsMCMCglmm <- lapply(X=seq_len(chainCount), FUN=fMcmc)
(elapsed <- Sys.time() - startTime1)

mcC <- as.mcmc.list(lapply(resultsMCMCglmm, function(m){m$Sol}))
ggResults <- ggmcmc::ggs(mcC)

# coda::cumuplot(mcC, ask=FALSE)


summary.MCMCglmm(resultsMCMCglmm[[1]])
# (mcmcMlmCondensed <- summary(mcC, quantiles=c(.025, pnorm(-1), .25, .5, .75, pnorm(1), .975)))
(mcmcMlmCondensed <- summary(mcC, quantiles=c(.025, .1587, .25, .5, .75, .8413, .975)))

save(list=c("mcmcMlmCondensed", "resultsMCMCglmm", "ggResults"), file=pathMcmcResults)


cat("R-Hat:\n")
gelman.diag(mcC, autoburnin=FALSE) #This is R-hat; the burnin period is manually specified above, so turn off the auto argument. 
coda::gelman.plot(mcC)

cat("Effective Size:\n")
effectiveSize(mcC)

############################
## @knitr GraphJagsSelect
ggs_density(ggResults) + theme(legend.position=c(1, 1), legend.justification=c(1,1)) 
ggs_traceplot(ggResults) + theme(legend.position=c(0, 0), legend.justification=c(0,0)) 
ggs_running(ggResults) 
ggs_compare_partial(ggResults)
ggs_crosscorrelation(ggResults)

############################
## @knitr GraphJagsAll
ggs_Rhat(ggResults) 
ggs_geweke(ggResults)
ggs_caterpillar(ggResults)
ggs_crosscorrelation(ggResults) #Graph all the parameters first

# ggs_rocplot(ggResults, outcome = "ProbeDepth")
# Another feature of caterpillar plots is the possibility to plot two different models, and be able to easily compare between them. A list of two ggs() objects must be provided.
# ggs_caterpillar(list(A = ggs(s), B = ggs(s, par_labels = P)))

############################
## @knitr DisplayModels
fMcmc
