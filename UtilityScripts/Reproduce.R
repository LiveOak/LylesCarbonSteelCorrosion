
###################################
### Reproducible Research
###################################
# When executed by R, this file will manipulate the original data sources (ie, ZZZZ)
# to produce a groomed dataset suitable for analysis and graphing.

###################################
# Clear memory from previous runs
base::rm(list=base::ls(all=TRUE))

###################################
# Verify the working directory has been set correctly.  Much of the code assumes the working directory is the repository's root directory.
if( base::basename(base::getwd()) != "LylesCarbonSteelCorrosion" ) {
  base::stop("The working directory should be set to the root of the package/repository.  ",
       "It's currently set to `", base::getwd(), "`.")
}
###################################
# Install the necessary packages.
pathInstallPackages <- "./UtilityScripts/InstallPackages.R"
if( !file.exists(pathInstallPackages)) {
  base::stop("The file `", pathInstallPackages, "` was not found.  Make sure the working directory is set to the root of the repository.")
}
base::source(pathInstallPackages, local=new.env()) 

base::rm(pathInstallPackages)
###################################
# Load the necessary packages.
base::require(base)
base::require(knitr)
base::require(markdown)
base::require(testit)

###################################
# Declare the paths of the necessary files.

# The raw/input data files:
pathInputProfilometer <- "./Data/Raw/CouponPitDepth.csv" #Each record is a bin on the histogram

# The derived/intermediate data files (which are produced by the repository's code files):
pathDerivedSummaryBinAll <- "./Data/Derived/SummaryBinAll.rds"
pathDerivedProbeAll <- "./Data/Derived/ProbeAll.rds"

# Code Files:
pathAugment <- "./Code/AugmentData.R"
pathMcmc <- "./EstimateMlmMcmc/EstimateMlmMcmc.R"
pathAnalysis <- "./Analysis/CouponDepth.R"

#Report Files:
pathsReports <- base::file.path("./Analysis", c("CouponDepth.Rmd"))

###################################
# Verify the necessary path can be found.

# The raw/input data files:
testit::assert("The profilometer data should exist.", base::file.exists(pathInputProfilometer))

# Code Files:
testit::assert("The file that restructures the data should exist.", base::file.exists(pathAugment))
testit::assert("The file that runs the MCMC model should exist.", base::file.exists(pathMcmc))
testit::assert("The file that analyzes all the models should exist.", base::file.exists(pathAnalysis))

#Report Files:
testit::assert("The knitr Rmd files should exist.", base::file.exists(pathsReports))

###################################
# Run the files that manipulate and analyze.

# Execute code that restructures the Census data
base::source(pathAugment, local=base::new.env())

# Assert that the intermediate files exist (the two files produced by `AugmentData.R`)
testit::assert("The augmented profilometer records should exist.", base::file.exists(pathDerivedSummaryBinAll))
testit::assert("The profilometer-expanded records should exist.", base::file.exists(pathDerivedProbeAll))

#Execute code that analyzes & graphs the models
base::source(pathAnalysis, local=base::new.env())

# ###################################
# # Build the reports
# for( pathRmd in pathsReports ) {
#   pathMd <- base::gsub(pattern=".Rmd$", replacement=".md", x=pathRmd)
#   pathHtml <- base::gsub(pattern=".Rmd$", replacement=".html", x=pathRmd)
#   knitr::knit(input=pathRmd, output=pathMd)
#   markdown::markdownToHTML(file=pathMd, output=pathHtml)
# }
