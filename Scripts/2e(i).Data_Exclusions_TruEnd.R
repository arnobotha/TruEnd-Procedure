# ===================================== DATA FUSION =====================================
# Applying exclusions, followed by engineering some basic features that must 
# precede any (non-clustered) subsampling. TruEnd-procedure applied.
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: Classifier Diagnostics
# SCRIPT AUTHOR(S): Dr Arno Botha

# DESCRIPTION:
# This script performs the following high-level tasks:
#   1) Apply preliminary exclusions and assess impact using event rate
#   2a) Removes a few variables that are unlikely to be useful within the context of 
#      analysing/modelling of default risk.
#   2b) Fuses macroeconomic data unto the main credit dataset
#   3a) Creates a preliminary target/outcome variable for modelling default risk,
#       itself used in tracking influence of exclusions
#   3b) Engineers a few basic features that require entire loan histories, given
#       the intended (non-clustered) subsampling scheme in script 3-series
# ---------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R
#   - 2a.Data_Prepare_Credit_Basic.R
#   - 2b.Date_Prepare_Credit_Advanced.R
#   - 2c(i).Data_Prepare_Credit_TruEnd.R
#   - 2d(i).Data_Enrich_TruEnd.R
#
# -- Inputs:
#   - datCredit_real | Prepared credit data from script 2d(i)
#
# -- Outputs:
#   - datCredit_real | enriched credit dataset, fused with base macroeconomic variables
# =======================================================================================




# ------- 1. Apply exclusions on the credit dataset to increase available memory

ptm <- proc.time() # for runtime calculations (ignore)

# - Confirm prepared datasets are loaded into memory
if (!exists('datCredit_real')) unpack.ffdf(paste0(genPath,"creditdata_final3-TruEnd"), tempPath)
if (!exists('datExclusions')) unpack.ffdf(paste0(genObjPath,"Exclusions-TruEnd"), tempPath)

# - Population-level prevalence rate and record tally before any Exclusions
# NOTE: choose default prior probability as measure for evaluating impact
classPrior_Pop <- datCredit_real[, sum(DefaultStatus1, na.rm=T)]/datCredit_real[!is.na(DefaultStatus1), .N]
recCount_start <- datCredit_real[,.N]

# - Add a starting line for Exclusions table
datExclusions <- rbind(data.table(Excl_ID=NA, Reason="Base dataset", Impact_Account=0, Impact_Dataset=0, Impact_records=0),
                       datExclusions)

# - Define data structure for vectors wherein impacts of Exclusions are to be captured
excl_count <- datExclusions[!is.na(Excl_ID), .N]; vecExcl <- datExclusions[!is.na(Excl_ID), Excl_ID]
recCount_impact <- rep(0, excl_count); classPrior_remainRecs <- copy(recCount_impact); 
recCount_remain <- copy(recCount_impact); excl_impactRelat <- copy(recCount_impact)

# - Iterate through each listed Exclusion and assess impact
for (i in 1:excl_count) {
  recCount_impact[i] <- datCredit_real[ExclusionID == vecExcl[i], .N]
  recCount_remain[i] <- datCredit_real[ExclusionID > vecExcl[i] | ExclusionID == 0, .N]
  # [SANITY CHECK] Does record tallies remain logical and correct as we progress through ExclusionIDs?
  if (recCount_remain[i] == recCount_start - sum(recCount_impact[1:i])) cat('SAFE\t') else cat('ERROR\t') # TRUE = safe
  # Impact on event prevalence (Default prior probability)
  classPrior_remainRecs[i] <- datCredit_real[ExclusionID > vecExcl[i] | ExclusionID == 0, sum(DefaultStatus1, na.rm=T)] /
    datCredit_real[ExclusionID > vecExcl[i] | ExclusionID == 0 & !is.na(DefaultStatus1), .N] 
  # Relative impact on dataset (row-wise) given proposed sequential application of exclusions
  excl_impactRelat[i] <- recCount_impact[i] / recCount_remain[i]
}

# - Enrich Exclusions Table with new fields
datExclusions[, Records_Remain := c(recCount_start, recCount_remain)]
datExclusions[, Impact_Dataset_Cumul := c(NA, percent(excl_impactRelat, accuracy = 0.001))]
datExclusions[, ClassPrior_Remain := percent(c(classPrior_Pop, classPrior_remainRecs), accuracy = 0.001)]
datExclusions[, classPrior_Remain_Diff := c(NA,percent(diff(c(classPrior_Pop,classPrior_remainRecs)), accuracy = 0.001))]

# - Store experimental objects | Memory optimisation
pack.ffdf(paste0(genObjPath,"Exclusions-TruEnd-Enriched"), datExclusions);

# - Check the impact of the exclusions from script 2d | RECORD-LEVEL
(exclusions_credit <- datCredit_real[ExclusionID != 0, .N] / datCredit_real[, .N] * 100)
# Exclusions' impact: 6.05%

# - Now apply the exclusions
datCredit_real <- subset(datCredit_real, ExclusionID == 0); gc()

# - Successful?
cat( (datCredit_real[ExclusionID > 0, .N] == 0) %?% "SAFE: Exclusions applied successfully.\n" %:%
       "WARNING: Some Exclusions failed to apply.\n")

# - Remove unnecessary variables
datCredit_real <- subset(datCredit_real, select = -c(ExclusionID))





# ------ 2. General cleanup & checks
# - Clean-up
rm(classPrior_Pop, recCount_start, recCount_impact, recCount_remain, classPrior_remainRecs, excl_impactRelat, vecExcl, excl_count)

# - Save to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genPath, "creditdata_final4-TruEnd"), datCredit_real); gc()
proc.time() - ptm # IGNORE: elapsed runtime
