# ================================== TruEnd-procedure ===================================
# Case-study: Applying the TruEnd-procedure on a small set of real-world loans
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: TruEnd-procedure
# SCRIPT AUTHOR(S): Dr Arno Botha
# ---------------------------------------------------------------------------------------
# DESCRIPTION:
# Having extracted a few loans, we apply TruEnd by iterating across a given vector of 
#   thresholds for a chosen control variable (Balance), within a multithreaded setup. 
#   For each iteration, we examine the results on a few aspects (e.g., prevalence of TZB)
# ---------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R
#   - 2a.Data_Prepare_Credit_Basic.R
#   - 2b.Date_Prepare_Credit_Advanced.R

# -- Inputs:
#   - datCredit_real | Enhanced version of input dataset (script 2b)
#
# -- Outputs:
#   - <analytics>
#   - <optimisation results>
# =======================================================================================




# ------ 1. Analysis

# --- 0. Preliminaries

caseStudy_Name <- "2Loan"

# - Confirm raw data is loaded into memory
if (!exists('datCredit_real')) unpack.ffdf(paste0(genPath,"creditdata_final1c"), tempPath)

# - Filter specific example
datGiven <- subset(datCredit_real, ExclusionID == 0 & LoanID %in% c(3000006071745, 3000000109309),
                   select=c("LoanID", "Counter", "Date", "Principal", "Balance", "WOff_Ind"))

# - memory optimisation
rm(datCredit_real); gc()

# - Create Balance-to-Principal ratio as a candidate control variable
datGiven[Principal > 0, Principal_Ratio := Balance / Principal]

# - Save snapshot for external experimentation
write_xlsx(datGiven, paste0(genObjPath, "Extract_", caseStudy_Name, ".xlsx"))

# - Save to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genObjPath,"Extract_", caseStudy_Name), datGiven)

# --- 1. Parameters for TruEnd-procedure
minLength <- 1 # minimum length (in months) of an isolated TZB-regime, by definition
tau <- 6 # length of non-TZB period that should precede an isolated TZB-regime, for M2-purposes

# - Define threshold vector for primary control variable: [Balance]
thres.v <- c(0,10,25,50,75,100,150,200,250,300,400,500,750,1000,1500,2000,3000,4000,5000,7500,10000) # expanded search space
thres.v <- c(0,10,25,50,75,100,150,200,250,300,400,500)

# - Define threshold vector for secondary/optional control variable: [Principal_Ratio]
# NOTE: If secondary control variable is discarded, then thres2.v should have a single 0-value
# This should ensure the proper working of the internal logic within the TruEnd-functions
thres2.v <-  c(0, 0.005, 0.01, 0.015, 0.02)

# - Calculate overall number of thresholds
numThres <- length(thres.v) * ( (length(thres2.v) > 0) %?% length(thres2.v) %:% 1)