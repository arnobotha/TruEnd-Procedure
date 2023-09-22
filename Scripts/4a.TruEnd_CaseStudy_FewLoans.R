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





# -------- TruEnd-procedure
# Isolating possible TZB-regimes of minimum length [minLength] for a given threshold [thres]
#  for a given control variable [contVar] across given credit history, 
#  indexed by account [accVar] and time [timeVar]. Inputs include:
# [controlVar]: field name within given dataset, e.g., Balance B_t; Principal_Ratio R_t.
# [controlVar2]: field name within given dataset for a second (optional) control variable, e.g., Balance B_t; Principal_Ratio R_t.
# [balanceVar]: field name for balance/exposure field (can be same as [controlVar]).
# [accVar]: field name for part of the composite key (loan ID)
# [timeVar]: field name for part of the composite key (counter)

# - testing conditions
controlVar <- "Balance"; balanceVar <- "Balance"
controlVar2 <- "Principal_Ratio"
controlVar2VoidVal <- 0 # threshold value at which secondary control variable is voided/discarded when finding TZB-regimes
accVar <- "LoanID"
timeVar <- "Counter"


# --- 1. Preliminaries
# NOTE: This part simply prepares some data and would become redundant when generalising 
# this script into a callable function, given this data.

# - Abstract the control variable [Balance] matrix from data
# NOTE: rows are periods (up to maximum observed period), columns are loan accounts
matControl <- as.matrix(pivot_wider(data=datGiven[order(get(accVar)),list(LoanID = get(accVar), Time = get(timeVar), ControlVar = get(controlVar))], 
                                    id_cols=LoanID:Time, names_from=LoanID, values_from=ControlVar))[,-1]

# - Abstract a secondary control variable [Balance-to-Principal] matrix from data
# NOTE: rows are periods (up to maximum observed period), columns are loan accounts
matControl2 <- as.matrix(pivot_wider(data=datGiven[order(get(accVar)),list(LoanID = get(accVar), Time = get(timeVar), ControlVar = get(controlVar2))], 
                                    id_cols=LoanID:Time, names_from=LoanID, values_from=ControlVar))[,-1]

# - Abstract the balance matrix from data for calculation of M1 and M2 measures later
# NOTE: rows are periods (up to maximum observed period), columns are loan accounts
matBalance <- as.matrix(pivot_wider(data=datGiven[order(get(accVar)),list(LoanID = get(accVar), Time = get(timeVar), Balance = get(balanceVar))], 
                                    id_cols=LoanID:Time, names_from=LoanID, values_from=Balance))[,-1]

# - Abstract the number of accounts from data
nAcc <- ncol(matControl)

# - Abstract the vector of observed maturities from data
vecMaturity <- datGiven[order(get(accVar)),list(Freq = max(get(timeVar),na.rm=T)), by=list(get(accVar))]$Freq

# - Compile the TruEnd-suite of evaluation (and auxiliary) functions
source(paste0(path_cust,'TruEnd.R'))




# --- 2. Create a multithreaded setup for testing each threshold

# - Setup & logging
ptm <- proc.time() #IGNORE: for computation time calculation
cl.port <- makeCluster(6); registerDoParallel(cl.port) # multi-threading setup
cat("New Job: Applying TruEnd-procedure ..", file=paste0("assesslog_", caseStudy_Name,".txt"), append=F)

# - Multithreaded for-loop
datResults <- foreach(it=1:numThres, .combine='rbind', .verbose=F, .inorder=T,
                      .packages='data.table', .export=c('TruEnd_outer', 'TruEnd_inner')) %dopar% {
                        # - testing conditions
                        # it <- 21

                        datResults.interim <- TruEnd_outer(matControl=matControl, thres=thres.v[it %% length(thres.v) + (it %% length(thres.v) == 0)*length(thres.v)], 
                                                           matControl2=matControl2, thres2 = thres2.v[(floor((it-1) / length(thres.v))+1)], 
                                                           controlVar=controlVar, controlVar2=controlVar2, tau=tau, matBalance=matBalance,
                                                           vecMaturity=vecMaturity, it=it, numThres=numThres, minLength=minLength, 
                                                           controlVar2VoidVal=controlVar2VoidVal, reportFlag=T, logName=caseStudy_Name,
                                                           f.obj=function(M1.v, M2.v) {mean(M2.v-M1.v)}, args=list(M2.v,M1.v))
                      }
stopCluster(cl.port); tme <- proc.time() - ptm #IGNORE: for computation time calculation

# - Save to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genObjPath,"Results_", caseStudy_Name), datResults)

# - Logging
cat(paste0("\n END: TruEnd-procedure applied! Runtime: ", sprintf("%.1f", tme[3] / 60), " minutes"),
    file=paste0("assesslog_", caseStudy_Name,".txt"), append=T)

### ARNO: Passing of a given objective function odes not yet work in the code above. Fix.

f.obj=function(M1.v, M2.v) {mean(M2.v-M1.v)}
args=list(M2.v,M1.v)
do.call(f.obj, args=args)

# --- 3a. Analytics: Main optimisation results: M1, M2, objective function across threshold b | Primary control variable only (B_t)

# - Prepare results dataset
datPlot <- pivot_longer(data=datResults[Threshold2==0,list(Threshold, M1_mean, M2_mean, Objective)], cols=M1_mean:Objective, 
                        names_to="Measure", values_to="Value") %>% as.data.table()
# - Graph results
ggplot(datPlot, aes(x=Threshold, y=Value, group=Measure)) + theme_minimal() + 
  labs(y="Value", x=bquote("Threshold "*italic(b))) + 
  geom_point(aes(colour=Measure, shape=Measure), size=1.5) + 
  geom_line(aes(colour=Measure, linetype=Measure), size=1)


# --- 3b. Analytics: Account ages & length of isolated TZB-regime across threshold b | Primary control variable only (B_t)
# - Prepare results dataset
datPlot <- pivot_longer(data=datResults[Threshold2==0,list(Threshold, FalseEnd_mean, TruEnd_mean, TZB_Length_mean)], 
                        cols=FalseEnd_mean:TZB_Length_mean, names_to="Measure", values_to="Value") %>% as.data.table()
datPlot[, Facet := ifelse(Measure=="TZB_Length_mean", "b. TZB-regime lengths", "a. Account ages")]

# - Graph results
ggplot(datPlot, aes(x=Threshold, y=Value, group=Measure)) + theme_minimal() + 
  labs(y="Value", x=bquote("Threshold "*italic(b))) + 
  theme(legend.position="bottom",
        strip.background = element_rect(fill="snow2", colour="snow2"),
        strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
  geom_point(aes(colour=Measure, shape=Measure), size=1.5) + 
  geom_line(aes(colour=Measure, linetype=Measure), size=1) + 
  facet_grid(Facet~., scales="free")


# --- 3c. Analytics: Prevalence of TZB-regimes across threshold b | Primary control variable only (B_t)
# - Graph results
ggplot(datResults[Threshold2==0,], aes(x=Threshold, y=TZB_prevalence)) + theme_minimal() + 
  labs(y="Portfolio-wide TZB-prevalence (%) amongst all accounts", x=bquote("Threshold "*italic(b))) + 
  theme(legend.position="bottom") + 
  geom_point(size=1.5) + geom_line(size=1) + 
  scale_y_continuous(labels=percent)


# --- 3d. Analytics: True ending balances across threshold b | Primary control variable only (B_t)
# - Graph results
ggplot(datResults[Threshold2==0,], aes(x=Threshold, y=TruBal_mean)) + theme_minimal() + 
  labs(y="True ending balance (R)", x=bquote("Threshold "*italic(b))) + 
  theme(legend.position="bottom") + 
  geom_point(size=1.5) + geom_line(size=1) + 
  scale_y_continuous(labels=comma)





# Arno: try to think about the objective function. Want to show a bump somehow at b=250. 
#   Maybe incorporate length of TZB as penalization? Or divide (M2- M1) by something?


# --- Cleanup
rm(matBalance, matControl, datResults)