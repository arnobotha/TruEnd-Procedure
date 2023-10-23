# ================================== TruEnd-procedure ===================================
# Case-study: Applying the TruEnd-procedure on a very small set of real-world loans
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




# ------ 1. Preliminaries

# --- 0. Setup

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

# - Confirm extracted case study is loaded into memory
if (!exists('datGiven')) unpack.ffdf(paste0(genObjPath,"Extract_", caseStudy_Name), tempPath)

minLength <- 1 # minimum length (in months) of an isolated TZB-regime, by definition
tau <- 6 # length of non-TZB period that should precede an isolated TZB-regime, for M2-purposes

# - Define threshold vector for primary control variable: [Balance]
vThres <- c(0,10,25,50,75,100,150,200,250,300,400,500,750,1000,1500,2000,3000,4000,5000,7500,10000) # expanded search space

# - Define threshold vector for secondary/optional control variable: [Principal_Ratio]
# NOTE: If secondary control variable is to be deactivated completely, then assign a single 0-value to vThres2
# This should ensure the proper working of the internal logic within the TruEnd-functions
vThres2 <-  c(0, 0.005, 0.01, 0.015, 0.02)
vThres2 <-  c(0)

# - Calculate overall number of thresholds across both control variables
numThres <- length(vThres) * ( (length(vThres2) > 0) %?% length(vThres2) %:% 1)





# ------ 2. TruEnd-procedure
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
                                    id_cols=Time, names_from=LoanID, values_from=ControlVar))[,-1]

# - Abstract a secondary control variable [Balance-to-Principal] matrix from data
# NOTE: rows are periods (up to maximum observed period), columns are loan accounts
matControl2 <- as.matrix(pivot_wider(data=datGiven[order(get(accVar)),list(LoanID = get(accVar), Time = get(timeVar), ControlVar = get(controlVar2))], 
                                    id_cols=Time, names_from=LoanID, values_from=ControlVar))[,-1]

# - Abstract the balance matrix from data for calculation of M1 and M2 measures later
# NOTE: rows are periods (up to maximum observed period), columns are loan accounts
matBalance <- as.matrix(pivot_wider(data=datGiven[order(get(accVar)),list(LoanID = get(accVar), Time = get(timeVar), Balance = get(balanceVar))], 
                                    id_cols=Time, names_from=LoanID, values_from=Balance))[,-1]

# - Abstract the vector of observed maturities from data
vecMaturity <- datGiven[order(get(accVar)),list(Freq = max(get(timeVar),na.rm=T)), by=list(get(accVar))]$Freq




# --- 2a. Create a multithreaded setup for testing each threshold | Candidate Objective Function 1

# - Define objective function and associated argument list
# A premature t_z-point will produce larger M1-values, i.e., we should minimise M1, or equivalently, maximise -M1
# A delayed t_z-point will produce lower M2-values, i.e., we should maximise M2.
# Therefore, pursuing both optimisations imply maximising (M2 - M1)
# Best t_z given by maximising:
objFunc <- function(vM1, vM2) {sum(vM2-vM1, na.rm=T)}
args <- c("vM2", "vM1")

# - Setup & logging
ptm <- proc.time() #IGNORE: for computation time calculation
cl.port <- makeCluster(6); registerDoParallel(cl.port) # multi-threading setup
cat("New Job: Applying TruEnd-procedure ..", file=paste0("assesslog_", caseStudy_Name,".txt"), append=F)

# - Multithreaded for-loop
datResults <- foreach(it=1:numThres, .combine='rbind', .verbose=F, .inorder=T,
                      .packages='data.table', .export=c('TruEnd_outer', 'TruEnd_inner')) %dopar% {
                        # - testing conditions
                        # it <- 6

                        datResults.interim <- TruEnd_outer(matControl=matControl, thres=vThres[it %% length(vThres) + (it %% length(vThres) == 0)*length(vThres)], 
                                                           matControl2=matControl2, thres2 = vThres2[(floor((it-1) / length(vThres))+1)], 
                                                           controlVar=controlVar, controlVar2=controlVar2, tau=tau, matBalance=matBalance,
                                                           vecMaturity=vecMaturity, it=it, numThres=numThres, minLength=minLength, 
                                                           controlVar2VoidVal=controlVar2VoidVal, reportFlag=T, logName=caseStudy_Name,
                                                           objFunc=objFunc, args=args)
}
stopCluster(cl.port); tme <- proc.time() - ptm #IGNORE: for computation time calculation

# - Save to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genObjPath,"Results_Candidate1_", caseStudy_Name), datResults)

# - Logging
cat(paste0("\n END: TruEnd-procedure applied! Runtime: ", sprintf("%.1f", tme[3] / 60), " minutes"),
    file=paste0("assesslog_", caseStudy_Name,".txt"), append=T)


# --- 2b. Analytics: Main optimisation results: M1, M2, objective function across threshold b | Primary control variable only (B_t)

# - Prepare results dataset
datPlot <- pivot_longer(data=datResults[Threshold2==0,list(Threshold, M1_mean, M2_mean, Objective)], cols=M1_mean:Objective, 
                        names_to="Measure", values_to="Value") %>% as.data.table()
# - Graph results
ggplot(datPlot[Threshold<=2500, ], aes(x=Threshold, y=Value, group=Measure)) + theme_minimal() + 
  labs(y="Value", x=bquote("Threshold "*italic(b)), title="Candidate 1") + 
  geom_point(aes(colour=Measure, shape=Measure), size=1.5) + 
  geom_line(aes(colour=Measure, linetype=Measure), linewidth=1)




# --- 3a. Create a multithreaded setup for testing each threshold | Candidate Objective Function 2

# - Define objective function and associated argument list
# M2's domain will typically be much larger than that of M1.
# This implies that changes in M2 will affect f.obj1 much more than changes in M1,
# which complicates the optimisation of f.obj1 with regard to M1
# Therefore, define weights for each measure to downscale and upscale the influences of M2 and M1 respectively
# These weights can be preset and left outside of the optimisation itself, just as a practical expedient for now
w1 <- 1 # weight for M1 with its small domain
w2 <- 0.05 # weight for M2 with its large domain, should logically be < w1
# Best t_z given by maximising:
objFunc <- function(vM1, vM2, w1=w1, w2=w2) {sum(w2*vM2 - w1*vM1, na.rm=T)}
args <- c("vM2", "vM1", "w1", "w2")

# - Setup & logging
ptm <- proc.time() #IGNORE: for computation time calculation
cl.port <- makeCluster(6); registerDoParallel(cl.port) # multi-threading setup
cat("New Job: Applying TruEnd-procedure ..", file=paste0("assesslog_", caseStudy_Name,".txt"), append=F)

# - Multithreaded for-loop
datResults2 <- foreach(it=1:numThres, .combine='rbind', .verbose=F, .inorder=T,
                      .packages='data.table', .export=c('TruEnd_outer', 'TruEnd_inner')) %dopar% {
                        # - testing conditions
                        # it <- 21
                        
                        datResults.interim <- TruEnd_outer(matControl=matControl, thres=vThres[it %% length(vThres) + (it %% length(vThres) == 0)*length(vThres)], 
                                                           matControl2=matControl2, thres2 = vThres2[(floor((it-1) / length(vThres))+1)], 
                                                           controlVar=controlVar, controlVar2=controlVar2, tau=tau, matBalance=matBalance,
                                                           vecMaturity=vecMaturity, it=it, numThres=numThres, minLength=minLength, 
                                                           controlVar2VoidVal=controlVar2VoidVal, reportFlag=T, logName=caseStudy_Name,
                                                           objFunc=objFunc, args=args, w1=w1, w2=w2)
                      }
stopCluster(cl.port); tme <- proc.time() - ptm #IGNORE: for computation time calculation

# - Save to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genObjPath,"Results_Candidate2_", caseStudy_Name), datResults2)

# - Logging
cat(paste0("\n END: TruEnd-procedure applied! Runtime: ", sprintf("%.1f", tme[3] / 60), " minutes"),
    file=paste0("assesslog_", caseStudy_Name,".txt"), append=T)


# --- 3b. Analytics: Main optimisation results: M1, M2, objective function across threshold b | Primary control variable only (B_t)

# - Prepare results dataset
datPlot <- pivot_longer(data=datResults2[Threshold2==0,list(Threshold, M1_mean, M2_mean, Objective)], cols=M1_mean:Objective, 
                        names_to="Measure", values_to="Value") %>% as.data.table()
# - Graph results
ggplot(datPlot[Threshold<=2500, ], aes(x=Threshold, y=Value, group=Measure)) + theme_minimal() + 
  labs(y="Value", x=bquote("Threshold "*italic(b)), title="Candidate 2a") + 
  geom_point(aes(colour=Measure, shape=Measure), size=1.5) + 
  geom_line(aes(colour=Measure, linetype=Measure), linewidth=1)




# --- 4a. Create a multithreaded setup for testing each threshold | Candidate Objective Function 2b

# - Define objective function and associated argument list
# M2's domain will typically be much larger than that of M1.
# This implies that changes in M2 will affect f.obj1 much more than changes in M1,
# which complicates the optimisation of f.obj1 with regard to M1
# Therefore, define weights for each measure to downscale and upscale the influences of M2 and M1 respectively
# These weights can be preset and left outside of the optimisation itself, just as a practical expedient for now
w1 <- 1 # weight for M1 with its small domain
w2 <- 0.05 # weight for M2 with its large domain, should logically be < w1
# Best t_z given by maximising:
objFunc <- function(vM1, vM2, w1=w1, w2=w2) {sum(w2*vM2 - w1*vM1, na.rm=T)/sd(w2*vM2 - w1*vM1, na.rm=T)}
args <- c("vM2", "vM1", "w1", "w2")

# - Setup & logging
ptm <- proc.time() #IGNORE: for computation time calculation
cl.port <- makeCluster(6); registerDoParallel(cl.port) # multi-threading setup
cat("New Job: Applying TruEnd-procedure ..", file=paste0("assesslog_", caseStudy_Name,".txt"), append=F)

# - Multithreaded for-loop
datResults2b <- foreach(it=1:numThres, .combine='rbind', .verbose=F, .inorder=T,
                      .packages='data.table', .export=c('TruEnd_outer', 'TruEnd_inner')) %dopar% {
                        # - testing conditions
                        # it <- 21
                        
                        datResults.interim <- TruEnd_outer(matControl=matControl, thres=vThres[it %% length(vThres) + (it %% length(vThres) == 0)*length(vThres)], 
                                                           matControl2=matControl2, thres2 = vThres2[(floor((it-1) / length(vThres))+1)], 
                                                           controlVar=controlVar, controlVar2=controlVar2, tau=tau, matBalance=matBalance,
                                                           vecMaturity=vecMaturity, it=it, numThres=numThres, minLength=minLength, 
                                                           controlVar2VoidVal=controlVar2VoidVal, reportFlag=T, logName=caseStudy_Name,
                                                           objFunc=objFunc, args=args, w1=w1, w2=w2)
                      }
stopCluster(cl.port); tme <- proc.time() - ptm #IGNORE: for computation time calculation

# - Save to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genObjPath,"Results_Candidate2b_", caseStudy_Name), datResults2b)

# - Logging
cat(paste0("\n END: TruEnd-procedure applied! Runtime: ", sprintf("%.1f", tme[3] / 60), " minutes"),
    file=paste0("assesslog_", caseStudy_Name,".txt"), append=T)


# --- 4b. Analytics: Main optimisation results: M1, M2, objective function across threshold b | Primary control variable only (B_t)

# - Prepare results dataset
datPlot <- pivot_longer(data=datResults2b[Threshold2==0,list(Threshold, M1_mean, M2_mean, Objective)], cols=M1_mean:Objective, 
                        names_to="Measure", values_to="Value") %>% as.data.table()
# - Graph results
ggplot(datPlot[Threshold<=2500, ], aes(x=Threshold, y=Value, group=Measure)) + theme_minimal() + 
  labs(y="Value", x=bquote("Threshold "*italic(b)), title="Candidate 2b") + 
  geom_point(aes(colour=Measure, shape=Measure), size=1.5) + 
  geom_line(aes(colour=Measure, linetype=Measure), linewidth=1)
datPlot[Measure=="Objective",]

# - Graph only M1 and Objective function
ggplot(datPlot[Threshold<=2500 & Measure != "M2_mean", ], aes(x=Threshold, y=Value, group=Measure)) + theme_minimal() + 
  labs(y="Value", x=bquote("Threshold "*italic(b)), title="Candidate 2b") + 
  geom_point(aes(colour=Measure, shape=Measure), size=1.5) + 
  geom_line(aes(colour=Measure, linetype=Measure), linewidth=1)



# --- 5a. Create a multithreaded setup for testing each threshold | Candidate Objective Function 2c

# - Define objective function and associated argument list
# M2's domain will typically be much larger than that of M1.
# This implies that changes in M2 will affect f.obj1 much more than changes in M1,
# which complicates the optimisation of f.obj1 with regard to M1
# Therefore, define weights for each measure to downscale and upscale the influences of M2 and M1 respectively
# These weights can be preset and left outside of the optimisation itself, just as a practical expedient for now
w1 <- 1 # weight for M1 with its small domain
w2 <- 0.05 # weight for M2 with its large domain, should logically be < w1
# Best t_z given by maximising:
objFunc <- function(vM1, vM2, w1=w1, w2=(vM1 / (vM1 + vM2))) {sum(w2*vM2 - w1*vM1, na.rm=T)/sd(w2*vM2 - w1*vM1, na.rm=T)}
args <- c("vM2", "vM1", "w1")

# - Setup & logging
ptm <- proc.time() #IGNORE: for computation time calculation
cl.port <- makeCluster(6); registerDoParallel(cl.port) # multi-threading setup
cat("New Job: Applying TruEnd-procedure ..", file=paste0("assesslog_", caseStudy_Name,".txt"), append=F)

# - Multithreaded for-loop
datResults2c <- foreach(it=1:numThres, .combine='rbind', .verbose=F, .inorder=T,
                        .packages='data.table', .export=c('TruEnd_outer', 'TruEnd_inner')) %dopar% {
                          # - testing conditions
                          # it <- 21
                          
                          datResults.interim <- TruEnd_outer(matControl=matControl, thres=vThres[it %% length(vThres) + (it %% length(vThres) == 0)*length(vThres)], 
                                                             matControl2=matControl2, thres2 = vThres2[(floor((it-1) / length(vThres))+1)], 
                                                             controlVar=controlVar, controlVar2=controlVar2, tau=tau, matBalance=matBalance,
                                                             vecMaturity=vecMaturity, it=it, numThres=numThres, minLength=minLength, 
                                                             controlVar2VoidVal=controlVar2VoidVal, reportFlag=T, logName=caseStudy_Name,
                                                             objFunc=objFunc, args=args, w1=w1)
                        }
stopCluster(cl.port); tme <- proc.time() - ptm #IGNORE: for computation time calculation

# - Save to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genObjPath,"Results_Candidate2c_", caseStudy_Name), datResults2c)

# - Logging
cat(paste0("\n END: TruEnd-procedure applied! Runtime: ", sprintf("%.1f", tme[3] / 60), " minutes"),
    file=paste0("assesslog_", caseStudy_Name,".txt"), append=T)


# --- 4b. Analytics: Main optimisation results: M1, M2, objective function across threshold b | Primary control variable only (B_t)

# - Prepare results dataset
datPlot <- pivot_longer(data=datResults2c[Threshold2==0,list(Threshold, M1_mean, M2_mean, Objective)], cols=M1_mean:Objective, 
                        names_to="Measure", values_to="Value") %>% as.data.table()
# - Graph results
ggplot(datPlot[Threshold<=2500, ], aes(x=Threshold, y=Value, group=Measure)) + theme_minimal() + 
  labs(y="Value", x=bquote("Threshold "*italic(b)), title="Candidate 2c") + 
  geom_point(aes(colour=Measure, shape=Measure), size=1.5) + 
  geom_line(aes(colour=Measure, linetype=Measure), linewidth=1)
datPlot[Measure=="Objective",]

# - Graph only M1 and Objective function
ggplot(datPlot[Threshold<=2500 & Measure != "M2_mean", ], aes(x=Threshold, y=Value, group=Measure)) + theme_minimal() + 
  labs(y="Value", x=bquote("Threshold "*italic(b)), title="Candidate 2b") + 
  geom_point(aes(colour=Measure, shape=Measure), size=1.5) + 
  geom_line(aes(colour=Measure, linetype=Measure), linewidth=1)




# --- 6a. Create a multithreaded setup for testing each threshold | Candidate Objective Function 3

# - Define objective function and associated argument list
# Calculating the 'contamination' degree to which M1 is contaminated by M2
# Best t_z given by maximising:
objFunc <- function(vM1, vM2) {sum(vM1 / (vM1 + vM2), na.rm=T)}
args <- c("vM2", "vM1")

# - Setup & logging
ptm <- proc.time() #IGNORE: for computation time calculation
cl.port <- makeCluster(6); registerDoParallel(cl.port) # multi-threading setup
cat("New Job: Applying TruEnd-procedure ..", file=paste0("assesslog_", caseStudy_Name,".txt"), append=F)

# - Multithreaded for-loop
datResults3 <- foreach(it=1:numThres, .combine='rbind', .verbose=F, .inorder=T,
                       .packages='data.table', .export=c('TruEnd_outer', 'TruEnd_inner')) %dopar% {
                         # - testing conditions
                         # it <- 21
                         
                         datResults.interim <- TruEnd_outer(matControl=matControl, thres=vThres[it %% length(vThres) + (it %% length(vThres) == 0)*length(vThres)], 
                                                            matControl2=matControl2, thres2 = vThres2[(floor((it-1) / length(vThres))+1)], 
                                                            controlVar=controlVar, controlVar2=controlVar2, tau=tau, matBalance=matBalance,
                                                            vecMaturity=vecMaturity, it=it, numThres=numThres, minLength=minLength, 
                                                            controlVar2VoidVal=controlVar2VoidVal, reportFlag=T, logName=caseStudy_Name,
                                                            objFunc=objFunc, args=args)
                       }
stopCluster(cl.port); tme <- proc.time() - ptm #IGNORE: for computation time calculation

# - Save to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genObjPath,"Results_Candidate3_", caseStudy_Name), datResults2)

# - Logging
cat(paste0("\n END: TruEnd-procedure applied! Runtime: ", sprintf("%.1f", tme[3] / 60), " minutes"),
    file=paste0("assesslog_", caseStudy_Name,".txt"), append=T)



# --- 6b. Analytics: Main optimisation results: M1, M2, objective function across threshold b | Primary control variable only (B_t)

# - Prepare results dataset
datPlot <- pivot_longer(data=datResults3[Threshold2==0,list(Threshold, M1_mean, M2_mean, Objective)], cols=M1_mean:Objective, 
                        names_to="Measure", values_to="Value") %>% as.data.table()
datPlot[Measure=="Objective",]

# - Graph results
ggplot(datPlot[Threshold<=2500, ], aes(x=Threshold, y=Value, group=Measure)) + theme_minimal() + 
  labs(y="Value", x=bquote("Threshold "*italic(b)), title="Candidate 3") + 
  geom_point(aes(colour=Measure, shape=Measure), size=1.5) + 
  geom_line(aes(colour=Measure, linetype=Measure), linewidth=1)

# - Graph only M1 and Objective function
ggplot(datPlot[Threshold<=2500 & Measure != "M2_mean", ], aes(x=Threshold, y=Value, group=Measure)) + theme_minimal() + 
  labs(y="Value", x=bquote("Threshold "*italic(b)), title="Candidate 2b") + 
  geom_point(aes(colour=Measure, shape=Measure), size=1.5) + 
  geom_line(aes(colour=Measure, linetype=Measure), linewidth=1)


# --- Cleanup
rm(matBalance, matControl, matControl2, vecMaturity, datResults, datResults2, datResults2b, datResults2c, datResults3, datPlot, datGiven)
