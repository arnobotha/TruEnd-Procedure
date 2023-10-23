# ================================== TruEnd-procedure ===================================
# Case-study: Applying the TruEnd-procedure on a set of real-world loans
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

caseStudy_Name <- "SmallSamp"

# - Confirm raw data is loaded into memory
if (!exists('datCredit_real')) unpack.ffdf(paste0(genPath,"creditdata_final1c"), tempPath)

# - Get list of unique loans from which we will subsample down to a specific size
vAccs <- unique(datCredit_real[ExclusionID==0, LoanID])
subSampSize <- 10000; subSampSize_Prop <- subSampSize / length(vAccs)

# - Subsample accounts down to fixed size
vAccs_samp <- data.table(LoanID=vAccs) %>% slice_sample(prop=subSampSize_Prop)

# - Subsample main credit dataset accordingly
datGiven <- subset(datCredit_real, ExclusionID == 0 & LoanID %in% vAccs_samp$LoanID,
                   select=c("LoanID", "Counter", "Date", "Principal", "Balance", "WOff_Ind"))

# - memory optimisation
rm(datCredit_real, vAccs, vAccs_samp); gc()

# - Create Balance-to-Principal ratio as a candidate control variable
datGiven[Principal > 0, Principal_Ratio := Balance / Principal]

# - Save to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genPath,"Extract_", caseStudy_Name), datGiven)




# --- 1. Parameters for TruEnd-procedure

# - Confirm extracted case study is loaded into memory
if (!exists('datGiven')) unpack.ffdf(paste0(genPath,"Extract_", caseStudy_Name), tempPath)

minLength <- 1 # minimum length (in months) of an isolated TZB-regime, by definition
tau <- 6 # length of non-TZB period that should precede an isolated TZB-regime, for M2-purposes

# - Define threshold vector for primary control variable: [Balance]
vThres <- c(0,10,25,50,75,100,150,200,250,300,400,500,750,1000,1250,1500,1750,2000,2500,3000,4000,5000,7500,10000) # expanded search space

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



# --- 2a. Create a multithreaded setup for testing each threshold | 
# This section is run interactively across various candidate objective functions; see results-section for brief log

# - Define objective function and associated argument list
# M2's domain will typically be much larger than that of M1.
# This implies that changes in M2 will affect f.obj1 much more than changes in M1,
# which complicates the optimisation of f.obj1 with regard to M1
# Therefore, define weights for each measure to downscale and upscale the influences of M2 and M1 respectively
# These weights can be preset and left outside of the optimisation itself, just as a practical expedient for now
w1 <- 1 # weight for M1 with its small domain
w2 <- 0.0007394088 # weight for M2 with its large domain, should logically be < w1
# Best t_z given by maximising:
objFunc <- function(vM1, vM2, w1=w1, w2=w2) {
  # - Candidate 1a
  # sum(w2*vM2 - w1*vM1, na.rm=T)
  # - Candidate 1b
  #sum(w2*vM2 - w1*vM1, na.rm=T)  / sd(w2*vM2 - w1*vM1, na.rm=T)
  # - Candidate 1c
  sum( ( (w2*vM2 - w1*vM1)  ) / sd(w2*vM2 - w1*vM1, na.rm=T), na.rm=T )
  # - Candidate 2a
  #sum( ( (w2*vM2 - w1*vM1) - mean(w2*vM2 - w1*vM1, na.rm=T)  ) / sd(w2*vM2 - w1*vM1, na.rm=T), na.rm=T )
  # - Candidates 3a-c
  #w2=vM1/(vM1 + vM2)
  #w2=mean(vM1,na.rm=T)/(mean(vM1,na.rm=T) + mean(vM2,na.rm=T))
  #w2= max(mean(vM1,na.rm=T)/(mean(vM1,na.rm=T) + mean(vM2,na.rm=T)), w2)
  #sum( ( (w2*vM2 - w1*vM1)  ) / sd(w2*vM2 - w1*vM1, na.rm=T), na.rm=T )
  }
args <- c("vM2", "vM1", "w1", "w2")

# - Setup & logging
ptm <- proc.time() #IGNORE: for computation time calculation
cl.port <- makeCluster(6); registerDoParallel(cl.port) # multi-threading setup
cat("New Job: Applying TruEnd-procedure ..", file=paste0("assesslog_", caseStudy_Name,".txt"), append=F)

# - Multithreaded for-loop
datResults <- foreach(it=1:numThres, .combine='rbind', .verbose=F, .inorder=T,
                       .packages='data.table', .export=c('TruEnd_outer', 'TruEnd_inner')) %dopar% {
                         # - testing conditions
                         # it <- 9
                         
                         datResults.interim <- TruEnd_outer(matControl=matControl, thres=vThres[it %% length(vThres) + (it %% length(vThres) == 0)*length(vThres)], 
                                                            matControl2=matControl2, thres2 = vThres2[(floor((it-1) / length(vThres))+1)], 
                                                            controlVar=controlVar, controlVar2=controlVar2, tau=tau, matBalance=matBalance,
                                                            vecMaturity=vecMaturity, it=it, numThres=numThres, minLength=minLength, 
                                                            controlVar2VoidVal=controlVar2VoidVal, reportFlag=T, logName=caseStudy_Name,
                                                            objFunc=objFunc, args=args, w1=w1, w2=w2)
                       }
stopCluster(cl.port); tme <- proc.time() - ptm #IGNORE: for computation time calculation

# - Save to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genObjPath,"Results_Candidate2_", caseStudy_Name), datResults)

# - Logging
cat(paste0("\n END: TruEnd-procedure applied! Runtime: ", sprintf("%.1f", tme[3] / 60), " minutes"),
    file=paste0("assesslog_", caseStudy_Name,".txt"), append=T)


# - Plot main results for primary control variable (B_t)
datPlot <- subset(datResults, Threshold2==0)
ggplot(datPlot, aes(x=Threshold, y=Objective)) + theme_minimal() + 
  labs(x=bquote("Threshold "*italic(b)), y=bquote("Objective function value "*italic(f(b))) ) +
  geom_point(size=1.5) + geom_line(linewidth=1)

# - Get optima
cat("Minimum of f: ", comma(min(datPlot$Objective, na.rm=T)), " found at b=", 
    datPlot$Threshold[which(datPlot$Objective == min(datPlot$Objective, na.rm=T))], "\nMaximum of f: ",
    comma(max(datPlot$Objective, na.rm=T)), " found at b=", 
    datPlot$Threshold[which(datPlot$Objective == max(datPlot$Objective, na.rm=T))])

### RESULTS (Various candidate objective functions) on 10k-sample:
# -- 1a: sum(w2*vM2 - w1*vM1, na.rm=T)
#   Achieves maximasation at b=1000 with intuitively downward sloping curve
# -- 1b: sum(w2*vM2 - w1*vM1, na.rm=T)  / sd(w2*vM2 - w1*vM1, na.rm=T)
#   Improves upon 1a by accounting for inherent variance in vectors within the objective function
#   Achieves best maximisation result at b=300 (with w2=0.0007).
#   The drawback is the need for specifying w2 at all
# -- 1c: sum( ( (w2*vM2 - w1*vM1)  ) / sd(w2*vM2 - w1*vM1, na.rm=T), na.rm=T )
#   Exactly the same curve as 1b, but mathematically more tractable/defensible 
# -- 2a: sum( ( (w2*vM2 - w1*vM1) - mean(w2*vM2 - w1*vM1, na.rm=T)  ) / sd(w2*vM2 - w1*vM1, na.rm=T), na.rm=T )
#   Standardising [(X-mu)/sd] the vector within the objective function had a nasty effect, with the curve breaking down across all thresholds b
# -- 3a-b: sum( ( (w2*vM2 - w1*vM1)  ) / sd(w2*vM2 - w1*vM1, na.rm=T), na.rm=T )
#   In an effort to avoid specifying w2, one can perhaps use the contamination degree, which will change given the b-threshold
# - a: w2=vM1/(vM1 + vM2)
#   However, optimisation fails to isolate suspected point (b=250), instead showing b=0 to be 'optimal'
# - b: w2=mean(vM1,na.rm=T)/(mean(vM1,na.rm=T) + mean(vM2,na.rm=T))
#   Since the portfolio-level mean contamination degree looked promising and displayed a suitable scale, it may serve as a weight
#   The curve had a drastic drop from b=0, but then reached a (local) maximum near b=1250, after which it tapered off. 
#   This is promising but not the expected result and would mean justifying throwing away very small thresholds; untenable
# - c: max(mean(vM1,na.rm=T)/(mean(vM1,na.rm=T) + mean(vM2,na.rm=T)), w2)
#   Imposing a minimum value of w2 (or whatever) will ensure that the near-zero (or zero) values in contamination degrees for small 
#   thresholds do not ruin the optimisation. W2 is itself then obtain as the average between highest and lowest mean contamination degree
#   Works well! However, the floor (w2) is triggered overwhelming across most thresholds, with only the very larger thresholds not needing it
#   This calls in question the utility of calculating w2 within this function at all, when it can be prespecified instead
### CONCLUSION
# 1c seems best, but specify w2 beforehand as the average between highest and lowest contamination degrees at the portfolio-level

datPlot[, list(Threshold, Objective)]

### AB: Start creating proper analytics for article



# ------ 3. Analytics

# - Confirm extracted case study is loaded into memory
if (!exists('datResults')) unpack.ffdf(paste0(genObjPath,"Results_Candidate2_", caseStudy_Name), tempPath)

# --- 1. Main optimisation results: M1, M2, objective function across threshold b | Primary control variable only (B_t)

# - Prepare results dataset
datPlot <- pivot_longer(data=datResults[Threshold2==0,list(Threshold, M1_mean, M2_mean, Objective)], cols=M1_mean:Objective, 
                        names_to="Measure", values_to="Value") %>% as.data.table()
# - Graph results
ggplot(datPlot, aes(x=Threshold, y=Value, group=Measure)) + theme_minimal() + 
  labs(y="Value", x=bquote("Threshold "*italic(b)), title="Candidate 2") + 
  geom_point(aes(colour=Measure, shape=Measure), size=1.5) + 
  geom_line(aes(colour=Measure, linetype=Measure), linewidth=1)

# - Graph M1 & objective
ggplot(datPlot[Measure %in% c("M1_mean", "Objective")], aes(x=Threshold, y=Value, group=Measure)) + theme_minimal() + 
  labs(y="Value", x=bquote("Threshold "*italic(b)), title="Candidate 2") + 
  geom_point(aes(colour=Measure, shape=Measure), size=1.5) + 
  geom_line(aes(colour=Measure, linetype=Measure), linewidth=1)



# --- 2. Portfolio-level mean contamination degree in M1 (due to M2) across threshold b | Primary control variable only (B_t)
ggplot(datResults[Threshold2==0,], aes(x=Threshold, y=(M1_mean / (M1_mean+M2_mean)))) + theme_minimal() + 
  labs(y=bquote("Prtfolio-level contamination degree (%) in "*italic(M[1])), x=bquote("Threshold "*italic(b))) + 
  theme(legend.position="bottom") + 
  geom_point(size=1.5) + geom_line(size=1) + 
  scale_y_continuous(labels=percent)
(datExp <- datResults[Threshold2==0, list(Threshold, Contam= M1_mean / (M1_mean+M2_mean))])
mean(datExp[Contam>0, Contam]) # influenced by choice of thresholds, not reliably summary
datExp[Contam>0, (Contam[.N] - Contam[1])/2] # average value between highest & lowest contamination degree | Seems plausible



# --- 3. Account ages & length of isolated TZB-regime across threshold b | Primary control variable only (B_t)
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



# --- 4. Prevalence of TZB-regimes across threshold b | Primary control variable only (B_t)
ggplot(datResults[Threshold2==0,], aes(x=Threshold, y=TZB_prevalence)) + theme_minimal() + 
  labs(y="Portfolio-wide TZB-prevalence (%) amongst all accounts", x=bquote("Threshold "*italic(b))) + 
  theme(legend.position="bottom") + 
  geom_point(size=1.5) + geom_line(size=1) + 
  scale_y_continuous(labels=percent)
datResults[Threshold2==0 & Threshold==250,]



# --- 5. True ending balances across threshold b | Primary control variable only (B_t)
ggplot(datResults[Threshold2==0,], aes(x=Threshold, y=TruBal_mean)) + theme_minimal() + 
  labs(y="True ending balance (R)", x=bquote("Threshold "*italic(b))) + 
  theme(legend.position="bottom") + 
  geom_point(size=1.5) + geom_line(size=1) + 
  scale_y_continuous(labels=comma)



# --- Cleanup
rm(matBalance, matControl, matControl2, vecMaturity, datResults, datPlot, datGiven, datExp)