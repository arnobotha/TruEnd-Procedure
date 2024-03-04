# ================================== TruEnd-procedure ===================================
# Case-study: Applying the TruEnd-procedure on a small set of real-world loans
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: TruEnd-procedure
# SCRIPT AUTHOR(S): Dr Arno Botha
# ---------------------------------------------------------------------------------------
# DESCRIPTION:
# Having extracted a few loans, we apply TruEnd by iterating across a given vector of 
#   thresholds for a chosen control variable (Balance), within a multithreaded setup. 
#   For each iteration, we examine the results on a few aspects (e.g., prevalence of TZB),
#    as performed within the custom TruEnd_outer() function.
#   Afterwards, we graph the implied loss function across all iterations as the main result.
#   Ancillary results across all iterations also include: 1) mean contamination degree;
#     2) portfolio-level TZB-prevalence rate; 3) mean account ages
# ---------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R
#   - 2a.Data_Prepare_Credit_Basic.R
#   - 2b.Date_Prepare_Credit_Advanced.R

# -- Inputs:
#   - datCredit_real | Enhanced version of input dataset (script 2b)
#   - TruEnd_outer | Custom function (TruEnd.R)
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
set.seed(3)
vAccs_samp <- data.table(LoanID=vAccs) %>% slice_sample(prop=subSampSize_Prop)

# - Subsample main credit dataset accordingly
datGiven <- subset(datCredit_real, ExclusionID == 0 & LoanID %in% vAccs_samp$LoanID,
                   select=c("LoanID", "Counter", "Age_Adj", "Date", "Principal", "Balance", "WOff_Ind"))

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
ageVar <- "Age_Adj"


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
vMaturity <- datGiven[order(get(accVar)),list(Freq = max(get(ageVar),na.rm=T)), by=list(get(accVar))]$Freq

# - Abstract a vector of vector sizes (number of elements within balance matrices per account)
vSize <- datGiven[order(get(accVar)),list(Freq = max(get(timeVar),na.rm=T)), by=list(get(accVar))]$Freq



# --- 2a. Create a multithreaded setup for testing each threshold
# This section is run interactively across various candidate objective functions; see results-section for brief log

# - Define objective function and associated argument list
# M2's domain will typically be much larger than that of M1.
# This implies that changes in M2 will affect f.obj1 much more than changes in M1,
# which complicates the optimisation of f.obj1 with regard to M1
# Therefore, define weights for each measure to downscale and scale the influences of M2 and M1 respectively
# These weights can be preset and left outside of the optimisation itself, just as a practical expedient for now
w1 <- 1 # weight for M1 with its small domain
w2 <- 0.0006362424 # weight for M2 with its large domain, should logically be < w1
# Best t_z given by maximising:
objFunc <- function(vM1, vM2, w1=w1, w2=w2) {
  # - Candidate 1a
  # sum(w2*vM2 - w1*vM1, na.rm=T)
  # - Candidate 1b
  #sum(w2*vM2 - w1*vM1, na.rm=T)  / sd(w2*vM2 - w1*vM1, na.rm=T)
  # - Candidate 1c
  sum( (w2*vM2 - w1*vM1), na.rm=T ) / sd(w2*vM2 - w1*vM1, na.rm=T)
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
                         # it <- 10
                         
                         datResults.interim <- TruEnd_outer(matControl=matControl, thres=vThres[it %% length(vThres) + (it %% length(vThres) == 0)*length(vThres)], 
                                                            matControl2=matControl2, thres2 = vThres2[(floor((it-1) / length(vThres))+1)], 
                                                            controlVar=controlVar, controlVar2=controlVar2, tau=tau, matBalance=matBalance,
                                                            vMaturity=vMaturity, vSize=vSize, it=it, numThres=numThres, minLength=minLength, 
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
  labs(x=bquote("Threshold "*italic(b)), y=bquote("Objective function value "*italic(f(b))),
       subtitle=substr(funcStr<-paste0(str_squish(deparse(body(objFunc))),collapse=""), start = 2, stop = str_length(funcStr))) +
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
# -- 1c: sum( ( (w2*vM2 - w1*vM1)  ) / sd(w2*vM2 - w1*vM1, na.rm=T), na.rm=T )  | CHOSEN CANDIDATE
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
#   This calls in question the utility of calculating w2 within this function at all, when it can be pre specified instead
### CONCLUSION
# 1c seems best, but specify w2 beforehand as the average between highest and lowest contamination degrees at the portfolio-level




# ------ 3. Analytics

# - Confirm extracted case study is loaded into memory
if (!exists('datResults')) unpack.ffdf(paste0(genObjPath,"Results_Candidate2_", caseStudy_Name), tempPath)

# - Set universal graphing parameters
chosenFont <- "Cambria"


# --- 1. Main optimisation results: M1, M2, objective function across threshold b | Primary control variable only (B_t)

# - Prepare results dataset
datPlot <- pivot_longer(data=datResults[Threshold2==0,list(Threshold, M1_mean, M2_mean, M2_mean_scaled = M2_mean * w2, Objective)], cols=M1_mean:Objective, 
                        names_to="Measure", values_to="Value") %>% as.data.table()
# - Graph results
ggplot(datPlot[Measure %in% c("M1_mean", "M2_mean", "Objective")], aes(x=Threshold, y=Value, group=Measure)) + theme_minimal() + 
  labs(y="Value", x=bquote("Threshold "*italic(b))) + 
  geom_point(aes(colour=Measure, shape=Measure), size=1.5) + 
  geom_line(aes(colour=Measure, linetype=Measure), linewidth=1)
### NOTE: This graph is ruined by M2's massive domain relative to those of M1 and the objective function.

# - Calculate slope ("1st derivative") to help isolate region of optima | FAILED
#datAnnotate <- subset(datResults, Threshold2==0)[,list(Threshold, Value=Objective, Measure="Objective")]
#obj_max <- max(datAnnotate$Value, na.rm=T)
#datAnnotate[, Objective_Diff := c(NA, diff(Value)/diff(Threshold))]
#describe(abs(datAnnotate$Objective_Diff))
#datAnnotate[, Objective_Diff_abs := abs(Objective_Diff)]
#(cut_off <- quantile(datAnnotate[complete.cases(Objective_Diff_abs),Objective_Diff_abs], 0.2))
#datAnnotate[, Status := ifelse( Objective_Diff_abs <= cut_off, "Optimal", "Suboptimal")]
### NOTE: Using the 1st derivative-based approach does not work empirically given that the f is not smooth and fully of jaggies.
# Hence, the labelling results in broken sequences of "optimal" and "sub-optimal" instances

# - Isolate region of optima using Euclidean distance
datAnnotate <- subset(datResults, Threshold2==0)[,list(Threshold, Value=Objective, Measure2="Objective")]
obj_max <- max(datAnnotate$Value, na.rm=T)
obj_argmax <- datAnnotate$Threshold[which(datAnnotate$Value == obj_max)]
datAnnotate[, Objective_Dist := sqrt((obj_max-Value)^2 + (obj_argmax-Threshold)^2)]
# plot(y=datAnnotate$Objective_Dist, x= datAnnotate$Threshold, type="b")

# - Label optimality region accordingly using distributional analysis on distance measure
#describe(datAnnotate$Objective_Dist); hist(datAnnotate$Objective_Dist)
(cut_off <- quantile(datAnnotate[complete.cases(Objective_Dist),Objective_Dist], 0.30) )
datAnnotate[, Status := ifelse( Objective_Dist <= cut_off, "Optimal", "Suboptimal")]
datAnnotate[, Value2 := ifelse( Objective_Dist <= cut_off, Value, NA)]
datAnnotate[, Value3 := ifelse( Objective_Dist == 0, Value, NA)]

# - Establish rectangular coordinates for optimal region
datOptim <- data.table(xmin = min(datAnnotate[!is.na(Value2), Threshold], na.rm=T)*0.6, 
                       xmax = max(datAnnotate[!is.na(Value2), Threshold], na.rm=T)*1.15,
                       ymin = min(datAnnotate[!is.na(Value2), Value], na.rm=T)*0.96,
                       ymax = max(datAnnotate[!is.na(Value2), Value], na.rm=T)*1.05)

# - Enrich plotting data with optimality regions for aesthetic purposes
datPlot[, Measure2 := 
          ifelse(Measure == "Objective" & Value %in% datAnnotate[!is.na(Value2), Value], "Objective-optimal", Measure)]

# - Finally, exclude M2 measure from plotting data
datPlot_sub <- subset(datPlot, !(Measure %in% c("M2_mean_scaled", "M2_mean", "M1_mean")))

# - Set graphing parameters
vCol <- brewer.pal(8, "Dark2")[c(3,4)]
vCol2 <- brewer.pal(8, "Pastel2")[c(3,4)]
vLabel <- c("M1_mean" = bquote(italic(M[1])), "Objective" = bquote("Objective function "*italic(f)),
            "Objective-optimal" = bquote("Optimal region of "*italic(f)))

# - Graph M1 & objective
(g0 <- ggplot(datPlot_sub, aes(x=Threshold, y=Value, group=Measure)) + theme_minimal() + 
  labs(y=bquote("Objective function "*italic(f)), x=bquote("Threshold (ZAR) "*italic(b))) + 
  theme(text=element_text(family=chosenFont), legend.position="bottom") + 
  # Main graph
  geom_line(aes(colour=Measure2), linewidth=0.5) + 
  geom_point(aes(colour=Measure2, shape=Measure2), size=2) + 
  # Annotations
  geom_rect(xmin=datOptim$xmin, xmax=datOptim$xmax, ymin=datOptim$ymin, ymax=datOptim$ymax, fill=vCol2[2], alpha=0.01) + 
  geom_point(data=datAnnotate, aes(x=Threshold, y=Value3, group=Measure2), shape=1, size=5, show.legend = F, colour=vCol[2]) + 
    annotate(geom="text", x=4300, y=obj_max, family=chosenFont, size=4, parse=T,
             label=paste0("'Max attained: '*{italic(f(b))=='", comma(obj_max,accuracy=0.01), "'}*' at '*italic(b)*' = ", obj_argmax,"'")) + 
  annotate(geom="text", x=4000, y=obj_max*0.9, family=chosenFont, size=4, parse=T,
             label=paste0("'Optimal region of '*italic(f)*': '*italic(b)%in%~'[",
                          min(datAnnotate[!is.na(Value2), Threshold], na.rm=T), ", ",
                          max(datAnnotate[!is.na(Value2), Threshold], na.rm=T), "]'")) + 
  # Facets & scale options
  scale_colour_manual(name="Measure", values=vCol, labels=vLabel) + 
  scale_shape_discrete(name="Measure", labels=vLabel) + 
  scale_x_continuous(labels=comma) + scale_y_continuous(labels=comma)
)

# - save plot
dpi <- 175
ggsave(g0, file=paste0(genFigPath,"CaseStudy-",caseStudy_Name, "_TruEnd-optima.png"),width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")


# -- Version with bigger DPI for miniplots in LaTeX
(g0 <- ggplot(datPlot_sub, aes(x=Threshold, y=Value, group=Measure)) + theme_minimal() + 
    labs(y=bquote("Objective function "*italic(f)), x=bquote("Threshold (ZAR) "*italic(b))) + 
    theme(text=element_text(family=chosenFont), legend.position="bottom") + 
    # Main graph
    geom_line(aes(colour=Measure2), linewidth=0.5) + 
    geom_point(aes(colour=Measure2, shape=Measure2), size=2) + 
    # Annotations
    geom_rect(xmin=datOptim$xmin, xmax=datOptim$xmax, ymin=datOptim$ymin, ymax=datOptim$ymax, fill=vCol2[2], alpha=0.01) + 
    geom_point(data=datAnnotate, aes(x=Threshold, y=Value3, group=Measure2), shape=1, size=5, show.legend = F, colour=vCol[2]) + 
    annotate(geom="text", x=5630, y=obj_max, family=chosenFont, size=4, parse=T,
             label=paste0("'Max attained: '*{italic(f(b))=='", comma(obj_max,accuracy=0.01), "'}*' at '*italic(b)*' = ", obj_argmax,"'")) + 
    annotate(geom="text", x=5200, y=obj_max*0.85, family=chosenFont, size=4, parse=T,
             label=paste0("'Optimal region of '*italic(f)*': '*italic(b)%in%~'[",
                          min(datAnnotate[!is.na(Value2), Threshold], na.rm=T), ", ",
                          max(datAnnotate[!is.na(Value2), Threshold], na.rm=T), "]'")) + 
    # Facets & scale options
    scale_colour_manual(name="Measure", values=vCol, labels=vLabel) + 
    scale_shape_discrete(name="Measure", labels=vLabel) + 
    scale_x_continuous(labels=comma) + scale_y_continuous(labels=comma)
)

# - save plot
dpi <- 260
ggsave(g0, file=paste0(genFigPath,"CaseStudy-",caseStudy_Name, "_TruEnd-optima_small.png"),width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")



# --- 2. Portfolio-level mean contamination degree in M1 (due to M2) across threshold b | Primary control variable only (B_t)

# - Find weight for scaling M2 during optimisation first; intended as an annotation
(datExp <- datResults[Threshold2==0, list(Threshold, Contam= M1_mean / (M1_mean+M2_mean))])
mean(datExp[Contam>0, Contam]) # influenced by choice of thresholds, not reliably summary
(contamDeg_mid <- datExp[Contam>0, (Contam[.N] - Contam[1])/2]) # average value between highest & lowest contamination degree | Seems plausible
# datExp[, (Contam[.N] - Contam[1])/2] ## Very similar results when we remove the "Contam>0" condition; itself kept merely to lessen outliers, espeically in smaller samples# datExp[, (Contam[.N] - Contam[1])/2] ## Very similar results when we remove the "Contam>0" condition; itself kept merely to lessen outliers, espeically in smaller samples

# - graphing parameters
vCol <- brewer.pal(10, "Paired")[c(2,1)]

# - create graph
(g1 <- ggplot(datResults[Threshold2==0,], aes(x=Threshold, y=(M1_mean / (M1_mean+M2_mean)))) + theme_minimal() + 
  labs(y=bquote("Portfolio-level contamination degree "*italic(bar(phi1))), x=bquote("Threshold (ZAR) "*italic(b))) + 
  theme(text=element_text(family=chosenFont), legend.position="bottom") + 
  # Main graph
    geom_line(linewidth=0.5, colour=vCol[2]) + geom_point(size=2, colour=vCol[1]) +
  # Annotations
  annotate(geom="text", x=4800, y=contamDeg_mid*1.05, family=chosenFont, size=4, parse=T,
           label=paste0("'Midpoint of '*italic(bar(phi1))*': '*", sprintf("%.3f", contamDeg_mid*100), "*'%'")) + 
  geom_hline(yintercept=contamDeg_mid, linewidth=0.5, linetype="dotted") + 
  # Facets & scale options
  scale_y_continuous(labels=percent) + scale_x_continuous(labels=comma)
)

# - save plot
dpi <- 200
ggsave(g1, file=paste0(genFigPath,"CaseStudy-",caseStudy_Name, "_ContaminationDegree.png"),width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")



# --- 3. Prevalence of TZB-regimes across threshold b | Primary control variable only (B_t)

b_value <- 300 # assigned using discretion for following graphs

# - Prepare summaries for annotating the eventual graph
datAnnotate <- subset(datResults, Threshold2==0 & Threshold==b_value)
if (NROW(datAnnotate) == 0) stop("Threshold not found!")
b_prevalence <- datAnnotate[, TZB_prevalence]

# - graphing parameters
vCol <- brewer.pal(10, "Paired")[c(10,9)]

# - create graph
(g2 <- ggplot(datResults[Threshold2==0,], aes(x=Threshold, y=TZB_prevalence)) + theme_minimal() + 
  labs(y="Portfolio-wide TZB-prevalence (%) amongst all accounts", x=bquote("Threshold (ZAR) "*italic(b))) + 
  theme(text=element_text(family=chosenFont),legend.position="bottom") + 
  # Main graph
  geom_line(linewidth=0.5, colour=vCol[2]) + geom_point(size=2, colour=vCol[1]) +
  # Annotations
  annotate(geom="text", x=3500, y=b_prevalence*1.01, family=chosenFont, size=4, parse=T,
           label=paste0("'Prevalence at chosen '*italic(b^{'*'})==",b_value,"*':  ", sprintf("%.1f", b_prevalence*100), "%'")) + 
  geom_segment(x=0, xend=b_value, y=b_prevalence, yend=b_prevalence, linewidth=0.5, linetype="dotted") + 
  geom_segment(x=b_value, xend=b_value, y=0, yend=b_prevalence, linewidth=0.5, linetype="dotted") + 
  geom_point(x=b_value, y=b_prevalence, size=4, shape=1, show.legend=F) + 
  # Facets & scale options
  scale_y_continuous(labels=percent) + scale_x_continuous(labels=comma)
)

# - save plot
dpi <- 200
ggsave(g2, file=paste0(genFigPath,"CaseStudy-",caseStudy_Name, "_TZB-prevalence.png"),width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")



# --- 4. Account ages & length of isolated TZB-regime across threshold b | Primary control variable only (B_t)

# - Prepare results dataset
datPlot <- pivot_longer(data=datResults[Threshold2==0,list(Threshold, FalseEnd_mean, TruEnd_mean, TZB_Length_mean)], 
                        cols=FalseEnd_mean:TZB_Length_mean, names_to="Measure", values_to="Value") %>% as.data.table()
datPlot[, Facet := ifelse(Measure=="TZB_Length_mean", "2.  TZB-regimes only", "1.  Account lifespans")]

# - Enrich graphing dataset with standard deviation estimates
datEnrich <- pivot_longer(data=datResults[Threshold2==0,list(Threshold, TruEnd_sd, TZB_Length_sd)], 
                                   cols=TruEnd_sd:TZB_Length_sd, names_to="Measure", values_to="SD") %>% as.data.table()
datEnrich[, Measure := case_when(Measure == "TZB_Length_sd" ~ "TZB_Length_mean", Measure == "TruEnd_sd" ~ "TruEnd_mean")] # Temporary key just to facilitate fusion back to datPlot
datPlot <- merge(datPlot, datEnrich, by=c("Threshold", "Measure"), all.x=T); rm(datEnrich)
datPlot[, N := unique(datResults$Accs_Count)[1]] # Enrich with sample size info

# - Create 95% confidence interval for point estimate (mean) : Population-training set comparison
datPlot[, ErrMargin := (qnorm(1-(1-0.95)/2)*SD/sqrt(N))]
datPlot[, Value_lower := Value - ErrMargin]
datPlot[, Value_upper := Value + ErrMargin]
datPlot[, Measure2 := ifelse(is.na(SD), NA, Measure)]

# - Prepare summaries for annotating the eventual graph
datAnnotate <- subset(datPlot, Threshold==b_value)
if (NROW(datAnnotate) == 0) stop("Threshold not found!")
datAnnotate[Measure=="FalseEnd_mean", Value:=NA] # void the non-varying quantity for graphical purposes
datAnnotate[, Label := paste0("'Value at chosen '*italic(b^{'*'})==", b_value, "*':  ", sprintf("%.2f", Value), " ± ", sprintf("%.2f", ErrMargin), " months'")]
datAnnotate[, x_Label := 4500]
datAnnotate[, y_Label := case_when(Measure=="TruEnd_mean"~Value*1.005,
                                   Measure=="TZB_Length_mean"~Value*0.95,
                                   NA~NA)]

# - graphing parameters
vCol <- brewer.pal(8, "Dark2")[c(2,1,3)]; vCol2 <- brewer.pal(8, "Set2")[c(1,3,2)]
vLabel <- c("FalseEnd_mean"="Age (No TruEnd)", "TruEnd_mean"="Age (TruEnd)", "TZB_Length_mean" = "Length of TZB-regime")
vLabel2 <- c("TruEnd_mean"="Age (TruEnd)", "TZB_Length_mean" = "Length of TZB-regime")

# - Graph results
(g3 <- ggplot(datPlot, aes(x=Threshold, y=Value, group=Measure)) + theme_minimal() + 
    labs(y="Months", x=bquote("Threshold (ZAR) "*italic(b))) + 
    theme(legend.position="bottom", legend.box="vertical",
          text=element_text(family=chosenFont),
          strip.background = element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=8, colour="gray50"), strip.text.y.right=element_text(angle=90)) + 
    # Main graph
    geom_ribbon(aes(fill=Measure2, ymin=Value_lower, ymax=Value_upper), alpha=0.4) + 
    geom_line(aes(colour=Measure, linetype=Measure), linewidth=0.5) +    
    geom_point(aes(colour=Measure, shape=Measure), size=2) + 
    # Annotations
    geom_point(data=datAnnotate, aes(x=Threshold, y=Value, colour=Measure), size=4, shape=1, show.legend=F) + 
    geom_hline(data=datAnnotate, aes(yintercept=Value, colour=Measure), linewidth=0.3, linetype="dotted", show.legend=F) + 
    geom_text(data=datAnnotate, aes(x=x_Label, y=y_Label, group=Measure, label=Label), size=4, family=chosenFont, parse=T) + 
    # Facets & scale options
    facet_grid(Facet~., scales="free") + 
    scale_colour_manual(name="Mean of", values=vCol, labels=vLabel) + 
    scale_linetype_discrete(name="Mean of", labels=vLabel) + 
    scale_shape_discrete(name="Mean of", labels=vLabel) + 
    scale_fill_manual(name="95% CI for means", values=vCol2, labels=vLabel2, na.translate=F) +
    scale_y_continuous(labels=comma) + scale_x_continuous(labels=comma)
)

# - save plot
dpi <- 220
ggsave(g3, file=paste0(genFigPath,"CaseStudy-",caseStudy_Name, "_AccountAges.png"),width=1200/dpi, height=1500/dpi,dpi=dpi, bg="white")




# --- 5a. Balance summaries (M1, M2, Balance at TruEnd) across threshold b | Primary control variable only (B_t)

# - Prepare results dataset
datPlot <- pivot_longer(data=datResults[Threshold2==0,list(Threshold, M1_mean, M2_mean, TruBal_mean)], cols=M1_mean:TruBal_mean, 
                        names_to="Measure", values_to="Value") %>% as.data.table()
datPlot[, Facet := ifelse(Measure == "M1_mean", "TZB", "Non-TZB")]
datPlot[, Measure2 := case_when(Measure == "M1_mean"~"'1.  TZB-periods starting from '*italic(t[z])",
                                Measure == "M2_mean"~"'2.  Non-TZB periods ending at '*italic(t[z])-1",
                                Measure == "TruBal_mean"~"'3.  True closing balances at '*italic(t[z])-1",)]

# - Prepare summaries for annotating the eventual graph
datAnnotate <- subset(datPlot, Threshold==b_value)
if (NROW(datAnnotate) == 0) stop("Threshold not found!")
datAnnotate[, Label := paste0("'Value at chosen '*italic(b^{'*'})==", b_value, "*':  ZAR ", comma(Value,accuracy=0.01), "'")]
datAnnotate[, x_Label := case_when(Measure=="M1_mean"~5000,
                                   Measure=="M2_mean"~5000,
                                   Measure=="TruBal_mean"~5000)]
datAnnotate[, y_Label := case_when(Measure=="M1_mean"~Value*5,
                                   Measure=="M2_mean"~Value*1.002,
                                   Measure=="TruBal_mean"~Value*1.0002)]

# - graphing parameters
vCol <- brewer.pal(9, "Set1")[c(1,3,7)]
vLabel <- c("M1_mean"=bquote("Mean of "*italic(M[1])), "M2_mean"=bquote("Mean of "*italic(M[2])),
            "TruBal_mean"=bquote("Mean balance"))

# - Graph results
(g4 <- ggplot(datPlot, aes(x=Threshold, y=Value, group=Measure)) + theme_minimal() + 
  labs(y="Value (ZAR)", x=bquote("Threshold (ZAR) "*italic(b))) + 
  theme(legend.position="bottom", text=element_text(family=chosenFont),
        strip.background = element_rect(fill="snow2", colour="snow2"),
        strip.text=element_text(size=9.8, colour="gray50"), strip.text.y.right=element_text(angle=90),
        legend.text = element_text(size=9.5)) + 
  # Main graph
  geom_line(aes(colour=Measure, linetype=Facet), linewidth=0.5) + 
  geom_point(aes(colour=Measure, shape=Facet), size=2) + 
  # Annotations
  geom_point(data=datAnnotate, aes(x=Threshold, y=Value, colour=Measure), size=4, shape=1, show.legend=F) + 
  geom_hline(data=datAnnotate, aes(yintercept=Value, colour=Measure), linewidth=0.3, linetype="dotted", show.legend=F) + 
  geom_text(data=datAnnotate, aes(x=x_Label, y=y_Label, group=Measure, label=Label), size=4, family=chosenFont, parse=T) +
  # Facets & scale options
  facet_grid(Measure2 ~ ., scales="free", labeller = label_parsed) + 
  scale_colour_manual(name="Measure", values=vCol, labels=vLabel) + 
  scale_shape_discrete(name="") + scale_linetype_discrete(name="") + 
  scale_y_continuous(labels=comma) + scale_x_continuous(labels=comma)
)

# - save plot
dpi <- 195
ggsave(g4, file=paste0(genFigPath,"CaseStudy-",caseStudy_Name, "_Balances.png"),width=1200/dpi, height=1700/dpi,dpi=dpi, bg="white")




# --- 5b. Balance summaries (M1, M2) across threshold b | Primary control variable only (B_t)

# - Prepare results dataset
datPlot <- pivot_longer(data=datResults[Threshold2==0,list(Threshold, M1_mean, M2_mean)], cols=M1_mean:M2_mean, 
                        names_to="Measure", values_to="Value") %>% as.data.table()
datPlot[, Measure2 := case_when(Measure == "M1_mean"~"'1.  TZB-periods starting from '*italic(t[z])",
                                Measure == "M2_mean"~"'2.  Non-TZB periods ending at '*italic(t[z])-1")]

# - Enrich graphing dataset with standard deviation estimates
datEnrich <- pivot_longer(data=datResults[Threshold2==0,list(Threshold, M1_sd, M2_sd)], 
                          cols=M1_sd:M2_sd, names_to="Measure", values_to="SD") %>% as.data.table()
datEnrich[, Measure := case_when(Measure == "M1_sd" ~ "M1_mean", Measure == "M2_sd" ~ "M2_mean")] # Temporary key just to facilitate fusion back to datPlot
datPlot <- merge(datPlot, datEnrich, by=c("Threshold", "Measure"), all.x=T); rm(datEnrich)
datPlot[, N := unique(datResults$Accs_Count)[1]] # Enrich with sample size info

# - Create 95% confidence interval for point estimate (mean) : Population-training set comparison
datPlot[, ErrMargin := (qnorm(1-(1-0.95)/2)*SD/sqrt(N))]
datPlot[, Value_lower := Value - ErrMargin]
datPlot[, Value_upper := Value + ErrMargin]
datPlot[, Measure3 := ifelse(is.na(SD), NA, Measure)]

# - Prepare summaries for annotating the eventual graph
datAnnotate <- subset(datPlot, Threshold==b_value)
if (NROW(datAnnotate) == 0) stop("Threshold not found!")
datAnnotate[, Label := paste0("'Value at chosen '*italic(b^{'*'})==", b_value, "*':  ZAR ", comma(Value,accuracy=0.01), " ± ", comma(ErrMargin,accuracy=0.01), "'")]
datAnnotate[, x_Label := case_when(Measure=="M1_mean"~6000,
                                   Measure=="M2_mean"~5500)]
datAnnotate[, y_Label := case_when(Measure=="M1_mean"~Value*4.5,
                                   Measure=="M2_mean"~Value*0.995)]

# - graphing parameters
vCol <- brewer.pal(9, "Set1")[c(1,3)]
vLabel <- c("M1_mean"=bquote(italic(bar(M)[1])), "M2_mean"=bquote(italic(bar(M)[2])))
vLabel2 <- c("M1_mean"=bquote(italic(bar(M)[1])), "M2_mean"=bquote(italic(bar(M)[2])))

# - Graph results
(g5 <- ggplot(datPlot, aes(x=Threshold, y=Value, group=Measure)) + theme_minimal() + 
    labs(y="Value (ZAR)", x=bquote("Threshold (ZAR) "*italic(b))) + 
    theme(legend.position="bottom", text=element_text(family=chosenFont),
          strip.background = element_rect(fill="snow2", colour="snow2"),
          strip.text=element_text(size=9.8, colour="gray50"), strip.text.y.right=element_text(angle=90),
          legend.text = element_text(size=9.5)) + 
    # Main graph
    geom_ribbon(aes(fill=Measure3, ymin=Value_lower, ymax=Value_upper), alpha=0.4) + 
    geom_line(aes(colour=Measure, linetype=Measure), linewidth=0.5) + 
    geom_point(aes(colour=Measure, shape=Measure), size=2) + 
    # Annotations
    geom_point(data=datAnnotate, aes(x=Threshold, y=Value, colour=Measure), size=4, shape=1, show.legend=F) + 
    geom_hline(data=datAnnotate, aes(yintercept=Value, colour=Measure), linewidth=0.3, linetype="dotted", show.legend=F) + 
    geom_text(data=datAnnotate, aes(x=x_Label, y=y_Label, group=Measure, label=Label), size=4, family=chosenFont, parse=T) +
    # Facets & scale options
    facet_grid(Measure2 ~ ., scales="free", labeller = label_parsed) + 
    scale_colour_manual(name="Mean of", values=vCol, labels=vLabel) + 
    scale_shape_discrete(name="Mean of", labels=vLabel) + 
    scale_linetype_discrete(name="Mean of", labels=vLabel) + 
    scale_fill_manual(name="95% CI for means", values=vCol, labels=vLabel2, na.translate=F) +
    scale_y_continuous(labels=comma) + scale_x_continuous(labels=comma)
)

# - save plot
dpi <- 220
ggsave(g5, file=paste0(genFigPath,"CaseStudy-",caseStudy_Name, "_Balances_M1M2.png"),width=1200/dpi, height=1500/dpi,dpi=dpi, bg="white")




# --- Cleanup
rm(matBalance, matControl, matControl2, vMaturity, vSize, datResults, datPlot, datGiven, datExp, datAnnotate, datGraph, datOptim, datPlot_sub,
   g0, g1, g2, g3, g4, g5, vLabel, vLabel1, vLabel2); gc()
