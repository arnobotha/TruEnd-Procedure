# ================================== TruEnd-procedure ===================================
# Applying the TruEnd-procedure on repeatedly subsampled datasets from the full set in 
# a Monte Carlo-type setup.
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: TruEnd-procedure
# SCRIPT AUTHOR(S): Dr Arno Botha
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

caseStudy_Name <- "MonteCarlo"

# - Field name assignments
# Isolating possible TZB-regimes of minimum length [minLength] for a given threshold [thres]
#  for a given control variable [contVar] across given credit history, 
#  indexed by account [accVar] and time [timeVar]. Inputs include:
# [controlVar]: field name within given dataset, e.g., Balance B_t; Principal_Ratio R_t.
# [controlVar2]: field name within given dataset for a second (optional) control variable, e.g., Balance B_t; Principal_Ratio R_t.
# [balanceVar]: field name for balance/exposure field (can be same as [controlVar]).
# [accVar]: field name for part of the composite key (loan ID)
# [timeVar]: field name for part of the composite key (counter)
controlVar <- "Balance"; balanceVar <- "Balance"
controlVar2 <- "Principal_Ratio"
controlVar2VoidVal <- 0 # threshold value at which secondary control variable is voided/discarded when finding TZB-regimes
accVar <- "LoanID"
timeVar <- "Counter"
ageVar <- "Age_Adj"

# - Confirm raw data is loaded into memory
if (!exists('datCredit_real')) unpack.ffdf(paste0(genPath,"creditdata_final1c"), tempPath)

# - Get list of unique loans from which we will subsample down to a specific size
vAccs <- unique(datCredit_real[ExclusionID==0, LoanID])

# - Set Monte Carlo parameters
subSampSize <- 10000; subSampSize_Prop <- subSampSize / length(vAccs)
MonteCarloRuns <- 100


# --- 1. Parameters for TruEnd-procedure

minLength <- 1 # minimum length (in months) of an isolated TZB-regime, by definition
tau <- 6 # length of non-TZB period that should precede an isolated TZB-regime, for M2-purposes

# - Define threshold vector for primary control variable: [Balance]
vThres <- c(0,10,25,50,75,100,150,200,250,300,400,500,750,1000,1250,1500,1750,2000,2500,3000,4000,5000,7500,10000) # expanded search space

# - Define threshold vector for secondary/optional control variable: [Principal_Ratio]
# NOTE: If secondary control variable is to be deactivated completely, then assign a single 0-value to vThres2
# This should ensure the proper working of the internal logic within the TruEnd-functions
vThres2 <-  c(0)

# - Calculate overall number of thresholds across both control variables
numThres <- length(vThres) * ( (length(vThres2) > 0) %?% length(vThres2) %:% 1)

# - Define objective function and associated argument list
w1 <- 1 # weight for M1 with its small domain
w2 <- 0.0006362424 # weight for M2 with its large domain, should logically be < w1 | \tau=6
# Best t_z given by maximising:
objFunc <- function(vM1, vM2, w1=w1, w2=w2) {
  sum( (w2*vM2 - w1*vM1), na.rm=T ) / sd(w2*vM2 - w1*vM1, na.rm=T)
}
args <- c("vM2", "vM1", "w1", "w2")




# ------ 2. Monte Carlo setup

# - Setup & logging
ptm <- proc.time() #IGNORE: for computation time calculation
cl.port <- makeCluster(8); registerDoParallel(cl.port) # multi-threading setup

for (i in 1:MonteCarloRuns) {
  # i <- 1
  
  cat("New Job: Applying TruEnd-procedure within a Monte Carlo Setup..", file=paste0("assesslog_MonteCarlo.txt"), append=F)
  cat(paste0("\nIteration ", i, " of ", MonteCarloRuns, " .."))
  
  # - Subsample accounts down to fixed size
  set.seed(i*10)
  vAccs_samp <- data.table(LoanID=vAccs) %>% slice_sample(prop=subSampSize_Prop)
  
  # - Subsample main credit dataset accordingly
  datGiven <- subset(datCredit_real, ExclusionID == 0 & LoanID %in% vAccs_samp$LoanID,
                     select=c("LoanID", "Counter", "Age_Adj", "Date", "Principal", "Balance", "WOff_Ind"))
  
  # --- 1. Preliminaries
  # NOTE: This part simply prepares some data and would become redundant when generalising 
  # this script into a callable function, given this data.
  
  # - Abstract the control variable [Balance] matrix from data
  # NOTE: rows are periods (up to maximum observed period), columns are loan accounts
  matControl <- as.matrix(pivot_wider(data=datGiven[order(get(accVar)),list(LoanID = get(accVar), Time = get(timeVar), ControlVar = get(controlVar))], 
                                      id_cols=Time, names_from=LoanID, values_from=ControlVar))[,-1]
  
  # - Abstract the balance matrix from data for calculation of M1 and M2 measures later
  # NOTE: rows are periods (up to maximum observed period), columns are loan accounts
  matBalance <- as.matrix(pivot_wider(data=datGiven[order(get(accVar)),list(LoanID = get(accVar), Time = get(timeVar), Balance = get(balanceVar))], 
                                      id_cols=Time, names_from=LoanID, values_from=Balance))[,-1]
  
  # - Abstract the vector of observed maturities from data
  vMaturity <- datGiven[order(get(accVar)),list(Freq = max(get(ageVar),na.rm=T)), by=list(get(accVar))]$Freq
  
  # - Abstract a vector of vector sizes (number of elements within balance matrices per account)
  vSize <- datGiven[order(get(accVar)),list(Freq = max(get(timeVar),na.rm=T)), by=list(get(accVar))]$Freq
  
  
  
  # --- 2. TruEnd-procedure
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
  
  # - Append results
  if (i > 1) {
    datResults_outer <- rbind(datResults_outer, data.table("Iteration"=i, datResults)) 
  } else datResults_outer <- data.table("Iteration"=i, datResults)
  # - Logging
  tme <- proc.time() - ptm #IGNORE: for computation time calculation
  cat(paste0("\n END: TruEnd-procedure applied! Runtime: ", sprintf("%.1f", tme[3] / 60), " minutes"),
      file=paste0("assesslog_MonteCarlo.txt"), append=T)
}

stopCluster(cl.port); 

# - Save to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genObjPath,"MonteCarlo_TruEndResults_MinLength-",minLength,"_Tau-",tau), datResults_outer)





# ------ 3. Analytics

# - Confirm extracted case study is loaded into memory
if (!exists('datResults')) unpack.ffdf(paste0(genObjPath,"MonteCarlo_TruEndResults_MinLength-",minLength,"_Tau-",tau), tempPath)

# - Set universal graphing parameters
chosenFont <- "Cambria"


# --- 1. Main optimisation results: objective function across threshold b for primary control variable (balance)

# - Aggregate across iterations
datPlot <- datResults_outer[Threshold2==0, list(Objective = mean(Objective, na.rm=T),
                                                Obj_sd = sd(Objective, na.rm=T),
                                                N = .N) , by=list(Threshold)]

# - Create 95% confidence interval for point estimate (mean) : Population-training set comparison
datPlot[, ErrMargin := (qnorm(1-(1-0.95)/2)*Obj_sd/sqrt(N))]
datPlot[, Value_lower := Objective - ErrMargin]
datPlot[, Value_upper := Objective + ErrMargin]

# - Isolate region of optima using Euclidean distance
datAnnotate <- datPlot[,list(Threshold, Value=Objective, Measure2="Objective")]
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
datPlot[, Measure := "Objective"]
datPlot[, Measure2 := ifelse(Objective %in% datAnnotate[!is.na(Value2), Value], "Objective-optimal", "Objective")]

# - Set graphing parameters
vCol <- brewer.pal(8, "Dark2")[c(3,4)]
vCol2 <- brewer.pal(8, "Pastel2")[c(3,4)]
vLabel <- c("Objective" = bquote("Objective function "*italic(f)),
            "Objective-optimal" = bquote("Optimal region of "*italic(f)))

# - Main Graph
(g0 <- ggplot(datPlot, aes(x=Threshold, y=Objective, group=Measure)) + theme_minimal() + 
    labs(y=bquote("Mean value "*bar(italic(f))~(italic(b)) ), x=bquote("Threshold (ZAR) "*italic(b))) + 
    theme(text=element_text(family=chosenFont), legend.position="bottom") + 
    # Main graph
    geom_ribbon(aes(ymin=Value_lower, ymax=Value_upper), alpha=0.5, fill=vCol2[1]) + 
    #geom_line(aes(colour=Measure2), linewidth=0.25) + 
    geom_point(aes(colour=Measure2, shape=Measure2), size=2) + 
    # Annotations
    #geom_rect(xmin=datOptim$xmin, xmax=datOptim$xmax, ymin=datOptim$ymin, ymax=datOptim$ymax, fill=vCol2[2], alpha=0.01) + 
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
ggsave(g0, file=paste0(genFigPath,"CaseStudy-",caseStudy_Name,"_MinLength-",minLength,"_Tau-",tau, "_TruEnd-optima_small.png"),width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")



# --- Cleanup
rm(matBalance, matControl, matControl2, vMaturity, vSize, datResults, datPlot, datGiven, datExp, datAnnotate, datGraph, datOptim, datPlot_sub,
   g0, g1, g2, g3, g4, g5, vLabel, vLabel1, vLabel2,
   vAccs_samp, datResults_outer); gc()
