# ================================== TruEnd-functions ===================================
# Function definitions used in the TruEnd-procedure itself
# ---------------------------------------------------------------------------------------
# SCRIPT AUTHOR(S): Dr Arno Botha
# VERSION: 1.1 (Oct-2023)
# DESCRIPTION: 
# See Botha, A., Verster, T., Bester, R. (2023). The TruEnd-procedure: Treating trailing
#   zero-valued balances in credit data (forthcoming). Overleaf
#   https://www.overleaf.com/read/dptmgxnvqjjq
# =======================================================================================



# --- Custom function for isolating a TZB-regime for a given vector [vGiven] and threshold [thres], 
# which should be of minimum length [minLenght], while returning a number of specifiable items, 
# based on return type [retType]: 
# 1) "t_z": starting point of TZB-regime, if found
# 2) "t_z-1": true end point, considering the TZB-regime (if found), otherwise the observed account end
# 3) "tzb_len": length of the TZB-regime, if found
# [nonTZB_Val]: optional parameter that is assigned to the TZB-start t_z for non-TZB accounts.
TruEnd_inner <- function(vGiven, thres=0, retType, minLength=2, nonTZB_Val=-1) {
  # - testing conditions
  # vGiven <- matControl[,i]
  # retType <- "epiNum"
  
  # Retain only NAs in given vector
  vGiven <- vGiven[!is.na(vGiven)]
  
  # - Find positions in reversed vector where threshold is triggered
  vFound <- which(rev(vGiven) <= thres)
  
  # - Find complementing position in reversed vector where threshold is first NOT triggered
  lastNotTrig <- which(rev(vGiven) > thres)[1]
  
  # - Logic check: Only execute if any regimes are isolated and if the last of these regimes 
  # exceed a given minimum length, bearing in mind that the "true end" itself would be <= [thres] 
  # and is not technically part of the TZB-regime (hence minus 1)
  if(length(vFound) > 0 & !is.na(lastNotTrig) &
     (lastNotTrig-vFound[1]-1) >= minLength) {logiCheck <- T} else {logiCheck <- F}
  
  # - Find start of TZB-regime, i.e., the t_z point
  # Note: 1st parenthesis (A) is the last position where control variable > [thres]
  # Therefore, the "true end" would be (A) + 1, while the TZB-regime starts at (A) + 2
  if (logiCheck) {t_z <- (length(vGiven) - lastNotTrig+1) + 2} else {t_z <- nonTZB_Val}
  # Possible return point: starting point (as index in given vector) of TZB-regime (if found)
  if (retType == "t_z") {return(t_z)}
  
  # - Find "true end" point (as index in given vector) of TZB-regime (if found)
  if (logiCheck) {tru_end <- t_z - 1} else {tru_end <- length(vGiven)}
  # Possible return point
  if (retType == "t_z-1") {return(tru_end)}
  
  # - Length of the isolated TZB-regime, if found
  if (logiCheck) {tzb_len <- length(vGiven)-t_z+1} else {tzb_len <- 0}
  # Possible return point
  if (retType == "tzb_len") {return(tzb_len)}
  
  # Generic return point
  return(list(t_z=t_z, True_End=tru_end, TZB_Length=tzb_len))
}



# --- Multithreadable function for testing the impact of a specified threshold on a given loan portfolio
# Inputs: [matControl]: data matrix to scan against threshold; [thres]: a particular threshold-value;
# [controlVar]: text describing the control variable being evaluated against the given [thres]
# [controlVar2]: text describing the secondary/optional control variable
# [thres2]: threshold corresponding to the secondary control variable
# [controlVar2VoidVal]: threshold value for voiding the secondary control variable (as if its influence is ignored)
# [matBalance]: balance matrix from which M1 and M2 measures are calculated in evaluating thresholds
# [vecMaturity]: vector of observed maturities (total loan ages) up to the 'false'/untreated account termination time points
# [tau]; length of preceding non-TZB period, [it]: current threshold iteration within broader setup;
# [numThresh]: number of thresholds being tested within broader setup.
# [minLength]: passable argument denoting the minimum length of a TZB-regime, by definition
# [reportFlag]: Boolean-value indicating whether to log activities or not
# [logName]: "filename for log"
# [objFunc]: Given objective function that accepts arguments in [args]
# [args]: any external arguments required by [objFunc]; internally-calculated arguments include vM1, vM2, vT_z, vTruEnd_points, vTZB_len, vTruEnd_bal
TruEnd_outer <- function(matControl, thres=0, controlVar, matBalance, vecMaturity, tau=6, it=1, numThres=1, minLength=1, 
                         matControl2, thres2=0, controlVar2=NA, controlVar2VoidVal=0, reportFlag=F,logName="General",objFunc, args, ...) {
  
  
  # --- 1. Preliminaries 
  
  # - testing conditions
  # it <- 9; reportFlag <- F; thres <- vThres[it %% length(vThres) + (it %% length(vThres) == 0)*length(vThres)]
  # thres2 <- vThres2[(floor((it-1) / length(vThres))+1)]
  # objFunc <- function(vM1, vM2) {sum(vM2-vM1, na.rm=T)}; args <- c("vM2", "vM1")
  
  
  if (reportFlag & (is.na(controlVar2) | thres2==controlVar2VoidVal)) {
    cat(paste0("\n 1)[", it, " of ", numThres, "] Evaluating control variable [", controlVar, 
               "] against threshold ", thres, " .."), file=paste0("assesslog_", logName,".txt"), append=T)
  } else if (reportFlag) {
    cat(paste0("\n 1)[", it, " of ", numThres, "] Evaluating control variable [", controlVar, 
               "] (primary) against threshold ", thres, ", and also secondary control variable [",
               controlVar2, "] against threshold ", thres2, " .."), file=paste0("assesslog_", logName,".txt"), append=T)
  }
  
  # - Abstract the number of accounts from data
  nAcc <- NCOL(matControl)
  
  
  # --- 2. Calculate various vectors of interest
  
  # - Find preliminary t_z points as the start of a TZB-regime, if found
  vT_z <- sapply(1:nAcc, function(i){
    # testing: i <- 1
    if (is.na(controlVar2) | thres2==controlVar2VoidVal) { # only primary control variable
      if (nAcc > 1) {
        TruEnd_inner(vGiven=matControl[,i], thres=thres, retType="t_z", minLength=minLength)  
      } else {
        TruEnd_inner(vGiven=matControl, thres=thres, retType="t_z", minLength=minLength)
      }
      
    } else { # 2 control variables given, use minimum to coalesce signals
      if (nAcc > 1) {
        min(TruEnd_inner(vGiven=matControl[,i], thres=thres, retType="t_z", minLength=minLength),
            TruEnd_inner(vGiven=matControl2[,i], thres=thres2, retType="t_z", minLength=minLength)) 
      } else {
        min(TruEnd_inner(vGiven=matControl, thres=thres, retType="t_z", minLength=minLength),
            TruEnd_inner(vGiven=matControl2, thres=thres2, retType="t_z", minLength=minLength))
      }
    }
  })
  
  # - Find preliminary "true end" points
  vTruEnd_points <- sapply(1:nAcc, function(i){
    # testing conditions
    # i <- 1
    if (is.na(controlVar2) | thres2==controlVar2VoidVal) {  # only primary control variable
      if (nAcc > 1) {
        TruEnd_inner(vGiven=matControl[,i], thres=thres, retType="t_z-1", minLength=minLength)
      } else {
        TruEnd_inner(vGiven=matControl, thres=thres, retType="t_z-1", minLength=minLength)
      }
    } else { # 2 control variables given, use minimum to coalesce signals
      if (nAcc > 1) {
        min(TruEnd_inner(vGiven=matControl[,i], thres=thres, retType="t_z-1", minLength=minLength),
            TruEnd_inner(vGiven=matControl2[,i], thres=thres2, retType="t_z-1", minLength=minLength))
      } else {
        min(TruEnd_inner(vGiven=matControl, thres=thres, retType="t_z-1", minLength=minLength),
            TruEnd_inner(vGiven=matControl2, thres=thres2, retType="t_z-1", minLength=minLength))
      }
    }
  })
  
  # - Calculate length of isolated TZB-regimes, if found
  vTZB_len <- sapply(1:nAcc, function(i){
    if (is.na(controlVar2) | thres2==controlVar2VoidVal) {  # only primary control variable
      if (nAcc > 1) {
        TruEnd_inner(vGiven=matControl[,i], thres=thres, retType="tzb_len", minLength=minLength)
      } else {
        TruEnd_inner(vGiven=matControl, thres=thres, retType="tzb_len", minLength=minLength)
      }
    } else { # 2 control variables given, use maximum to coalesce signals
      if (nAcc > 1) {
        max(TruEnd_inner(vGiven=matControl[,i], thres=thres, retType="tzb_len", minLength=minLength),
            TruEnd_inner(vGiven=matControl2[,i], thres=thres2, retType="tzb_len", minLength=minLength))
      } else {
        max(TruEnd_inner(vGiven=matControl, thres=thres, retType="tzb_len", minLength=minLength),
            TruEnd_inner(vGiven=matControl2, thres=thres2, retType="tzb_len", minLength=minLength))
      }
    }
  })
  
  # - Find balance at the "true end" points, if found
  vTruEnd_bal <- sapply(1:nAcc, function(i, t){
    return(matBalance[t[i],i])}, t=vTruEnd_points)
  
  
  # --- 3. Calculate 2 counter measures: M1 and M2
  
  # - Measure 1: mean TZB-balance, where found
  vM1 <- sapply(1:nAcc, function(i, t_z, m){
    if (t_z[i] > 0) {
      # Isolate entire TZB-regime and calculate mean
      meanBal <- mean(matBalance[t_z[i]:m[i],i],na.rm=T)
    } else {meanBal <- NA}
    return(meanBal)
  }, t_z=vT_z, m=vecMaturity)
  
  # - Measure 2: non-TZB mean balance
  vM2 <- sapply(1:nAcc, function(i, tru, tau){
    # Isolate the [tau] values in the vector that precede the TZB-regime (if found), and calculate mean
    if (tru[i]-tau > 0) { # sufficient history exists
      mean(matBalance[(tru[i]-tau):tru[i],i], na.rm=T)
    } else { # insufficient history exists, therefore start from element 1
      mean(matBalance[1:tru[i],i], na.rm=T)
    }
  }, tru=vTruEnd_points, tau=tau)

  

  # --- 3. Evaluate objective function, given measures M1 and M2
  # Calculate given objective function
  vObjFunc <- do.call(objFunc, args= setNames( lapply(1:length(args), function(i) {get(args[i])} ), args) ) 
  
  
  # --- 4. Concatenate results
  datResults.interim <- data.table(ResultSet = logName, Control = controlVar, Threshold = thres, Control2 = controlVar2, Threshold2 = thres2,
                                   Accs_Count = nAcc,
                                   FalseEnd_mean = mean(vecMaturity, na.rm=T), FalseEnd_sd = sd(vecMaturity, na.rm=T),
                                   TruEnd_mean = mean(vTruEnd_points, na.rm=T), TruEnd_sd = sd(vTruEnd_points, na.rm=T),
                                   TZB_Length_mean = mean(vTZB_len, na.rm=T), TZB_Length_sd = sd(vTZB_len, na.rm=T),
                                   TruBal_mean = mean(vTruEnd_bal, na.rm=T), TruBal_sd = sd(vTruEnd_bal, na.rm=T),
                                   M1_mean = mean(vM1, na.rm=T), M1_sd = sd(vM1, na.rm=T),
                                   M2_mean = mean(vM2, na.rm=T), M2_sd = sd(vM2, na.rm=T),
                                   TZB_prevalence = sum(vT_z>=0)/nAcc,Objective = vObjFunc[1]
  )
  
  return (datResults.interim)
}
