# ================================== DATA PREPARATION ===================================
# Excluding the TruEnd-procedure, apply last few data treatments.
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: TruEnd-procedure
# SCRIPT AUTHOR(S): Dr Arno Botha, Roelinde Bester
# ---------------------------------------------------------------------------------------
# DESCRIPTION:
# In continuing with script 2b, this script applies the following Advanced Data Treatments:
#   6)  Varying [Term]-values; treated by assigning account-level mode of [Term].
#   7)  Inferring net cash flow field (Receipts) using month-end balance-differences between
#       two consecutive points in time.
#   8)  Illogical event amounts at termination; treated by by changing receipts [Receipt_Inf]
#       and correcting write-off/early-settlement amounts such that the balance is
#       naturally zero-valued at account termination, given the corrected [Receipt_Inf].
#   9)  Zero-valued instalments at the end of loan life; treated by assigning last
#       non-zero [Instalment]-value.
#   10) Zero-valued instalment regimes at the start of loan life; treated by assigning first
#       non-zero [Receipt]-value.
#
# Finally, given the obscurities revealed by the previous treatments, re-apply the previous 
# exclusions from script 2a
# ---------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R
#   - 2a.Data_Prepare_Credit_Basic.R
#   - 2b.Date_Prepare_Credit_Advanced.R

# -- Inputs:
#   - datCredit_real | Enhanced version of input dataset (script 2b)
#   - various parameters set in the setup script 0
#
# -- Outputs:
#   - datCredit_real | Enhanced version of input dataset, without Tru-End applied
# ---------------------------------------------------------------------------------------
# NOTE: This script predominantly comes from another project (Kasmeer). As such, all 
# references to associated Data Experiments reside within that project and will be taken 
# as established 'facts' within this project.
# =======================================================================================



# ------ 6. Advanced Data Treatments

# --- 0. Preliminaries
ptm3 <- proc.time() # for runtime calculations (ignore)

# - Confirm raw data is loaded into memory
if (!exists('datCredit_real')) unpack.ffdf(paste0(genPath,"creditdata_final1c"), tempPath)
if (!exists('datExclusions')) unpack.ffdf(paste0(genObjPath,"Exclusions-Preliminary"), tempPath)


# --- 6. Advanced Data Treatment 6: Treating varying [Term]-values | See Data Experiment 5

# - Preliminaries
datCredit_real[ExclusionID==0, Var_Term := var(Term, na.rm=T), by=list(LoanID)]

# [DIAGNOSTIC] How often is the variance non-zero for [Term] where the account 
#   has neither redraw nor further loan across its history?
diag.real5_2a <- datCredit_real[ExclusionID==0 & Counter==1 & Var_Term > 0 & HasRedraw == 0 & HasFurtherLoan == 0, .N] / 
  datCredit_real[ExclusionID==0 & Counter==1, .N] * 100  # account-level
diag.real5_2a_rec <- datCredit_real[ExclusionID==0 & Var_Term > 0 & HasRedraw == 0 & HasFurtherLoan == 0, .N] /
  datCredit_real[ExclusionID==0, .N] * 100  # dataset-level

# - Conditional Treatment | Varying Term
if (diag.real5_2a > 0) {
  
  cat(paste0("DETECTED: Varying [Term]-values unaccounted for by Redraws or Further Loans found in ",
             round(diag.real5_2a,digits=4), "% of accounts. \n\tTreating by assigning the account-level mode ..\n"))
  
  # - Calculate account-level mode of [Term]
  datCredit_real[ExclusionID==0 & Var_Term>0 & HasRedraw==0 & HasFurtherLoan==0,
                 Term_Mode := getmode(Term), by=list(LoanID)]
  
  # [TREATMENT] Assign the account-level mode to the non-equal parts
  datCredit_real[ExclusionID==0 & Term!=Term_Mode & HasRedraw==0 & HasFurtherLoan==0,
                 Term := Term_Mode[1], by=list(LoanID)]
  
  # - Mark affected accounts with current treatment ID
  datCredit_real[ExclusionID==0 & Term!=Term_Mode & HasRedraw==0 & HasFurtherLoan==0, 
                 TreatmentID := paste0(TreatmentID, ";6")]
  
  # - Recalculate affected aggregates
  datCredit_real[ExclusionID==0, Var_Term := var(Term, na.rm=T), by=list(LoanID)]
  
  # [SANITY CHECK] Account-level prevalence of accounts that varying [Term]-values?
  check_advTreat6 <- datCredit_real[ExclusionID==0 & Counter==1 & Var_Term > 0 & HasRedraw == 0 & 
                                      HasFurtherLoan == 0, .N] / 
    datCredit_real[ExclusionID==0 & Counter==1, .N] * 100 == 0 # Should be true
  cat( check_advTreat6 %?% 
         'SAFE: Cases with varying [Term]-values successfully treated with the account-level mode.\n' %:% 
         'WARNING: Failed to treat cases with varying [Term]-values.\n') 
}

# - Cleanup
suppressWarnings( datCredit_real[, `:=`(Var_Term = NULL, Term_Mode = NULL)] )



# --- 7. Advanced Data Treatment 7: Inferring net cash inflow (Receipts) | See Data Experiment 9

cat("Inferring [Receipt_Inf] field from first principles, using balance-differences\n\t",
    "between two consecutive points in time ..\n")

# - Create lagged fields | Intermediary fields
# Balance | Note: Assign first element to Principal if a new loan
datCredit_real[ExclusionID==0, Balance_prev := shift(Balance), by=list(LoanID)]
datCredit_real[ExclusionID==0 & Counter==1, Balance_prev := ifelse(New_Ind, Principal[1], Balance[1]), by=list(LoanID)]
# Interest Rate
datCredit_real[ExclusionID==0, IntRate_prev := shift(InterestRate_Nom), by=list(LoanID)]
datCredit_real[ExclusionID==0 & Counter==1, IntRate_prev := InterestRate_Nom[1], by=list(LoanID)]
# Arrears
datCredit_real[ExclusionID==0, Arrears_prev := shift(Arrears), by=list(LoanID)]
datCredit_real[ExclusionID==0 & Counter==1, Arrears_prev := 0, by=list(LoanID)]

# - Calculate net cash flow (Balance difference) between two successive points in time
datCredit_real[ExclusionID==0, CashFlow := (1+IntRate_prev/12)*Balance_prev - Balance, by=list(LoanID)]

# - Arrears difference between two successive points in time
datCredit_real[ExclusionID==0, Arrears_Diff := (1+IntRate_prev/12)*Arrears_prev - Arrears, by=list(LoanID)]

# - Infer the Receipt R(t) given the following logic with balance B(t), interest i(t):
# 1) For terminal events (settlement, write-off) with event amount E at last time t': 
#     R(t') = (1+i(t-1)/12)*B(t-1) - B(t) + E_s, where R(t) >= 0 and E_s is the settlement amount
#     R(t') = (1+i(t-1)/12)*B(t-1) - B(t) - E_w, where R(t) >= 0 and E_w is the write-off amount
#     This should also redistribute any non-zero balance B(t) (which should rightfully be zero; treated in 
#     Advanced Data Treatment 8) to the receipt amount itself.
# 2) For behavioural events (redraw, further loan) with instalment I(t):
#     R(t) = I(t), where R(t) >= 0. 
#     In order for a redraw/further loan to occur, we must reasonably assume the account to be in good standing,
#     otherwise, why 'disburse' even more funds? As such, we can assume that at least one instalment to be paid.
# 3) For all other events, with calculated cash flow C(t) and Arrears balance A(t):
#     R(t) = C(t) + A(t), where R(t) >= 0
#     Any accrued arrears are offset against the presumably positive cash flow,
#     itself signifying a reducing balance, thereby a payment of sorts. Otherwise, if negative, then no payment.
datCredit_real[ExclusionID==0, Receipt_Inf := round(case_when(
  EarlySettle_Ind==1 ~ pmax((1+IntRate_prev/12)*Balance_prev - Balance + EarlySettle_Amt, 0),
  WOff_Ind==1 ~ pmax((1+IntRate_prev/12)*Balance_prev - Balance - WriteOff_Amt, 0),
  (Redraw_Ind == 1 | FurtherLoan_Ind == 1) ~ pmax(Instalment, 0),
  TRUE ~ pmax(CashFlow + Arrears_Diff, 0)
), digits=2), by=list(LoanID)]
datCredit_real <- datCredit_real %>% relocate(Receipt_Inf, .after=Instalment)

cat("[Receipt_Inf] inferred.\n")

# - Cleanup
suppressWarnings( datCredit_real[, `:=`(Balance_prev = NULL, IntRate_prev = NULL, Arrears_prev = NULL,
                                        CashFlow = NULL, Arrears_Diff = NULL, Receipt_Diff = NULL)] )



# --- 8. Advanced Data Treatment 8: Conforming event amounts to zero-valued balance at termination 
# See Data Experiment 2

# - Isolate affected cases
treat_samp1 <- subset(datCredit_real, ExclusionID==0 & Counter==Max_Counter & 
                        (HasSettle == 1 | HasWOff == 1 | HasRepaid == 1))[,
                                                                          list(LoanID, Counter, Max_Counter, ExclusionID, TreatmentID, 
                                                                               Receipt_Inf, Balance, HasSettle, EarlySettle_Amt,
                                                                               HasWOff, WriteOff_Amt, HasRepaid)]

# - For early settlements & repaids, redistribute any remaining balance to [Receipt_Inf]
treat_samp1[HasSettle==1 | HasRepaid==1, Receipt_Adj := Receipt_Inf + Balance]

# - For write-offs, redistribute any remaining balance to the Write-off Amount
treat_samp1[HasWOff==1, WriteOff_Amt_Adj := WriteOff_Amt + Balance]

# [DIAGNOSTIC] Prevalence of mismatched early settlement amounts observed
treat_samp1[HasSettle==1 & Counter==Max_Counter, Settle_Diff := EarlySettle_Amt - Receipt_Adj]
diag.real2_3a <- treat_samp1[ExclusionID==0 & HasSettle==1 & Counter==Max_Counter & EarlySettle_Amt != Receipt_Adj, .N] / 
  treat_samp1[ExclusionID==0 & HasSettle==1 & Counter==Max_Counter, .N] * 100 
diag.real2_3b <- mean(treat_samp1[HasSettle==1, Settle_Diff], na.rm=T)
diag.real2_3c <- median(treat_samp1[HasSettle==1, Settle_Diff], na.rm=T)

# [DIAGNOSTIC] Prevalence of mismatched write-off amounts observed
treat_samp1[HasWOff==1 & Counter==Max_Counter, WOff_Diff := WriteOff_Amt - WriteOff_Amt_Adj]
diag.real2_3d <- treat_samp1[ExclusionID==0 & HasWOff==1 & Counter==Max_Counter & WriteOff_Amt != WriteOff_Amt_Adj, .N] / 
  treat_samp1[ExclusionID==0 & HasWOff==1 & Counter==Max_Counter, .N] * 100 
diag.real2_3e <- mean(treat_samp1[HasWOff==1, WOff_Diff], na.rm=T)
diag.real2_3f <- median(treat_samp1[HasWOff==1, WOff_Diff], na.rm=T)

# [DIAGNOSTIC] Prevalence of mismatched repaid amounts observed
treat_samp1[HasRepaid==1 & Counter==Max_Counter, Repaid_Diff := Receipt_Inf - Receipt_Adj]
diag.real2_3g <- treat_samp1[ExclusionID==0 & HasRepaid==1 & Counter==Max_Counter & Receipt_Inf != Receipt_Adj, .N] / 
  treat_samp1[ExclusionID==0 & HasRepaid==1 & Counter==Max_Counter, .N] * 100
diag.real2_3h <- mean(treat_samp1[HasRepaid==1, Repaid_Diff], na.rm=T)
diag.real2_3i <- median(treat_samp1[HasRepaid==1, Repaid_Diff], na.rm=T)

# - Conditional Treatment
if (diag.real2_3a > 0 | diag.real2_3d > 0 | diag.real2_3g > 0) {
  
  cat(paste0("DETECTED: Of early settlements / closures, ", round(diag.real2_3a,digits=1), 
             "% of accounts had event amounts that did not agree with calculated 
             event amounts such that [Balance]=0. Mean difference of ", comma(round(diag.real2_3b,digits=2)), 
             " (median: ", comma(round(diag.real2_3c,digits=2)), "). \n\tCorrecting by redistributing non-zero ",
             "balance to [Receipt_Inf] ..\n"))
  
  cat(paste0("DETECTED: Of repaid accounts, ", round(diag.real2_3g,digits=1), 
             "% of accounts did not have [Balance]=0 at loan-end. Mean difference of ", comma(round(diag.real2_3h,digits=2)), 
             " (median: ", comma(round(diag.real2_3i,digits=2)), "). \n\tCorrecting by redistributing non-zero ",
             " balance to [Receipt_Inf] ..\n"))
  
  cat(paste0("DETECTED: Of write-offs, ", round(diag.real2_3d,digits=1), 
             "% of accounts had event amounts E_w that did not agree with calculated 
             event amounts such that 
                  B(t_w)' = B(t_w-1)*(1+i(t-1)/12) - R(t_w) - E_w  :=  0 where
             B(t) is the observed balance at time t, B(t)' is the calculated balance at time t, 
             t_w is the write-off point, i(t) is the nominal interest rate at time t, 
             R(t) is the inferred receipt at time t, E_w is the write-off event amount.
             Note: R(t) = B(t_w-1)*(1+i(t-1)/12)- B(t) - E_w by definition.
             
             Proposed correction: adjusting the write-off amount by E_w' = ( E_w + B(t_w) ), followed
                by setting B(t_w) = 0.
             
             Mean difference between E_w and E_w' of ZAR ", 
             comma(round(diag.real2_3e,digits=2)), " (median: ", comma(round(diag.real2_3f,digits=2)), 
             "). \n\tCorrecting ..\n"))
  
  # - Fuse treated accounts' event amounts with main dataset
  # To facilitate treatment success measurement later
  datCredit_real <- merge(datCredit_real, treat_samp1[,list(LoanID, WriteOff_Amt_Adj, Receipt_Adj)], 
                          by=c("LoanID"), all.x=T)
  
  # [TREATMENT] For early settlements, redistribute the 
  # remaining balance to [Receipt_Inf] and correct Early Settlement Amount
  datCredit_real[ExclusionID==0 & Counter==Max_Counter & HasSettle==1 & 
                   EarlySettle_Amt != Receipt_Adj, Receipt_Inf := Receipt_Inf + Balance]
  datCredit_real[ExclusionID==0 & Counter==Max_Counter & HasSettle==1 & 
                   EarlySettle_Amt != Receipt_Adj, EarlySettle_Amt := Receipt_Inf] 
  
  # [TREATMENT] For repaids, redistribute the remaining balance to [Receipt].
  datCredit_real[ExclusionID==0 & Counter==Max_Counter & HasRepaid==1 & Receipt_Inf != Receipt_Adj, 
                 Receipt_Inf := Receipt_Inf + Balance]
  
  # [TREATMENT] For write-offs, redistribute the remaining
  # balance to the Write-off Amount
  datCredit_real[ExclusionID==0 & Counter==Max_Counter & HasWOff==1 & WriteOff_Amt != WriteOff_Amt_Adj, 
                 WriteOff_Amt := WriteOff_Amt + Balance]
  
  # [TREATMENT] Set the balance explicitly to zero in either terminal case
  datCredit_real[ExclusionID==0 & Counter==Max_Counter & (HasSettle==1 | HasWOff==1 | HasRepaid==1),
                 Balance := 0]
  
  # - Mark the affected accounts with current treatment ID
  LoanIDs <- unique(treat_samp1[ExclusionID==0 & (WriteOff_Amt != WriteOff_Amt_Adj | 
                                                    EarlySettle_Amt != Receipt_Adj | 
                                                    Receipt_Inf != Receipt_Adj), LoanID])
  datCredit_real[LoanID %in% LoanIDs & Counter==Max_Counter, TreatmentID := paste0(TreatmentID, ";8")]
  
  # [SANITY CHECK] Are terminal cases behaving as expected?
  check_advTreat8a <- datCredit_real[ExclusionID==0 & Counter==Max_Counter & HasSettle==1 &
                                       Receipt_Inf != Receipt_Adj, .N] / 
    datCredit_real[ExclusionID==0 & Counter==Max_Counter & HasSettle==1, .N] *100 == 0 # Should be true
  check_advTreat8b <- datCredit_real[ExclusionID==0 & Counter==Max_Counter & HasWOff==1 &
                                       WriteOff_Amt != WriteOff_Amt_Adj, .N] / 
    datCredit_real[ExclusionID==0 & Counter==Max_Counter & HasWOff==1, .N] *100 == 0 # Should be true
  check_advTreat8c <- datCredit_real[ExclusionID==0 & Counter==Max_Counter & HasRepaid==1 &
                                       Receipt_Inf != Receipt_Adj, .N] / 
    datCredit_real[ExclusionID==0 & Counter==Max_Counter & HasRepaid==1, .N] *100 == 0
  cat( (check_advTreat8a & check_advTreat8b & check_advTreat8c)  %?% 
         'SAFE: Terminal event amounts successfully corrected where necessary.\n' %:% 
         'WARNING: Failed to correct all broken terminal event amounts.\n')
  
  # - Cleanup
  suppressWarnings( datCredit_real[, `:=`(Receipt_Adj = NULL, WriteOff_Amt_Adj = NULL)])
}

# - Cleanup
rm(treat_samp1)



# --- 9. Advanced Data Treatment 9: Treating zero-valued instalments at end-of-loan-life | See Data Experiment 8

# - Preliminaries
datCredit_real[ExclusionID == 0, HasZeroIns_Last := ifelse(Instalment[.N] == 0, T, F), by=list(LoanID)]
datCredit_real[ExclusionID == 0, HasZeroIns_SecondLast := ifelse(Instalment[.N-1] == 0, T, F), by=list(LoanID)]

# [DIAGNOSTIC] Prevalence of zero-valued instalments at the last record
diag.real8_3a <- datCredit_real[ExclusionID==0 & Counter==1 & HasZeroIns_Last == T & HasZeroIns_SecondLast == F &
                                  (HasWOff==1 | HasSettle==1 | HasRepaid==1), .N] / 
  datCredit_real[ExclusionID==0 & Counter==1, .N] * 100 
diag.real8_3a_abs <- datCredit_real[ExclusionID==0 & Counter==Max_Counter & HasZeroIns_Last == T & 
                                      HasZeroIns_SecondLast == F & (HasWOff==1 | HasSettle==1 | HasRepaid==1), .N]
diag.real8_3a_rec <- diag.real8_3a_abs / datCredit_real[ExclusionID==0 & Counter==Max_Counter, .N] * 100

# - Conditional treatment
if (diag.real8_3a > 0) {
  
  cat("DETECTED: Zero-valued instalments at end-of-loan-life with terminal events.\n\tPrevalence: ",
      round(diag.real8_3a,digits=2), "% of accounts (", round(diag.real8_3a_rec,digits=2), 
      "% of records as well).\n")
  
  # [TREATMENT] Copy previous non-zero instalment to last record
  datCredit_real[ExclusionID==0 & HasZeroIns_Last == T & HasZeroIns_SecondLast == F & 
                   (HasWOff==1 | HasSettle==1 | HasRepaid==1), 
                 Instalment := ifelse(Counter<Max_Counter, Instalment, Instalment[.N-1]),
                 by=list(LoanID)]
  
  # - Mark affected accounts with current treatment ID
  datCredit_real[ExclusionID==0 & Counter==Max_Counter & HasZeroIns_Last == T & HasZeroIns_SecondLast == F 
                 & (HasWOff==1 | HasSettle==1 | HasRepaid==1), TreatmentID := paste0(TreatmentID, ";9")]
  
  # - Recalculate affected aggregates
  datCredit_real[ExclusionID == 0, HasZeroIns_Last := ifelse(Instalment[.N] == 0, T, F), by=list(LoanID)]
  
  # [SANITY CHECK] Are terminal cases behaving as expected?
  check_advTreat9 <- datCredit_real[ExclusionID==0 & Counter==1 & HasZeroIns_Last == T & 
                                      HasZeroIns_SecondLast == F & (HasWOff==1 | HasSettle==1 | HasRepaid==1), .N] / 
    datCredit_real[ExclusionID==0 & Counter==1, .N] *100 == 0 # Should be true
  cat( check_advTreat9  %?% 
         'SAFE: Terminated accounts with zero-valued instalments at the end were successfully treated.\n' %:% 
         'WARNING: Failed to treat terminated accounts with zero-valued instalments at their end.\n')
}

# - Cleanup
suppressWarnings( datCredit_real[, `:=`(HasZeroIns_Last = NULL, HasZeroIns_SecondLast = NULL )])



# --- 10. Advanced Data Treatment 10: Treating zero-valued instalment-regimes at start | See Data Experiment 8

# - Preliminaries
datCredit_real[ExclusionID==0, Instalment_NonZero_Pos := which(Instalment>0)[1], by=list(LoanID)]
datCredit_real[ExclusionID==0, Arrears_Diff := c(0,diff(Arrears)), by=list(LoanID)]
datCredit_real[ExclusionID==0 & Instalment_NonZero_Pos > 1, ZeroIns_Length2 := Instalment_NonZero_Pos-1,
               by=list(LoanID)]

# [DIAGNOSTIC] Prevalence of accounts with starting zero-valued instalment-regimes
diag.real8_4a <- datCredit_real[ExclusionID==0 & Counter == 1 & Instalment_NonZero_Pos > 1, .N] / 
  datCredit_real[ExclusionID==0 & Counter==1, .N] * 100
diag.real8_4a_abs <- datCredit_real[ExclusionID==0 & Counter < Instalment_NonZero_Pos & Instalment_NonZero_Pos > 1, .N]
diag.real8_4a_rec <- diag.real8_4a_abs / datCredit_real[ExclusionID==0,.N] * 100
diag.real8_4d <- mean(datCredit_real[ExclusionID==0 & Counter == 1 & Instalment_NonZero_Pos > 1, ZeroIns_Length2])
diag.real8_4e <- median(datCredit_real[ExclusionID==0 & Counter == 1 & Instalment_NonZero_Pos > 1, ZeroIns_Length2])

# - Conditional treatment
if (diag.real8_4a > 0) {
  
  cat("DETECTED: Zero-valued instalment-regimes found at the start of credit histories.\n\tPrevalence:",
      round(diag.real8_4a,digits=2), "% of accounts (", round(diag.real8_4a_rec,digits=3), 
      "% of records).\n\tMean regime length of",
      round(diag.real8_4d, digits=1), "months (median: ", round(diag.real8_4e,digits=1), 
      ").\n\tTreating by [Instalment] = [Receipt_Inf] where [Receipt_Inf] > 0,",
      "\n\totherwise assign the first non-zero [Instalment].\n")
  
  # [TREATMENT] Assign [Receipt_Inf] selectively, otherwise assign first non-zero [Instalment]
  # Note: The filter "Counter <= Instalment_NonZero_Pos, Instalment" is necessary to 
  # lookup the first non-zero [Instalment], which "Counter < Instalment_NonZero_Pos, Instalment" will
  # not achieve. Effective contamination is zero since the value at Counter == Instalment_NonZero_Pos
  # is 'overwritten' by itself.
  datCredit_real[ExclusionID==0 & Instalment_NonZero_Pos > 1 & Counter <= Instalment_NonZero_Pos, Instalment := 
                   (Arrears_Diff>=0 & Receipt_Inf>0)*Receipt_Inf + 
                   (Arrears_Diff>=0 & Receipt_Inf==0)*Instalment[Instalment_NonZero_Pos[1]]
                 ,by=list(LoanID)]
  
  # - Mark affected accounts with current treatment ID
  datCredit_real[ExclusionID==0 & Instalment_NonZero_Pos > 1 & Counter < Instalment_NonZero_Pos, 
                 TreatmentID := paste0(TreatmentID, ";10")]
  
  # - Recalculate affected aggregates
  datCredit_real[ExclusionID==0, Instalment_NonZero_Pos := which(Instalment>0)[1], by=list(LoanID)]
  
  # [SANITY CHECK] Did the treatment succeed?
  check_advTreat10 <- datCredit_real[ExclusionID==0 & Counter==1 & Instalment_NonZero_Pos > 1, .N] / 
    datCredit_real[ExclusionID==0 & Counter==1, .N] *100 == 0 # Should be true
  cat( check_advTreat10  %?% 
         'SAFE: Accounts with zero-valued instalment-regimes at the start were successfully treated.\n' %:% 
         'WARNING: Failed to treat accounts with zero-valued instalment-regim
       es at the start.\n')
}

# - Cleanup
suppressWarnings( datCredit_real[,`:=`(Instalment_NonZero_Pos = NULL, Arrears_Diff = NULL, ZeroIns_Length2 = NULL)] )






# ------ 7. Re-applying Exclusions
# Previous treatments may have availed additional records for exclusion using
# the same logic, albeit applied as a single exclusion this time

# --- 1. Old single-record cases | See Data Experiment 4

# [DIAGNOSTIC] Account-level and dataset-wide impacts of exclusion
diag.real4b_2 <-  datCredit_real[ExclusionID == 0 & Counter == 1 & Max_Counter == 1 & 
                                   (Date_Origination < maxDate | is.na(Date_Origination) ), .N] / 
  datCredit_real[ExclusionID == 0 & Counter == 1, .N] * 100 
diag.real4b_2_abs <- datCredit_real[ExclusionID == 0 & Max_Counter == 1 & 
                                      (Date_Origination < maxDate | is.na(Date_Origination)) , .N]
diag.real4b_2_rec <-  diag.real4b_2_abs / datCredit_real[ExclusionID == 0, .N] * 100 

# - Conditional exclusion
if (diag.real4b_2 > 0) {
  
  cat("RE-EXCLUSION: Newly-revealed old single-record cases. Prevalence: ", round(diag.real4b_2,digits=3), 
      "% of accounts (", round(diag.real4b_2_rec,digits=5), "% of records).\n")
  
  # - Mark affected records for exclusion later with an ID-value
  LoanIDs <- unique(subset(datCredit_real, ExclusionID == 0 & Counter == 1 & Max_Counter == 1 & 
                             (Date_Origination < maxDate | is.na(Date_Origination)), select="LoanID"))
  datCredit_real[LoanID %in% LoanIDs$LoanID & ExclusionID == 0, ExclusionID := 7]
  
  # [SANITY CHECK] Treatment success?
  check_excl1_2 <- datCredit_real[ExclusionID == 0 & Counter == 1 & Max_Counter == 1 & 
                                    (Date_Origination < maxDate | is.na(Date_Origination) ), .N]  / 
    datCredit_real[ExclusionID==0 & Counter == 1, .N] * 100 == 0
  cat( check_excl1_2 %?% 'SAFE: Exclusion 1 successfully re-applied.\n' %:% 
         'WARNING: Re-applying Exclusion 1 failed.\n')
}


# --- 2. Zero-balance credit histories | See Data Experiment 6

# - Preliminaries
datCredit_real[ExclusionID==0, HasZeroBalances_all := all(ifelse(Balance <= 0, TRUE, FALSE)), by=list(LoanID)]

# [DIAGNOSTIC] What proportion of account's credit histories have zero balances throughout?
diag.real6a_2 <- datCredit_real[ExclusionID == 0 & Counter == 1 & HasZeroBalances_all == TRUE, .N] /
  datCredit_real[ExclusionID == 0 & Counter == 1, .N] * 100
diag.real6a_2_abs <- datCredit_real[ExclusionID == 0 & HasZeroBalances_all == TRUE, .N]
diag.real6a_2_rec <- diag.real6a_2_abs / datCredit_real[ExclusionID == 0, .N] * 100

# - Conditional exclusion
if (diag.real6a_2 > 0) {
  
  cat("RE-EXCLUSION: Newly-revealed zero-balance credit histories. Prevalence: ", round(diag.real6a_2,digits=3),
      "% of accounts (", round(diag.real6a_2_rec,digits=4), "% of records).\n")
  
  # - Mark affected records for exclusion later with an ID-value
  LoanIDs <- unique(subset(datCredit_real, ExclusionID == 0 & 
                             Counter == 1 & HasZeroBalances_all == TRUE, select="LoanID"))
  datCredit_real[LoanID %in% LoanIDs$LoanID & ExclusionID == 0, ExclusionID := 7]
  
  # - Recalculate affected aggregates
  datCredit_real[ExclusionID==0, HasZeroBalances_all := all(ifelse(Balance <= 0, TRUE, FALSE)), by=list(LoanID)]
  
  # [SANITY CHECK] Treatment success?
  check_excl2_2 <-  datCredit_real[ExclusionID == 0 & Counter == 1 & HasZeroBalances_all == TRUE, .N] / 
    datCredit_real[ExclusionID==0 & Counter == 1, .N] * 100 == 0
  cat( check_excl2_2 %?% 'SAFE: Exclusion 2 successfully re-applied.\n' %:% 
         'WARNING: Re-applying Exclusion 2 failed.\n')
}


# --- 3. Zero-Principal credit histories | See Data Experiment 5

# - Preliminaries
datCredit_real[ExclusionID==0, HasZeroPrincipal_all := all(Principal==0), by=list(LoanID)]

# [DIAGNOSTIC] Prevalence of zero-valued Principals throughout the entire account history?
diag.real5_1d_2 <- datCredit_real[ExclusionID==0 & Counter==1 & HasZeroPrincipal_all==T, .N] /
  datCredit_real[ExclusionID==0 & Counter==1, .N] * 100 
diag.real5_1d_2_abs <- datCredit_real[ExclusionID==0 & HasZeroPrincipal_all==T, .N]
diag.real5_1d_2_rec <- diag.real5_1d_2_abs / datCredit_real[ExclusionID==0, .N] * 100 

# - Conditional treatment
if (diag.real5_1d_2 > 0) {
  
  cat("RE-EXCLUSION: Newly-revealed zero-principal credit histories. Prevalence: ", 
      round(diag.real5_1d_2,digits=3), "% of accounts (",
      round(diag.real5_1d_2_rec,digits=4), "% of records).\n")
  
  # - Mark affected records for exclusion later with an ID-value
  LoanIDs <- unique(subset(datCredit_real, ExclusionID==0 & Counter==1 & 
                             HasZeroPrincipal_all==T, select="LoanID"))
  datCredit_real[LoanID %in% LoanIDs$LoanID & ExclusionID == 0, ExclusionID := 7]
  
  # - Recalculate affected aggregates
  datCredit_real[ExclusionID==0, HasZeroPrincipal_all := all(Principal==0), by=list(LoanID)]
  
  # [SANITY CHECK] Treatment success?
  check_excl3_2 <-  datCredit_real[ExclusionID==0 & Counter==1 & HasZeroPrincipal_all==T, .N] / 
    datCredit_real[ExclusionID==0 & Counter == 1, .N] * 100 == 0
  cat( check_excl3_2 %?% 'SAFE: Exclusion 3 successfully re-applied.\n' %:% 
         'WARNING: Re-applying Exclusion 3 failed.\n')
}


# --- 4. Zero-Instalment credit histories | See Data Experiment 8

# - Preliminaries
datCredit_real[ExclusionID==0, HasZeroIns_All := ifelse(all(Instalment==0),T,F),by=list(LoanID)]
datCredit_real[ExclusionID==0, HasZeroArrears_All := ifelse(all(Arrears==0),T,F),by=list(LoanID)]

# [DIAGNOSTIC] Prevalence of account-level zero-valued instalments
diag.real8_1a_2 <- datCredit_real[ExclusionID==0 & Counter==1 & HasZeroIns_All==T, .N] / 
  datCredit_real[ExclusionID==0 & Counter==1, .N] * 100
diag.real8_1a_2_abs <- datCredit_real[ExclusionID==0 & HasZeroIns_All==T, .N]
diag.real8_1a_2_rec <- diag.real8_1a_2_abs / datCredit_real[ExclusionID==0, .N] * 100

# [DIAGNOSTIC] Prevalence of zero-valued arrears amongst zero-valued instalments?
diag.real8_1c_2 <- datCredit_real[ExclusionID==0 & Counter==1 & HasZeroIns_All==T & HasZeroArrears_All==T, .N] /
  datCredit_real[ExclusionID==0 & Counter==1 & HasZeroIns_All==T, .N] * 100

# - Conditional treatment
if (diag.real8_1a_2 > 0 & diag.real8_1c_2 >= 0.9) {
  
  cat("RE-EXCLUSION: Newly-revealed zero-instalment credit histories. Prevalence: ", 
      round(diag.real8_1a_2,digits=3), "% of accounts (",
      round(diag.real8_1a_2_rec,digits=4), "% of records), \n\tof which ", round(diag.real8_1c_2,digits=3),
      "% also have zero-valued arrears throughout.\n")
  
  # - Mark affected records for exclusion later with an ID-value
  LoanIDs <- unique(subset(datCredit_real, ExclusionID==0 & Counter==1 & 
                             HasZeroIns_All==T & HasZeroArrears_All==T, select="LoanID"))
  datCredit_real[LoanID %in% LoanIDs$LoanID & ExclusionID == 0, ExclusionID := 7]
  
  # - Recalculate affected aggregates
  datCredit_real[ExclusionID==0, HasZeroIns_All := ifelse(all(Instalment==0),T,F),by=list(LoanID)]
  
  # [SANITY CHECK] Treatment success?
  check_excl4_2 <- datCredit_real[ExclusionID==0 & Counter==1 & HasZeroIns_All==T, .N] / 
    datCredit_real[ExclusionID==0 & Counter == 1, .N] * 100 == 0
  cat( check_excl4_2 %?% 'SAFE: Exclusion 4 successfully re-applied.\n' %:% 
         'WARNING: Re-applying Exclusion 4 failed.\n')
}


# --- Create and add one big exclusion impact to a common table
datExcl <- data.table("Excl_ID"=8, "Reason"="Rerunning previous exclusions (NoTruEnd applied), given recent data treatments",
                      "Impact_Account" = diag.real4b_2 + diag.real6a_2 + diag.real5_1d_2 + diag.real8_1a_2,
                      "Impact_Dataset" = diag.real4b_2_rec + diag.real6a_2_rec + diag.real5_1d_2_rec + diag.real8_1a_2_rec,
                      "Impact_records" = diag.real4b_2_abs + diag.real6a_2_abs + diag.real5_1d_2_abs + diag.real8_1a_2_abs)
if (exists('datExclusions')) datExclusions <- rbind(datExclusions, datExcl) else datExclusions <- datExcl

# --- Clean up | Intermediary treatment-related fields no longer necessary
suppressWarnings( datCredit_real[,`:=`(HasZeroPrincipal_all = NULL, HasZeroBalances_all = NULL,
                                       HasZeroIns_All = NULL, HasZeroArrears_All = NULL)] )
gc()

# - Store experimental objects | Memory optimisation
pack.ffdf(paste0(genObjPath,"Exclusions-NoTruEnd"), datExclusions);





# ------ 8. General Cleanup | Remove fields that will not likely be used
suppressWarnings( datCredit_real[, `:=`(WOFF_DATE = NULL, RMNG_TERM = NULL, BOND_AMT = NULL, 
                                        FACILITY_SIZE = NULL, DATEX = NULL)] )
### NOTES: See Data Experiment 2 on WOFF_DATE's removal

# [SANITY CHECK] Confirm dataset's grain
check_cred3a <- datCredit_real[,list(Freqs = .N), by=list(LoanID, Date)][Freqs > 1,.N]
cat( (check_cred3a == 0) %?% cat('SAFE: Grain of {datCredit_real} confirmed.\n') %:% 
       cat(paste0('ERROR: Grain broken in {datCredit_real} for ', check_cred3b, " cases.\n")) )

# - Save to disk (zip) for quick disk-based retrieval later
rm(LoanIDs, datExcl); gc() # memory optimization; no need for raw data anymore at this point
pack.ffdf(paste0(genPath,"creditdata_final2-NoTruEnd"), datCredit_real)

# - Logging
cat('Data finalised, with TruEnd-procedure NOT applied.\n')
proc.time() - ptm3 # IGNORE: elapsed runtime
