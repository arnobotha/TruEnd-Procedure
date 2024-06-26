# ================================== TruEnd-procedure ===================================
# Compare result sets of TruEnd vs No-TruEnd via realised loss rate distributions
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: TruEnd-procedure
# SCRIPT AUTHOR(S): Dr Arno Botha
# ---------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R
#   - 2a.Data_Prepare_Credit_Basic.R
#   - 2b.Date_Prepare_Credit_Advanced.R
#   - 2c(i).Data_Prepare_Credit_TruEnd.R
#   - 2c(ii).Data_Prepare_Credit_NoTruEnd.R
#   - 2d(i).Data_Enrich_TruEnd.R
#   - 2d(ii).Data_Enrich_NoTruEnd.R
#
# -- Inputs:
#   - datCredit_real | Enhanced version of input dataset (script 2b)
#
# -- Outputs:
#   - <analytics>
# =======================================================================================




# ------ 1. Preliminaries

# --- 0. General
chosenFont <- "Cambria"

# --- 1. Load TruEnd-treated data

if (!exists('datCredit_real')) unpack.ffdf(paste0(genPath,"creditdata_final4-TruEnd"), tempPath)

# - Sample accordingly | First record per default spell, itself resolved
datCredit_TruEnd <- subset(datCredit_real, !is.na(DefSpell_Num) & DefSpellResol_Type_Hist != "Censored" & 
                             DefSpell_Counter==1, 
                         select=c("LoanID", "Date", "DefSpell_Key", "DefSpell_Num", "TimeInDefSpell","DefSpell_Age", 
                                  "DefSpellResol_Type_Hist", "Principal", "Balance", "InterestRate_Nom", "LossRate_Real")); gc()

# - Sample accordingly | Account-level summary
datCreditAggr_TruEnd <- datCredit_real[, list(DefSpells_Num = max(DefSpell_Num, na.rm=T), LoanAge = Age_Adj[.N], MaxCount = .N,
                                              Balance_End = Balance[.N], Principal_Start = Principal[1], 
                                              WOff = max(WOff_Ind, na.rm=T), Settle = max(EarlySettle_Ind, na.rm=T),
                                              Event_Type=Event_Type[.N], Event_Time = Event_Time[.N], 
                                              LossRate = mean(LossRate_Real,na.rm=T)), 
                                       by=list(LoanID)]
# - Cleanup
rm(datCredit_real); gc()


# --- 2. Load TruEnd-untreated data

if (!exists('datCredit_real')) unpack.ffdf(paste0(genPath,"creditdata_final4-NoTruEnd"), tempPath)

# - Sample accordingly | First record per default spell, itself resolved
datCredit_NoTruEnd <- subset(datCredit_real, !is.na(DefSpell_Num) & DefSpellResol_Type_Hist != "Censored" & 
                               DefSpell_Counter==1, 
                             select=c("LoanID", "Date", "DefSpell_Key", "DefSpell_Num", "TimeInDefSpell","DefSpell_Age", 
                                      "DefSpellResol_Type_Hist", "Principal", "Balance", "InterestRate_Nom", "LossRate_Real")); gc()

# - Sample accordingly | Account-level summary
datCreditAggr_NoTruEnd <- datCredit_real[, list(DefSpells_Num = max(DefSpell_Num, na.rm=T), LoanAge = Age_Adj[.N], MaxCount = .N,
                                              Balance_End = Balance[.N], Principal_Start = Principal[1], 
                                              WOff = max(WOff_Ind, na.rm=T), Settle = max(EarlySettle_Ind, na.rm=T),
                                              Event_Type=Event_Type[.N], Event_Time = Event_Time[.N], 
                                              LossRate = mean(LossRate_Real,na.rm=T)), 
                                         by=list(LoanID)]

# - Cleanup
rm(datCredit_real); gc()





# ------ 1. Analysis: LGD-Densities (TruEnd vs NoTruEnd)
# Graph the realised LGD statistical distributions of the two credit datasets, 
# respectively treated and untreated with the TruEnd-procedure


# --- 1. TruEnd-treated set

# - Treat out-of-bounds loss rates
(diag.oob.lossrate_TruEnd <- datCredit_TruEnd[LossRate_Real < 0 | LossRate_Real > 1, .N] / datCredit_TruEnd[DefSpellResol_Type_Hist  == "WOFF",.N] * 100)
datCredit_TruEnd[, OOB_Ind := ifelse(LossRate_Real < 0 | LossRate_Real > 1, 1,0)]

# - Filter out OOB-cases, purely for graphing purposes
datCredit_TruEnd_NOOB <- subset(datCredit_TruEnd, OOB_Ind == 0)

# - assign explicit resolution outcomes
datCredit_TruEnd_NOOB[, Event := ifelse(DefSpellResol_Type_Hist == "Cured", "Cure", "Write-off")]

# - scenario for facetting
datCredit_TruEnd_NOOB[, Scenario := "Treated with TruEnd-procedure"]

# - subset for miniplot
datCredit_TruEnd_W <- subset(datCredit_TruEnd_NOOB, Event == "Write-off")

# - graphing parameters
vCol <- brewer.pal(10, "Paired")[c(8,6)]

# - Aesthetic engineering: Statistical Summaries
meanLoss_TruEnd <- mean(datCredit_TruEnd_NOOB$LossRate_Real, na.rm=T)
maxDensLoss_TruEnd <- max(density(datCredit_TruEnd_NOOB$LossRate_Real)$y)
MeanLoss_TruEnd_W <- mean(datCredit_TruEnd_W$LossRate_Real, na.rm=T)
maxDensLoss_TruEnd_W <- max(density(datCredit_TruEnd_W$LossRate_Real)$y)
mix_WC_TruEnd <- datCredit_TruEnd_W[, .N] / datCredit_TruEnd_NOOB[, .N] # overall write-off probability given default of 18%

# - main graphs a) Overall LGD distribution
(g1 <- ggplot(datCredit_TruEnd_NOOB, aes(x=LossRate_Real)) + theme_bw() +
    geom_histogram(aes(y=after_stat(density)), alpha=0.4, bins=round(2*datCredit_TruEnd_NOOB[,.N]^(1/3)),
                   position="identity", fill=vCol[1], colour=vCol[1]) + 
    geom_density(linewidth=1, colour=vCol[1], linetype="dotted") + 
    geom_vline(xintercept=meanLoss_TruEnd, linewidth=0.6, colour=vCol[1], linetype="dashed") + 
    annotate(geom="text", x=meanLoss_TruEnd*0.8, y=20, family=chosenFont,
             label = paste0("Mean Loss: ", sprintf("%.1f", meanLoss_TruEnd*100), "%"), size=3, colour=vCol[1], angle=90) +     
    # facets & scale options
    labs(x=bquote({Realised~loss~rate~italic(l)}), 
         y="Histogram and density of losses for resolved defaults [cures/write-offs]") + 
    theme(text=element_text(family=chosenFont),legend.position="bottom",
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text = element_text(size=8, colour="gray50"), 
          strip.text.y.right = element_text(angle=90)) + 
    facet_grid(Scenario ~., scales="free") +  
    scale_x_continuous(breaks=pretty_breaks(), label=percent)
)

# - miniplot for main graph | Write-offs only
(g2 <- ggplot(datCredit_TruEnd_W, aes(x=LossRate_Real)) + theme_bw() +
    geom_histogram(aes(y=after_stat(density)), alpha=0.4, bins=round(2*datCredit_TruEnd_W[,.N]^(1/3)), 
                   position="identity", fill=vCol[2], colour=vCol[2]) + 
    geom_density(linewidth=1, colour=vCol[2], linetype="dotted") + 
    geom_vline(xintercept=MeanLoss_TruEnd_W, linewidth=0.6, colour=vCol[2], linetype="dashed") + 
    annotate(geom="text", x=MeanLoss_TruEnd_W*0.93,  y=3, family=chosenFont,
             label = paste0("Mean Loss: ", sprintf("%.1f", MeanLoss_TruEnd_W*100), "%"), size=3, colour=vCol[2], angle=90) +     
    # facets & scale options
    labs(x="", y="", title=paste0("Write-offs only (", sprintf("%.0f", mix_WC_TruEnd*100), "%)")) + 
    theme(legend.position="none", text=element_text(size=12, family="Cambria"),
          #specific for plot-in-plot
          axis.text.y=element_text(margin=unit(c(0,0,0,0), "mm"), size=9),
          axis.text.x=element_text(margin=unit(c(0,0,0,0), "mm"), size=9),
          axis.ticks=element_blank(), axis.title.x=element_blank(), #axis.title.y=element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(color="black", fill="white"),
          plot.background = element_rect(color="white"), plot.margin = unit(c(0,0,0,0),"mm"),
          plot.title = element_text(hjust=0.55,vjust=-10,margin=margin(t=-12))
    ) + 
    scale_x_continuous(breaks=pretty_breaks(), label=percent)
)

ymin <- diff(ggplot_build(g1)$layout$panel_params[[1]]$y.range) * 0.2
ymax <- max(ggplot_build(g1)$layout$panel_params[[1]]$y.range) * 0.95
(plot.full <- g1 + annotation_custom(grob = ggplotGrob(g2), xmin=0.1, xmax=0.9, ymin=ymin, ymax=ymax))

# - save plot
dpi <- 180
ggsave(plot.full, file=paste0(genFigPath,"/LGD-Density_ResolvedDefaults_TruEnd.png"),width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")




# --- 2. Untreated dataset

# - Treat out-of-bounds loss rates
(diag.oob.lossrate_NoTruEnd <- datCredit_NoTruEnd[LossRate_Real < 0 | LossRate_Real > 1, .N] / datCredit_NoTruEnd[DefSpellResol_Type_Hist  == "WOFF",.N] * 100)
datCredit_NoTruEnd[, OOB_Ind := ifelse(LossRate_Real < 0 | LossRate_Real > 1, 1,0)]

# - Filter out OOB-cases, purely for graphing purposes
datCredit_NoTruEnd_NOOB <- subset(datCredit_NoTruEnd, OOB_Ind == 0)

# - assign explicit resolution outcomes
datCredit_NoTruEnd_NOOB[, Event := ifelse(DefSpellResol_Type_Hist == "Cured", "Cure", "Write-off")]

# - scenario for facetting
datCredit_NoTruEnd_NOOB[, Scenario := "Untreated with TruEnd-procedure"]

# - subset for miniplot
datCredit_NoTruEnd_W <- subset(datCredit_NoTruEnd_NOOB, Event == "Write-off")

# - graphing parameters
vCol <- brewer.pal(10, "Paired")[c(8,6)]

# - Aesthetic engineering: Statistical Summaries
meanLoss_NoTruEnd <- mean(datCredit_NoTruEnd_NOOB$LossRate_Real, na.rm=T)
maxDensLoss_NoTruEnd <- max(density(datCredit_NoTruEnd_NOOB$LossRate_Real)$y)
MeanLoss_NoTruEnd_W <- mean(datCredit_NoTruEnd_W$LossRate_Real, na.rm=T)
maxDensLoss_NoTruEnd_W <- max(density(datCredit_NoTruEnd_W$LossRate_Real)$y)
mix_WC_NoTruEnd <- datCredit_NoTruEnd_W[, .N] / datCredit_NoTruEnd_NOOB[, .N]

# - main graphs a) Overall LGD distribution
(g1 <- ggplot(datCredit_NoTruEnd_NOOB, aes(x=LossRate_Real)) + theme_bw() +
    geom_histogram(aes(y=after_stat(density)), alpha=0.4, bins=round(2*datCredit_NoTruEnd_NOOB[,.N]^(1/3)),
                   position="identity", fill=vCol[1], colour=vCol[1]) + 
    geom_density(linewidth=1, colour=vCol[1], linetype="dotted") + 
    geom_vline(xintercept=meanLoss_NoTruEnd, linewidth=0.6, colour=vCol[1], linetype="dashed") + 
    annotate(geom="text", x=meanLoss_NoTruEnd*0.8, y=20, family=chosenFont,
             label = paste0("Mean Loss: ", sprintf("%.1f", meanLoss_NoTruEnd*100), "%"), size=3, colour=vCol[1], angle=90) +     
    # facets & scale options
    labs(x=bquote({Realised~loss~rate~italic(l)}), 
         y="Histogram and density of losses for resolved defaults [cures/write-offs]") + 
    theme(text=element_text(family=chosenFont),legend.position="bottom",
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text = element_text(size=8, colour="gray50"), 
          strip.text.y.right = element_text(angle=90)) + 
    facet_grid(Scenario ~., scales="free") +  
    scale_x_continuous(breaks=pretty_breaks(), label=percent)
)

# - miniplot for main graph | Write-offs only
(g2 <- ggplot(datCredit_NoTruEnd_W, aes(x=LossRate_Real)) + theme_bw() +
    geom_histogram(aes(y=after_stat(density)), alpha=0.4, bins=round(2*datCredit_NoTruEnd_W[,.N]^(1/3)), 
                   position="identity", fill=vCol[2], colour=vCol[2]) + 
    geom_density(linewidth=1, colour=vCol[2], linetype="dotted") + 
    geom_vline(xintercept=MeanLoss_NoTruEnd_W, linewidth=0.6, colour=vCol[2], linetype="dashed") + 
    annotate(geom="text", x=MeanLoss_NoTruEnd_W*0.93,  y=3, family=chosenFont,
             label = paste0("Mean Loss: ", sprintf("%.1f", MeanLoss_NoTruEnd_W*100), "%"), size=3, colour=vCol[2], angle=90) +     
    # facets & scale options
    labs(x="", y="", title=paste0("Write-offs only (", sprintf("%.0f", mix_WC_NoTruEnd*100), "%)")) + 
    theme(legend.position="none", text=element_text(size=12, family="Cambria"),
          #specific for plot-in-plot
          axis.text.y=element_text(margin=unit(c(0,0,0,0), "mm"), size=9),
          axis.text.x=element_text(margin=unit(c(0,0,0,0), "mm"), size=9),
          axis.ticks=element_blank(), axis.title.x=element_blank(), #axis.title.y=element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(color="black", fill="white"),
          plot.background = element_rect(color="white"), plot.margin = unit(c(0,0,0,0),"mm"),
          plot.title = element_text(hjust=0.55,vjust=-10,margin=margin(t=-12))
    ) + 
    scale_x_continuous(breaks=pretty_breaks(), label=percent)
)

ymin <- diff(ggplot_build(g1)$layout$panel_params[[1]]$y.range) * 0.2
ymax <- max(ggplot_build(g1)$layout$panel_params[[1]]$y.range) * 0.95
(plot.full <- g1 + annotation_custom(grob = ggplotGrob(g2), xmin=0.1, xmax=0.9, ymin=ymin, ymax=ymax))

# - save plot
dpi <- 180
ggsave(plot.full, file=paste0(genFigPath,"/LGD-Density_ResolvedDefaults_NoTruEnd.png"),width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")



# --- 3. Combined graph: Treated + Untreated dataset | Write-offs only

# - combine respective datasets
datPlot <- rbind(data.table(datCredit_TruEnd_W, Dataset="a_TruEnd"),
  data.table(datCredit_NoTruEnd_W, Dataset="b_NoTruEnd"))

# - Aesthetic engineering: Statistical Summaries
datAnnotate <- data.table(MeanLoss=c(MeanLoss_TruEnd_W,MeanLoss_NoTruEnd_W), Dataset=c("a_TruEnd","b_NoTruEnd"), 
                          Label=c(paste0("Mean Loss: ", sprintf("%.1f", MeanLoss_TruEnd_W*100), "%; TruEnd"),
                                  paste0("Mean Loss: ", sprintf("%.1f", MeanLoss_NoTruEnd_W*100), "%; No TruEnd")),
                          Label_x=c(0.7,0.7), 
                          Label_y=c(maxDensLoss_TruEnd_W*1.4, maxDensLoss_NoTruEnd_W*1.2))

# - scenario for facetting
datPlot[, Scenario := "Write-offs only"]

# - graphing parameters
vCol <- brewer.pal(8, "Dark2")[c(1,2)]
labels.v <- c("a_TruEnd"="TruEnd", "b_NoTruEnd"="No TruEnd")

# - main graphs a) Overall LGD distribution
(g1 <- ggplot(datPlot, aes(x=LossRate_Real, group=Dataset)) + theme_bw() +
    geom_histogram(aes(y=after_stat(density), colour=Dataset, fill=Dataset), alpha=0.3, #bins=round(2*datCredit_NoTruEnd_W[,.N]^(1/3)), 
                   position="identity", linewidth=0.3) + 
    geom_density(aes(colour=Dataset, linetype=Dataset), linewidth=0.3) + 
    geom_vline(data=datAnnotate, aes(xintercept=MeanLoss, colour=Dataset, linetype=Dataset), linewidth=0.6) + 
    geom_label(data=datAnnotate, aes(x=Label_x, y=Label_y, label=Label, colour=Dataset), size=3, family=chosenFont, show.legend=F) + 
    # facets & scale options
    facet_grid(Scenario ~., scales="free") +  
    labs(x=bquote({Realised~loss~rate~italic(l)}), 
         y="Histogram and density of losses") + 
    theme(text=element_text(family=chosenFont),legend.position="bottom",
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text = element_text(size=8, colour="gray50"), 
          strip.text.y.right = element_text(angle=90)) + 
    scale_color_manual(name="Dataset", labels=labels.v, values=vCol) + 
    scale_fill_manual(name="Dataset", labels=labels.v, values=vCol) + 
    scale_linetype_discrete(name="Dataset", labels=labels.v) + 
    scale_x_continuous(breaks=pretty_breaks(), label=percent)
)

# - save plot
dpi <- 260
ggsave(g1, file=paste0(genFigPath,"/LGD-Densities_WriteOffs.png"),width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")

# - Analyse impact of TruEnd regarding monetary value of resolved defaulted population (cures/write-offs)
describe(datCredit_NoTruEnd_NOOB$Balance); hist(datCredit_NoTruEnd_NOOB$Balance, breaks="FD"); comma(sum(datCredit_NoTruEnd_NOOB$Balance))
describe(datCredit_TruEnd_NOOB$Balance); hist(datCredit_TruEnd_NOOB$Balance, breaks="FD"); comma(sum(datCredit_TruEnd_NOOB$Balance))
### RESULTS: TruEnd-dataset has fewer defaults than NoTruEnd-dataset, but higher mean (and total) EAD; not immediately obvious why

# - Analyse impact of TruEnd regarding monetary value of written-off population
describe(datCredit_NoTruEnd_W$LossRate_Real)
describe(datCredit_TruEnd_W$LossRate_Real)
describe(datCredit_NoTruEnd_W$Balance); hist(datCredit_NoTruEnd_W$Balance, breaks="FD"); comma(sum(datCredit_NoTruEnd_W$Balance))
describe(datCredit_TruEnd_W$Balance); hist(datCredit_TruEnd_W$Balance, breaks="FD"); comma(sum(datCredit_TruEnd_W$Balance))
### RESULTS: TruEnd-dataset has fewer write-offs than NoTruEnd-dataset, but higher mean (and total) EAD; not immediately obvious why

# - Find common write-offs between both sets for fair analysis
loanIDs <- intersect(datCredit_NoTruEnd_W$LoanID, datCredit_TruEnd_W$LoanID)
# [DIAGNOSTIC] Degree of commonality between sets
diag.real.an_1a <- NROW(loanIDs) / max(NROW(datCredit_NoTruEnd_W), NROW(datCredit_TruEnd_W))
cat("NOTE: Degree of commonality between raw dataset (", comma(NROW(datCredit_NoTruEnd_W)),
    ") and TruEnd-treated dataset (", comma(NROW(datCredit_TruEnd_W)),
    ") is ", percent(diag.real.an_1a, accuracy=0.1), ".\n")
### RESULTS: Very high commanality (98%)
# [DIAGNOSTIC] Degree of inequality in EADs between sets
datCompare <- merge(datCredit_NoTruEnd_W[,list(LoanID, DefSpell_Key, Balance, LossRate_Real)], 
                    datCredit_TruEnd_W[,list(LoanID, DefSpell_Key, Balance, LossRate_Real)], by=c("LoanID", "DefSpell_Key"))
diag.real.an_1b <- datCompare[Balance.x != Balance.y, .N] / datCompare[, .N]
cat( (diag.real.an_1b == 0) %?% 
  "SAFE: EADs between raw dataset and TruEnd-treated dataset are equal for common write-offs.\n" %:%
  paste0("ERROR: EADs between raw dataset and TruEnd-treated dataset are not equal for common write-offs, with", percent(diag.real.an_1),
         " difference.\n"))
datCompare[, list(EAD_sum_NoTruEnd = comma(sum(Balance.x)), EAD_sum_TruEnd = comma(sum(Balance.x)))]

# - Find set difference of written-off population between both sets
loanIDs_diffs <- setdiff(datCredit_NoTruEnd_W$LoanID, datCredit_TruEnd_W$LoanID)
# [DIAGNOSTIC] Degree of difference between sets
diag.real.an_1c <- NROW(loanIDs_diffs) / NROW(datCredit_NoTruEnd_W)
cat("NOTE: Degree of difference between raw dataset (", comma(NROW(datCredit_NoTruEnd_W)),
    ") and the set difference (",comma(NROW(loanIDs_diffs)),") with the TruEnd-treated dataset (", comma(NROW(datCredit_TruEnd_W)),
    ") is ", percent(diag.real.an_1c, accuracy=0.1), ".\n")
describe(datCredit_NoTruEnd_W[LoanID %in% loanIDs_diffs, Balance])
# [DIAGNOSTIC] Proportion of balances <= than optimal policy from TruEnd-procedure
diag.real.an_1d <- datCredit_NoTruEnd_W[LoanID %in% loanIDs_diffs & Balance <= currThresh, .N] /
  datCredit_NoTruEnd_W[LoanID %in% loanIDs_diffs, .N]
cat("NOTE: Within the previous set difference, the proportion of balances <= than the optimal policy b^*=",
    comma(currThresh), " is ", percent(diag.real.an_1d, accuracy=0.1), ".\n")
### RESULTS: Balances appears very small, which are however confirmed to be <= the b^*policy used in the TruEnd-procedure
# Therefore, the set difference universally consists of small-balance defaults, thereby explaining their exclusion in the
# TruEnd-treated dataset.


# - Analyse impact of TruEnd regarding monetary value of written-off population
# Use the reduced mean loss rate from TruEnd-treated dataset as a rough 'forecast'
CreditLoss_NoTruEnd <- sum(datCompare$Balance.x, na.rm=T) * meanLoss_NoTruEnd
CreditLoss_TruEnd <- sum(datCompare$Balance.y, na.rm=T) * meanLoss_TruEnd
cat("NOTE: Average credit loss amount for raw dataset using \\bar{l}=", percent(meanLoss_NoTruEnd,accuracy=0.1), " as forecast: ",
    comma(CreditLoss_NoTruEnd), ".\n")
cat("NOTE: Average credit loss amount for TruEnd-treated dataset using \\bar{l}=", percent(meanLoss_TruEnd,accuracy=0.1), " as forecast: ",
    comma(CreditLoss_TruEnd), ".\n")
cat("NOTE: Difference: ", comma(CreditLoss_NoTruEnd-CreditLoss_TruEnd))



# ------ 2. Analysis: Account Age Densities (TruEnd vs NoTruEnd)
# Graph the statistical distributions of account age for the two credit datasets, 
# respectively treated and untreated with the TruEnd-procedure


# --- 1. TruEnd-treated set

# - scenario for facetting
datCreditAggr_TruEnd[, Scenario := "Treated with TruEnd-procedure"]

# - subset for miniplot
datCreditAggr_TruEnd_W <- subset(datCreditAggr_TruEnd, WOff  == 1)

# - graphing parameters
vCol <- brewer.pal(10, "Paired")[c(4,6)]; xLimit <- 500

# - Aesthetic engineering: Statistical Summaries
meanAge_TruEnd <- mean(datCreditAggr_TruEnd$LoanAge, na.rm=T)
maxDensAge_TruEnd <- max(density(datCreditAggr_TruEnd$LoanAge)$y)
meanAge_TruEnd_W <- mean(datCreditAggr_TruEnd_W$LoanAge, na.rm=T)
maxDensAge_TruEnd_W <- max(density(datCreditAggr_TruEnd_W$LoanAge)$y)
mix_WOff_TruEnd <- datCreditAggr_TruEnd_W[, .N] / datCreditAggr_TruEnd[, .N] # overall write-off probability of 5%

# - main graphs a) Overall age distribution
(g1 <- ggplot(datCreditAggr_TruEnd[LoanAge <= xLimit,], aes(x=LoanAge)) + theme_bw() +
    geom_histogram(aes(y=after_stat(density)), alpha=0.4, #bins=round(2*datCreditAggr_TruEnd[,.N]^(1/3)),
                   position="identity", fill=vCol[1], colour=vCol[1]) + 
    geom_density(linewidth=1, colour=vCol[1], linetype="dotted") + 
    geom_vline(xintercept=meanAge_TruEnd, linewidth=0.6, colour=vCol[1], linetype="dashed") + 
    annotate(geom="text", x=meanAge_TruEnd*0.9, y=maxDensAge_TruEnd*0.8, family=chosenFont,
             label = paste0("Mean Age: ", sprintf("%.1f", meanAge_TruEnd), " months"), size=3, colour=vCol[1], angle=90) +     
    # facets & scale options
    labs(x="Account Age (months)", y="Histogram and density of account lifetimes") + 
    theme(text=element_text(family=chosenFont),legend.position="bottom",
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text = element_text(size=8, colour="gray50"), 
          strip.text.y.right = element_text(angle=90)) + 
    facet_grid(Scenario ~., scales="free") +  
    scale_x_continuous(breaks=pretty_breaks(), label=comma)
)

# - miniplot for main graph | Write-offs only
(g2 <- ggplot(datCreditAggr_TruEnd_W[LoanAge <= xLimit,], aes(x=LoanAge)) + theme_bw() +
    geom_histogram(aes(y=after_stat(density)), alpha=0.4, #bins=round(2*datCredit_TruEnd_W[,.N]^(1/3)), 
                   position="identity", fill=vCol[2], colour=vCol[2]) + 
    geom_density(linewidth=1, colour=vCol[2], linetype="dotted") + 
    geom_vline(xintercept=meanAge_TruEnd_W, linewidth=0.6, colour=vCol[2], linetype="dashed") + 
    annotate(geom="text", x=meanAge_TruEnd_W*0.87,  y=maxDensAge_TruEnd_W*0.8, family=chosenFont,
             label = paste0("Mean Age: ", sprintf("%.1f", meanAge_TruEnd_W), " months"), size=3, colour=vCol[2], angle=90) +     
    # facets & scale options
    labs(x="", y="", title=paste0("Write-offs only (", sprintf("%.0f", mix_WOff_TruEnd*100), "%)")) + 
    theme(legend.position="none", text=element_text(size=12, family="Cambria"),
          #specific for plot-in-plot
          axis.text.y=element_text(margin=unit(c(0,0,0,0), "mm"), size=9),
          axis.text.x=element_text(margin=unit(c(0,0,0,0), "mm"), size=9),
          axis.ticks=element_blank(), axis.title.x=element_blank(), #axis.title.y=element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(color="black", fill="white"),
          plot.background = element_rect(color="white"), plot.margin = unit(c(0,0,0,0),"mm"),
          plot.title = element_text(hjust=0.55,vjust=-10,margin=margin(t=-12))
    ) + 
    scale_x_continuous(breaks=pretty_breaks(), label=comma)
)

ymin <- diff(ggplot_build(g1)$layout$panel_params[[1]]$y.range) * 0.4
ymax <- max(ggplot_build(g1)$layout$panel_params[[1]]$y.range) * 0.975
(plot.full <- g1 + annotation_custom(grob = ggplotGrob(g2), xmin=100, xmax=xLimit, ymin=ymin, ymax=ymax))

# - save plot
dpi <- 180
ggsave(plot.full, file=paste0(genFigPath,"/AccountAge-Density_TruEnd.png"),width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")




# --- 2. Untreated dataset

# - scenario for facetting
datCreditAggr_NoTruEnd[, Scenario := "Untreated with TruEnd-procedure"]

# - subset for miniplot
datCreditAggr_NoTruEnd_W <- subset(datCreditAggr_NoTruEnd, WOff  == 1)

# - graphing parameters
vCol <- brewer.pal(10, "Paired")[c(4,6)]; xLimit <- 500

# - Aesthetic engineering: Statistical Summaries
meanAge_NoTruEnd <- mean(datCreditAggr_NoTruEnd$LoanAge, na.rm=T)
maxDensAge_NoTruEnd <- max(density(datCreditAggr_NoTruEnd$LoanAge)$y)
meanAge_NoTruEnd_W <- mean(datCreditAggr_NoTruEnd_W$LoanAge, na.rm=T)
maxDensAge_NoTruEnd_W <- max(density(datCreditAggr_NoTruEnd_W$LoanAge)$y)
mix_WOff_NoTruEnd <- datCreditAggr_NoTruEnd_W[, .N] / datCreditAggr_NoTruEnd[, .N] # overall write-off probability of 5%

# - main graphs a) Overall age distribution
(g1 <- ggplot(datCreditAggr_NoTruEnd[LoanAge <= xLimit,], aes(x=LoanAge)) + theme_bw() +
    geom_histogram(aes(y=after_stat(density)), alpha=0.4, #bins=round(2*datCreditAggr_TruEnd[,.N]^(1/3)),
                   position="identity", fill=vCol[1], colour=vCol[1]) + 
    geom_density(linewidth=1, colour=vCol[1], linetype="dotted") + 
    geom_vline(xintercept=meanAge_NoTruEnd, linewidth=0.6, colour=vCol[1], linetype="dashed") + 
    annotate(geom="text", x=meanAge_NoTruEnd*0.9, y=maxDensAge_NoTruEnd*0.8, family=chosenFont,
             label = paste0("Mean Age: ", sprintf("%.1f", meanAge_NoTruEnd), " months"), size=3, colour=vCol[1], angle=90) +     
    # facets & scale options
    labs(x="Account Age (months)", y="Histogram and density of account lifetimes") + 
    theme(text=element_text(family=chosenFont),legend.position="bottom",
          strip.background=element_rect(fill="snow2", colour="snow2"),
          strip.text = element_text(size=8, colour="gray50"), 
          strip.text.y.right = element_text(angle=90)) + 
    facet_grid(Scenario ~., scales="free") +  
    scale_x_continuous(breaks=pretty_breaks(), label=comma)
)

# - miniplot for main graph | Write-offs only
(g2 <- ggplot(datCreditAggr_NoTruEnd_W[LoanAge <= xLimit,], aes(x=LoanAge)) + theme_bw() +
    geom_histogram(aes(y=after_stat(density)), alpha=0.4, #bins=round(2*datCredit_TruEnd_W[,.N]^(1/3)), 
                   position="identity", fill=vCol[2], colour=vCol[2]) + 
    geom_density(linewidth=1, colour=vCol[2], linetype="dotted") + 
    geom_vline(xintercept=meanAge_NoTruEnd_W, linewidth=0.6, colour=vCol[2], linetype="dashed") + 
    annotate(geom="text", x=meanAge_NoTruEnd_W*0.87,  y=maxDensAge_NoTruEnd_W*0.8, family=chosenFont,
             label = paste0("Mean Age: ", sprintf("%.1f", meanAge_NoTruEnd_W), " months"), size=3, colour=vCol[2], angle=90) +     
    # facets & scale options
    labs(x="", y="", title=paste0("Write-offs only (", sprintf("%.0f", mix_WOff_NoTruEnd*100), "%)")) + 
    theme(legend.position="none", text=element_text(size=12, family="Cambria"),
          #specific for plot-in-plot
          axis.text.y=element_text(margin=unit(c(0,0,0,0), "mm"), size=9),
          axis.text.x=element_text(margin=unit(c(0,0,0,0), "mm"), size=9),
          axis.ticks=element_blank(), axis.title.x=element_blank(), #axis.title.y=element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_rect(color="black", fill="white"),
          plot.background = element_rect(color="white"), plot.margin = unit(c(0,0,0,0),"mm"),
          plot.title = element_text(hjust=0.55,vjust=-10,margin=margin(t=-12))
    ) + 
    scale_x_continuous(breaks=pretty_breaks(), label=comma)
)

ymin <- diff(ggplot_build(g1)$layout$panel_params[[1]]$y.range) * 0.40
ymax <- max(ggplot_build(g1)$layout$panel_params[[1]]$y.range) * 0.975
(plot.full <- g1 + annotation_custom(grob = ggplotGrob(g2), xmin=110, xmax=xLimit, ymin=ymin, ymax=ymax))

# - save plot
dpi <- 180
ggsave(plot.full, file=paste0(genFigPath,"/AccountAge-Density_NoTruEnd.png"),width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")




# --- 3. Account ages | Combined graph: Treated + Untreated dataset

# - combine respective datasets
datPlot <- rbind(data.table(datCreditAggr_TruEnd[, list(LoanID, LoanAge)], Dataset="a_TruEnd"),
                 data.table(datCreditAggr_NoTruEnd[, list(LoanID, LoanAge)], Dataset="b_NoTruEnd"))


# - Aesthetic engineering: Statistical Summaries
datAnnotate <- data.table(MeanAge=c(meanAge_TruEnd, meanAge_NoTruEnd), Dataset=c("a_TruEnd","b_NoTruEnd"), 
                          Label=c(paste0("Mean age: ", sprintf("%.1f", meanAge_TruEnd), " months; TruEnd"),
                                  paste0("Mean age: ", sprintf("%.1f", meanAge_NoTruEnd), " months; No TruEnd")),
                          Label_x=c(300,300), 
                          Label_y=c(maxDensAge_TruEnd*0.6,maxDensAge_NoTruEnd*0.5))

# - graphing parameters
vCol <- brewer.pal(8, "Dark2")[c(1,2)]; xLimit <- 500
labels.v <- c("a_TruEnd"="TruEnd", "b_NoTruEnd"="No TruEnd")

# - main graphs a) Overall LGD distribution
(g1 <- ggplot(datPlot[LoanAge <= xLimit, ], aes(x=LoanAge, group=Dataset)) + theme_bw() +
    geom_histogram(aes(y=after_stat(density), colour=Dataset, fill=Dataset), alpha=0.3, #bins=round(2*datCredit_NoTruEnd_W[,.N]^(1/3)), 
                   position="identity", linewidth=0.3) + 
    geom_density(aes(colour=Dataset, linetype=Dataset), linewidth=0.3) + 
    geom_vline(data=datAnnotate, aes(xintercept=MeanAge, colour=Dataset, linetype=Dataset), linewidth=0.6) + 
    geom_label(data=datAnnotate, aes(x=Label_x, y=Label_y, label=Label, colour=Dataset), size=3, family=chosenFont, show.legend=F) + 
    # facets & scale options
    labs(x="Account Age (months)", y="Histogram and density of account lifetimes") + 
    theme(text=element_text(family=chosenFont),legend.position="bottom") + 
    scale_color_manual(name="Dataset", labels=labels.v, values=vCol) + 
    scale_fill_manual(name="Dataset", labels=labels.v, values=vCol) + 
    scale_linetype_discrete(name="Dataset", labels=labels.v) + 
    scale_x_continuous(breaks=pretty_breaks(), label=comma)
)

# - save plot
dpi <- 260
ggsave(g1, file=paste0(genFigPath,"/AccountAge-Densities.png"),width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")





# ------ 3. Analysis: Account-level summaries: TruEnd vs NoTruEnd
# Just a quick investigation

# - General
describe(datCreditAggr_TruEnd$LoanAge); hist(datCreditAggr_TruEnd$LoanAge, breaks="FD") # Mean: 97.9
describe(datCreditAggr_NoTruEnd$LoanAge); hist(datCreditAggr_NoTruEnd$LoanAge, breaks="FD") # Mean: 100.9
describe(datCreditAggr_TruEnd$MaxCount); hist(datCreditAggr_TruEnd$MaxCount, breaks="FD") # Mean: 73.35
describe(datCreditAggr_TruEnd$DefSpells_Num); hist(datCreditAggr_TruEnd$DefSpells_Num, breaks="FD")
describe(datCreditAggr_TruEnd$Event_Type)
describe(datCreditAggr_TruEnd[DefSpells_Num>0 & Event_Type != "ACTIVE", WOff]) # Should represent ~ 19% write-off resolution of defaults
describe(datCreditAggr_TruEnd[Event_Type == "WOFF", LossRate]) # Should represent ~ 19% write-off resolution of defaults

# - Loan Age
describe(datCreditAggr_TruEnd[DefSpells_Num>0, LoanAge]) # Mean loan age (defaults): 165.5
describe(datCreditAggr_TruEnd[DefSpells_Num>0 & WOff==1, LoanAge]) # Mean loan age (write-offs): 92
hist(datCreditAggr_TruEnd[DefSpells_Num>0 & WOff==1, LoanAge], breaks="FD")




# - Cleanup
rm(datCredit_TruEnd_W, datCredit_NoTruEnd_W, g1, g2, plot.full, datPlot, datAnnotate,
   datCredit_NoTruEnd, datCredit_TruEnd, datCreditAggr_NoTruEnd, datCreditAggr_TruEnd,
   datCredit_NoTruEnd_NOOB, datCredit_TruEnd_NOOB, datCreditAggr_TruEnd_W, datCreditAggr_NoTruEnd_W, datCompare); gc()
