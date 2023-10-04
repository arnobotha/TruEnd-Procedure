# ================================== TruEnd-procedure ===================================
# Compare resultsets of TruEnd vs No-TruEnd via realised loss rate distributions
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
#   - <optimisation results>
# =======================================================================================




# ------ 1. Preliminaries

# --- 0. General
chosenFont <- "Cambria"
dpi <- 180 # graphical scaling factor

# --- 1. Load TruEnd-treated data

if (!exists('datCredit_real')) unpack.ffdf(paste0(genPath,"creditdata_final3-TruEnd"), tempPath)

# - Sample accordingly | First record per default spell, itself resolved
datCredit_TruEnd <- subset(datCredit_real, ExclusionID==0 & !is.na(DefSpell_Num) & DefSpellResol_Type_Hist != "Censored" & 
                             DefSpell_Counter==1, 
                         select=c("LoanID", "Date", "DefSpell_Key", "DefSpell_Num", "TimeInDefSpell","DefSpell_Age", 
                                  "DefSpellResol_Type_Hist", "Principal", "Balance", "InterestRate_Nom", "LossRate_Real")); gc()

# - Sample accordingly | Account-level summary
datCreditAggr_TruEnd <- datCredit_real[ExclusionID==0, 
                                         list(DefSpells_Num = max(DefSpell_Num, na.rm=T), LoanAge = Age_Adj[.N],
                                              Balance_End = Balance[.N], Principal_Start = Principal[1], 
                                              WOff = max(WOff_Ind, na.rm=T), ZeroBal_Start = max(ZeroBal_Start, na.rm=T),
                                              Event_Type=Event_Type[.N], Event_Time = Event_Time[.N], LossRate = mean(LossRate_Real,na.rm=T)), 
                                       by=list(LoanID)]
# - Cleanup
rm(datCredit_real); gc()


# --- 2. Load TruEnd-untreated data

if (!exists('datCredit_real')) unpack.ffdf(paste0(genPath,"creditdata_final3-NoTruEnd"), tempPath)

# - Sample accordingly | First record per default spell, itself resolved
datCredit_NoTruEnd <- subset(datCredit_real, ExclusionID==0 & !is.na(DefSpell_Num) & DefSpellResol_Type_Hist != "Censored" & 
                               DefSpell_Counter==1, 
                             select=c("LoanID", "Date", "DefSpell_Key", "DefSpell_Num", "TimeInDefSpell","DefSpell_Age", 
                                      "DefSpellResol_Type_Hist", "Principal", "Balance", "InterestRate_Nom", "LossRate_Real")); gc()

# - Sample accordingly | Account-level summary
datCreditAggr_NoTruEnd <- datCredit_real[ExclusionID==0, 
                                         list(DefSpells_Num = max(DefSpell_Num, na.rm=T), LoanAge = Age_Adj[.N],
                                              Balance_End = Balance[.N], Principal_Start = Principal[1], 
                                              WOff = max(WOff_Ind, na.rm=T), ZeroBal_Start = max(ZeroBal_Start, na.rm=T),
                                              Event_Type=Event_Type[.N], Event_Time = Event_Time[.N], LossRate = mean(LossRate_Real,na.rm=T)), 
                                         by=list(LoanID)]


# - Cleanup
rm(datCredit_real); gc()





# ------ 1. Analysis: LGD-Densities (TruEnd vs NoTruEnd)
# Graph the realised LGD statistical distributions of the two credit datasets, 
# respectively treated and untreated with the TruEnd-procedure


# --- 1. TruEnd-treated set

# - Treat out-of-bounds loss rates
(diag.oob.lossrate_TruEnd <- datCredit_TruEnd[LossRate_Real < 0 | LossRate_Real > 1, .N] / datCredit_TruEnd_W[,.N] * 100)
### AB: Significant proportion of oob-cases ... are we sure there are no errors in calc?





### SCRATCH-START

# - Distributional analysis on negative loss rates
describe(datCredit_TruEnd[LossRate_Real<0, LossRate_Real])
### RESULTS: left-skewed distribution, mean of -195% and median of -20.7%. Very large negative outliers, up to -129,611%
hist(datCredit_TruEnd[LossRate_Real<0 & LossRate_Real> -50, LossRate_Real], breaks="FD")

# - Distributional analysis on >100% loss rates
describe(datCredit_TruEnd[LossRate_Real>1, LossRate_Real])
### SAFE: no such cases

# - cures with non-zero loss rates?
datCredit_TruEnd[DefSpellResol_Type_Hist == "Cured", .N] / datCredit_TruEnd[,.N] # 76% cures
datCredit_TruEnd[DefSpellResol_Type_Hist == "Cured" & LossRate_Real != 0, .N] / datCredit_TruEnd[,.N]
### SAFE







# - In case these two fields are necessary
lookup[, ReceiptPV := sum(Receipt_Inf * (1+InterestRate_Nom/12)^(-1*TimeInDefSpell)), by=list(LoanID)]
lookup[DefSpell_Age > 1, LossRate_Real2 := (Balance[1] - ReceiptPV[1]) / Balance[1], by=list(LoanID)]

### SCRATCH-END





# - assign explicit resolution outcomes
datCredit_TruEnd[, Event := ifelse(DefSpellResol_Type_Hist == "Cured", "Cure", "Write-off")]

# - scenario for facetting
datCredit_TruEnd[, Scenario := "Treated with TruEnd-procedure"]

# - subset for miniplot
datCredit_TruEnd_W <- subset(datCredit_TruEnd, Event == "Write-off")

# - graphing parameters
col.v <- brewer.pal(10, "Paired")[c(8,6)]

# - Aesthetic engineering: Statistical Summaries
meanLoss_TruEnd <- mean(datCredit_TruEnd$LossRate_Real, na.rm=T)
MeanLoss_TruEnd_W <- mean(datCredit_TruEnd_W$LossRate_Real, na.rm=T)
mix_WC_TruEnd <- datCredit_TruEnd_W[, .N] / datCredit_TruEnd[, .N]

# - main graphs a) Overall LGD distribution
(g1 <- ggplot(datCredit_TruEnd, aes(x=LossRate_Real)) + theme_bw() +
    geom_histogram(aes(y=after_stat(density)), alpha=0.4, bins=round(2*datCredit_TruEnd[,.N]^(1/3)),
                   position="identity", fill=col.v[1], colour=col.v[1]) + 
    geom_density(linewidth=1, colour=col.v[1], linetype="dotted") + 
    geom_vline(xintercept=meanLoss_TruEnd, linewidth=0.6, colour=col.v[1], linetype="dashed") + 
    annotate(geom="text", x=meanLoss_TruEnd*0.8, y=20, family=chosenFont,
             label = paste0("Mean Loss: ", sprintf("%.1f", meanLoss_TruEnd*100), "%"), size=3, colour=col.v[1], angle=90) +     
    # facets & scale options
    labs(x=bquote({Realised~loss~rate~italic(L)}), 
         y="Density of resolved defaults [cures/write-offs]") + 
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
                   position="identity", fill=col.v[2], colour=col.v[2]) + 
    geom_density(linewidth=1, colour=col.v[2], linetype="dotted") + 
    geom_vline(xintercept=MeanLoss_TruEnd_W, linewidth=0.6, colour=col.v[2], linetype="dashed") + 
    annotate(geom="text", x=MeanLoss_TruEnd_W*0.93,  y=3, family=chosenFont,
             label = paste0("Mean Loss: ", sprintf("%.1f", MeanLoss_TruEnd_W*100), "%"), size=3, colour=col.v[2], angle=90) +     
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
ggsave(plot.full, file=paste0(genFigPath,"/LGD-Density_ResolvedDefaults_TruEnd.png"),width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")




# --- 2. Untreated dataset

# - assign explicit resolution outcomes
datCredit_NoTruEnd[, Event := ifelse(DefSpellResol_Type_Hist == "Cured", "Cure", "Write-off")]

# - scenario for facetting
datCredit_NoTruEnd[, Scenario := "Untreated with TruEnd-procedure"]

# - subset for miniplot
datCredit_NoTruEnd_W <- subset(datCredit_NoTruEnd, Event == "Write-off")

# - graphing parameters
col.v <- brewer.pal(10, "Paired")[c(8,6)]

# - Aesthetic engineering: Statistical Summaries
meanLoss_NoTruEnd <- mean(datCredit_NoTruEnd$LossRate_Real, na.rm=T)
MeanLoss_NoTruEnd_W <- mean(datCredit_NoTruEnd_W$LossRate_Real, na.rm=T)
mix_WC_NoTruEnd <- datCredit_NoTruEnd_W[, .N] / datCredit_NoTruEnd[, .N]

# - main graphs a) Overall LGD distribution
(g1 <- ggplot(datCredit_NoTruEnd, aes(x=LossRate_Real)) + theme_bw() +
    geom_histogram(aes(y=after_stat(density)), alpha=0.4, bins=round(2*datCredit_NoTruEnd[,.N]^(1/3)),
                   position="identity", fill=col.v[1], colour=col.v[1]) + 
    geom_density(linewidth=1, colour=col.v[1], linetype="dotted") + 
    geom_vline(xintercept=meanLoss_NoTruEnd, linewidth=0.6, colour=col.v[1], linetype="dashed") + 
    annotate(geom="text", x=meanLoss_NoTruEnd*0.8, y=20, family=chosenFont,
             label = paste0("Mean Loss: ", sprintf("%.1f", meanLoss_NoTruEnd*100), "%"), size=3, colour=col.v[1], angle=90) +     
    # facets & scale options
    labs(x=bquote({Realised~loss~rate~italic(L)}), 
         y="Density of resolved defaults [cures/write-offs]") + 
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
                   position="identity", fill=col.v[2], colour=col.v[2]) + 
    geom_density(linewidth=1, colour=col.v[2], linetype="dotted") + 
    geom_vline(xintercept=MeanLoss_NoTruEnd_W, linewidth=0.6, colour=col.v[2], linetype="dashed") + 
    annotate(geom="text", x=MeanLoss_NoTruEnd_W*0.93,  y=3, family=chosenFont,
             label = paste0("Mean Loss: ", sprintf("%.1f", MeanLoss_NoTruEnd_W*100), "%"), size=3, colour=col.v[2], angle=90) +     
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
ggsave(plot.full, file=paste0(genFigPath,"/LGD-Density_ResolvedDefaults_NoTruEnd.png"),width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")



# --- 3. Combined graph: Treated + Untreated dataset

# - combine respective datasets
datPlot <- rbind(data.table(datCredit_TruEnd_W, Dataset="a_TruEnd"),
  data.table(datCredit_NoTruEnd_W, Dataset="b_NoTruEnd"))

# - Aesthetic engineering: Statistical Summaries
datAnnotate <- data.table(MeanLoss=c(MeanLoss_TruEnd_W,MeanLoss_NoTruEnd_W), Dataset=c("a_TruEnd","b_NoTruEnd"), 
                          Label=c(paste0("Mean Loss: ", sprintf("%.1f", MeanLoss_TruEnd_W*100), "%; TruEnd"),
                                  paste0("Mean Loss: ", sprintf("%.1f", MeanLoss_NoTruEnd_W*100), "%; No TruEnd")),
                          Label_x=c(MeanLoss_TruEnd_W*0.9,MeanLoss_NoTruEnd_W*1.1), Label_y=c(2,2))

# - graphing parameters
col.v <- brewer.pal(8, "Dark2")[c(1,2)]
labels.v <- c("a_TruEnd"="TruEnd", "b_NoTruEnd"="No TruEnd")

# - main graphs a) Overall LGD distribution
(g1 <- ggplot(datPlot, aes(x=LossRate_Real, group=Dataset)) + theme_bw() +
    geom_histogram(aes(y=after_stat(density), colour=Dataset, fill=Dataset), alpha=0.4, #bins=round(2*datCredit_NoTruEnd_W[,.N]^(1/3)), 
                   position="identity") + 
    geom_density(aes(colour=Dataset, linetype=Dataset), linewidth=1) + 
    geom_vline(data=datAnnotate, aes(xintercept=MeanLoss, colour=Dataset, linetype=Dataset), linewidth=0.6) + 
    geom_text(data=datAnnotate, aes(x=Label_x, y=Label_y, label=Label, colour=Dataset), angle=90, size=3) + 
    # facets & scale options
    labs(x=bquote({Realised~loss~rate~italic(L)}), 
         y="Density of resolved defaults [write-offs]") + 
    theme(text=element_text(family=chosenFont),legend.position="bottom") + 
    scale_color_manual(name="Datatset", labels=labels.v, values=col.v) + 
    scale_fill_manual(name="Datatset", labels=labels.v, values=col.v) + 
    scale_linetype_discrete(name="Datatset", labels=labels.v) + 
    scale_x_continuous(breaks=pretty_breaks(), label=percent)
)

# - save plot
dpi <- 180
ggsave(g1, file=paste0(genFigPath,"/LGD-Densities_WriteOffs.png"),width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")





# ------ 2. Analysis: Account-level summaries: TruEnd vs NoTruEnd


### SCRATCH for planning analytics
# - General
describe(datCreditAggr_TruEnd)
describe(datCreditAggr_TruEnd$LoanAge); hist(datCreditAggr_TruEnd$LoanAge, breaks="FD") # Mean: 99.16
describe(datCreditAggr_TruEnd$DefSpells_Num); hist(datCreditAggr_TruEnd$DefSpells_Num, breaks="FD")
describe(datCreditAggr_TruEnd$Event_Type)
describe(datCreditAggr_TruEnd[DefSpells_Num>0 & Event_Type != "ACTIVE", WOff]) # Should represent ~ 19% write-off resolution of defaults
describe(datCreditAggr_TruEnd[Event_Type == "WOFF", LossRate]) # Should represent ~ 19% write-off resolution of defaults
### AB: re-perform this and build in 1-2 sanity checks to ensure aggregation is correct at top of script

# - Loan Age
describe(datCreditAggr_TruEnd[DefSpells_Num>0, LoanAge]) # Mean loan age (defaults): 165.5
describe(datCreditAggr_TruEnd[DefSpells_Num>0 & WOff==1, LoanAge]) # Mean loan age (write-offs): 92
hist(datCreditAggr_TruEnd[DefSpells_Num>0 & WOff==1, LoanAge], breaks="FD")


describe(datCreditAggr_NoTruEnd$LoanAge); hist(datCreditAggr_NoTruEnd$LoanAge, breaks="FD")

# --- 1. <Specific analytics>

### AB: Flesh out necessary graphs



# - Cleanup
rm(datCredit_TruEnd_W, datCredit_NoTruEnd_W, g1, g2, plot.full, datPlot)