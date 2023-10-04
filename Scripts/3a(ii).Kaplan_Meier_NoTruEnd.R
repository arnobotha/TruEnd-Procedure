# =================================== KAPLAN-MEIER =====================================
# Prepare Kaplan-Meier plots to examine the impact when the TruEnd procedure is not applied.
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: TruEnd-procedure
# SCRIPT AUTHOR(S): 
# ---------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R
#   - 2a.Data_Prepare_Credit_Basic.R
#   - 2b.Date_Prepare_Credit_Advanced.R
#   - 2c(i).Data_Prepare_Credit_TruEnd.R
#   - 2d(i).Data_Enrich_TruEnd.R
#s
# -- Inputs:
#   - datCredit_real | Prepared credit data from script 2d
#   - various parameters set in the setup script 0
#
# -- Outputs:

# ---------------------------------------------------------------------------------------


# ------ 0. Preliminaries
ptm <- proc.time() # for runtime calculations (ignore)

# - Confirm prepared credit data is loaded into memory
if (!exists('datCredit_real')) unpack.ffdf(paste0(genPath,"creditdata_final3-NoTruEnd"), tempPath)


# - Create a path to which the graphs must be saved
graphPath_real <- "C:/Users/R5422965/OneDrive - FRG/TruEnd-Procedure/Figures"

# --- 1. Graphing setup
windowsFonts("Cambria" = windowsFont("Cambria"))
chosenFont <- "Cambria"
trainGraphs <- "training_slc" # folder location
portName <- "FNB SLC"


# - graphing parameters
col.v <- brewer.pal(10, "Paired")[c(10)]

# --- 2. Default survival modelling with left-truncated spells | Spell-level | Multiple spells
# Each subject has a single spell-level observation that summarize that spell thus far; each subject may have multiple spells
# Competing risks are latent, i.e., treated as right-censored
# Spell correctly specified with left-truncated start time and 0 for unaffected spells

# --- Sample accordingly | First record per spell
datSurv_NoTruEnd <- subset(datCredit_real, ExclusionID==0 & !is.na(PerfSpell_Num) & PerfSpell_Counter==1, #& Partition == "Observed", 
                  select=c("LoanID", "PerfSpell_Key", "PerfSpell_Num", "TimeInPerfSpell","PerfSpell_Age", 
                           "PerfSpellResol_Type_Hist")); gc()
rm(datCredit_real); gc()

# --- Compute Kaplan-Meier survival estimates (product-limit) for default-event | Spell-level with right-censoring & left-truncation
# Left-truncated spells will have TimeInPerfSpell > 1 while the rest will have TimeInPerfSpell = 1, both of
#   which are adjusted during modelling by subtracting 1 (since stop must be > start for KM-estimation)
#   Note: At TimeInPerfSpell = 1, already 1 month has lapsed, therefore, the true "start" of a normal account is actually 
#     at TimeInPerfSpell = 0. Therefore, subtracting one is defensible.
# All competing events preclude the main event from happening and are therefore considered as censored
# ID is set as performing spell key, with no stratification
kmPerf_default_real_spell1c_NoTruEnd <- survfit(Surv(time=TimeInPerfSpell-1, time2=PerfSpell_Age, event=PerfSpellResol_Type_Hist=="Defaulted",
                                            type="counting") ~ 1, id=PerfSpell_Key, data=datSurv_NoTruEnd)
summary(kmPerf_default_real_spell1c_NoTruEnd)$table
surv_summary(kmPerf_default_real_spell1c_NoTruEnd)

# --- Graphing survival and related quantities
# - Survival probability, S(t)=y
(gsurv1c_NoTruEnd_a <- ggsurvplot(kmPerf_default_real_spell1c_NoTruEnd, fun="pct", conf.int=T, legend="none", 
                         break.time.by=round(max(kmPerf_default_real_spell1c_NoTruEnd$time)/8), palette=col.v,
                         xlab = bquote(Discrete~time~italic(t)*" (months) in performing spell: Multi-spell"),
                         ylab = bquote(Survival~probability~'[Default]'~italic(S(t))*': spell-level (Kaplan-Meier)'),
                         xlim=c(0, max(kmPerf_default_real_spell1c_NoTruEnd$time)+1), surv.median.line = "hv", censor=F, 
                         ggtheme = theme_bw(base_family=chosenFont), tables.theme = theme_cleantable(),
                         tables.height=0.10, tables.y.text=F, tables.y.text.col=T, risk.table = "abs_pct", risk.table.pos = "out",
                         cumevents=T, cumevents.title="Cumulative number of defaults", 
                         cumcensor=T, cumcensor.title="Cumulative number of censored observations (incl. competing risks)",
                         risk.table.title = "Number in (% of) sample at risk of default", font.family=chosenFont, fontsize=2.5))


# - Cumulative event/lifetime probability: ;, , so F(t)=1-S(t)
(gsurv1c_NoTruEnd_b <- ggsurvplot(kmPerf_default_real_spell1c_NoTruEnd, fun="event", conf.int=T, surv.scale = "percent", legend="none", 
                         break.time.by=round(max(kmPerf_default_real_spell1c_NoTruEnd$time)/8), palette=col.v,
                         xlab = bquote(Discrete~time~italic(t)*" (months) in performing spell: Multi-spell"),
                         ylab = bquote(Cumulative~lifetime~distribution~'[Default]'~italic(F(t))*': spell-level (Kaplan-Meier)'),
                         xlim=c(0, max(kmPerf_default_real_spell1c_NoTruEnd$time)+1), censor=F, 
                         ggtheme = theme_bw(base_family=chosenFont), tables.theme = theme_cleantable(),
                         tables.height=0.10, tables.y.text=F, tables.y.text.col=T, risk.table = "abs_pct", risk.table.pos = "out",
                         risk.table.title = "Number in (% of) sample at risk of default", font.family=chosenFont, fontsize=2.5))

# - Cumulative hazard: f(t)=-log(y), so H(t) = -log(S(t)). Discrete-variant: f(t)=-sum{ln(1-h(t))}
(gsurv1c_NoTruEnd_c <- ggsurvplot(kmPerf_default_real_spell1c_NoTruEnd, fun="cumhaz",conf.int=T, legend="none", 
                         break.time.by=round(max(kmPerf_default_real_spell1c_NoTruEnd$time)/8), palette=col.v,
                         xlab = bquote(Discrete~time~italic(t)*" (months) in performing spell: Multi-spell"),
                         ylab = bquote(Cumulative~hazard~distribution~'[Default]'~italic(H(t))*': spell-level (Nelson-Aalen)'),
                         xlim=c(0,  max(kmPerf_default_real_spell1c_NoTruEnd$time)+1), censor=F, 
                         ggtheme = theme_bw(base_family=chosenFont), tables.theme = theme_cleantable(),
                         tables.height=0.10, tables.y.text=F, tables.y.text.col=T, risk.table = "abs_pct", risk.table.pos = "out",
                         risk.table.title = "Number in (% of) sample at risk of default", font.family=chosenFont, fontsize=2.5))

# - Discrete baseline hazard function: h(t)
# create plotting data object
haz_dat_NoTruEnd <- data.table(Time=kmPerf_default_real_spell1c_NoTruEnd$time, AtRisk_n=kmPerf_default_real_spell1c_NoTruEnd$n.risk, 
                      Event_n = kmPerf_default_real_spell1c_NoTruEnd$n.event, Censored_n=kmPerf_default_real_spell1c_NoTruEnd$n.censor,
                      hazard=kmPerf_default_real_spell1c_NoTruEnd$n.event/kmPerf_default_real_spell1c_NoTruEnd$n.risk, 
                      CumulHazard = kmPerf_default_real_spell1c_NoTruEnd$cumhaz, Group="1",
                      Surv_KM = kmPerf_default_real_spell1c_NoTruEnd$surv) %>% 
  filter(Event_n > 0 | Censored_n >0) %>%
  # Discrete-time variants
  mutate(CumulHazard_Disc = -cumsum(log(1-hazard)), Surv_KM_Disc = cumprod(1-hazard)) %>% 
  mutate(Event_KM_Disc = 1-Surv_KM_Disc) %>% as.data.table()
haz_dat_NoTruEnd[, Surv_KM_Disc_prev:= shift(Surv_KM_Disc, n=1, type="lag"), by=list(Group)]
haz_dat_NoTruEnd[Time==Time[1], hazard2 := 1- Surv_KM_Disc]
haz_dat_NoTruEnd[Time>Time[1], hazard2 := 1 - Surv_KM_Disc/Surv_KM_Disc_prev]
all.equal(haz_dat_NoTruEnd$hazard, haz_dat_NoTruEnd$hazard2) # Should be TRUE
all.equal(haz_dat_NoTruEnd$Surv_KM, haz_dat_NoTruEnd$Surv_KM_Disc) # Should be TRUE
all.equal(haz_dat_NoTruEnd$CumulHazard, haz_dat_NoTruEnd$CumulHazard_Disc)

# graphing parameters
col.v <- brewer.pal(10, "Paired")[c(10,9)]
span.s <- 0.1
label.v <- paste0("Loess-smoothed hazard [span: ", span.s, "]")
# graph object for shorter time, informed by previous graphs
(gsurv1c_NoTruEnd_d <- ggplot(haz_dat_NoTruEnd[Time<=300,], aes(x=Time,y=hazard)) + theme_minimal() +
    geom_line(linetype="solid", colour=col.v[1]) + geom_point(colour=col.v[1]) + 
    geom_smooth(aes(colour=Group, fill=Group), se=T, method="loess", span=span.s, alpha=0.25, linetype="dotted") +
    labs(y=bquote(plain(Estimated~hazard*' function [Default]')~italic(h(t))*': spell-level (Kaplan-Meier)'), 
         x=bquote(Discrete~time~italic(t)*" (months) in performing spell: Multi-spell")) + 
    theme(text=element_text(family=chosenFont),legend.position="bottom") + 
    scale_colour_manual(name="", values=col.v[2], labels=label.v) + 
    scale_fill_manual(name="", values=col.v[2], labels=label.v) + 
    scale_y_continuous(breaks=breaks_pretty(), label=percent) + 
    scale_x_continuous(breaks=breaks_pretty(n=8), label=comma))



# - save plots
dpi <- 120 # need to decrease size for risk tables' text
ggsave(print(gsurv1c_NoTruEnd_a,newpage=F), file=paste0(graphPath_real,"/SurvFig1c_NoTruEnd_a-Default_Surv-KaplanMeier-SpellLevel-MultiSpell-LatentComp-InclLeftTrunc_Correct.png"),width=1200/dpi, height=1000/dpi,dpi=dpi)
ggsave(print(gsurv1c_NoTruEnd_b,newpage=F), file=paste0(graphPath_real,"/SurvFig1c_NoTruEnd_b-Default_CumulEvent-KaplanMeier-SpellLevel-MultiSpell-LatentComp-InclLeftTrunc_Correct.png"),width=1200/dpi, height=1000/dpi,dpi=dpi)
ggsave(print(gsurv1c_NoTruEnd_c,newpage=F), file=paste0(graphPath_real,"/SurvFig1c_NoTruEnd_c-Default_CumulHaz-NelsonAalen-SpellLevel-MultiSpell-LatentComp-InclLeftTrunc_Correct.png"),width=1200/dpi, height=1000/dpi,dpi=dpi)
dpi <- 140 # reset
ggsave(gsurv1c_NoTruEnd_d, file=paste0(graphPath_real,"/SurvFig1c_NoTruEnd_d-Default_Hazard-KaplanMeier-SpellLevel-MultiSpell-LatentComp-InclLeftTrunc_Correct.png"),width=1200/dpi, height=1000/dpi,dpi=dpi)

# - cleanup
#rm(datSurv_NoTruEnd); gc()


#---------------------------------------------------------------------------------------------------------

# -- Combination of the KM impact
# - Combining the datasets with and without the TruEnd into a single dataset
haz_dat_TruEnd <- haz_dat_TruEnd  %>% mutate(Dataset = rep("TruEnd", nrow(haz_dat_TruEnd)))
haz_dat_NoTruEnd <- haz_dat_NoTruEnd  %>% mutate(Dataset = rep("NoTruEnd", nrow(haz_dat_NoTruEnd)))

Combined_imp <- rbind(haz_dat_TruEnd,haz_dat_NoTruEnd)

# - Graphing parameters
chosenFont <- "Cambria"
label.v <- c("TruEnd"=expression("TruEnd"),
             "NoTruEnd"=expression("NoTruEnd"))

# - Survival function => S(t)=y
(gsurv1c_Combined_a <- ggplot(Combined_imp, aes(x=Time, y=Surv_KM)) + geom_line(aes(colour=factor(Dataset))) +
    theme_bw() + 
    labs(x="Performance spell age (months)", y="Survival probability [Different Resolution Types] ~ KM estimates") + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom", axis.text.x=element_text(angle=90)) + 
    scale_color_brewer(palette="Dark2", name="Dataset", labels=label.v) + 
    scale_fill_brewer(palette="Dark2", name="Dataset", labels=label.v))

# - Cumulative incidence function => F(t) = 1-S(t) = 1-y = 1-KM(t)
(gsurv1c_Combined_b <- ggplot(Combined_imp, aes(x=Time, y=1-Surv_KM)) + geom_line(aes(colour=factor(Dataset))) +
    theme_bw() + 
    labs(x="Performance spell age (months)", y="Cumulative incidenc funciton [Different Resolution Types] ~ KM estimates") + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom", axis.text.x=element_text(angle=90)) + 
    scale_color_brewer(palette="Dark2", name="Dataset", labels=label.v) + 
    scale_fill_brewer(palette="Dark2", name="Dataset", labels=label.v))

# - Cumulative hazard function => f(y)=-log(y)
(gsurv1c_Combined_c <- ggplot(Combined_imp, aes(x=Time, y=CumulHazard)) + geom_line(aes(colour=factor(Dataset))) +
    theme_bw() + 
    labs(x="Performance spell age (months)", y="Cumulative hazard [Different Resolution Types] ~ KM estimates") + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom", axis.text.x=element_text(angle=90)) + 
    scale_color_brewer(palette="Dark2", name="Dataset", labels=label.v) + 
    scale_fill_brewer(palette="Dark2", name="Dataset", labels=label.v))

# - Hazard functions
(gsurv1c_Combined_d <- ggplot(Combined_imp[Time<=300,], aes(x=Time, y=hazard)) + geom_line(aes(colour=factor(Dataset))) +
    theme_bw() + 
    labs(x="Performance spell age (months)", y="Hazard [Different Resolution Types] ~ KM estimates") + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom", axis.text.x=element_text(angle=90)) + 
    scale_color_brewer(palette="Dark2", name="Dataset", labels=label.v) + 
    scale_fill_brewer(palette="Dark2", name="Dataset", labels=label.v))