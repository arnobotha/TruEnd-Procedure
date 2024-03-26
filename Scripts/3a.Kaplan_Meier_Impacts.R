# =================================== KAPLAN-MEIER =====================================
# Prepare Kaplan-Meier plots to examine the impact when the TruEnd procedure is applied 
# and compare it to when the TruEnd procedure is not applied.
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: TruEnd-procedure
# SCRIPT AUTHOR(S): Roelinde Bester, Dr Arno Botha
# ---------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R
#   - 2a.Data_Prepare_Credit_Basic.R
#   - 2b.Date_Prepare_Credit_Advanced.R
#   - 2c(i).Data_Prepare_Credit_TruEnd.R
#   - 2d(i).Data_Enrich_TruEnd.R
#   - 2e(i).Data_Exclusions_NoTruEnd.R
#s
# -- Inputs:
#   - datCredit_real | Prepared credit data from script 2e(i)
#   - various parameters set in the setup script 0
#
# -- Outputs:

# ---------------------------------------------------------------------------------------


# ------ 0. Preliminaries
ptm <- proc.time() # for runtime calculations (ignore)



# ------ 1. Graphing setup
windowsFonts("Cambria" = windowsFont("Cambria"))
chosenFont <- "Cambria"
trainGraphs <- "training_slc" # folder location
portName <- "FNB SLC"


# - graphing parameters
col.v <- brewer.pal(10, "Paired")[c(10)]




# ------ 2. Get the relevant data for testing TruEnd and NoTruEnd and sample accordingly
# --- 2.1 TruEnd 

# Load data
# - Confirm prepared credit data is loaded into memory
if (!exists('datCredit_real')) unpack.ffdf(paste0(genPath,"creditdata_final4-TruEnd"), tempPath)

# --- Sample accordingly | First record per default spell
datSurv_TruEnd <- subset(datCredit_real, !is.na(DefSpell_Num) & DefSpell_Counter==1, #& Partition == "Observed", 
                  select=c("LoanID", "DefSpell_Key", "DefSpell_Num", "TimeInDefSpell","DefSpell_Age", 
                           "DefSpellResol_Type_Hist")); gc()
rm(datCredit_real); gc()


# --- 2.2 NoTruEnd

# Load data
# - Confirm prepared credit data is loaded into memory
if (!exists('datCredit_real')) unpack.ffdf(paste0(genPath,"creditdata_final4-NoTruEnd"), tempPath)

# --- Sample accordingly | First record per spell
datSurv_NoTruEnd <- subset(datCredit_real, !is.na(DefSpell_Num) & DefSpell_Counter==1, #& Partition == "Observed", 
                           select=c("LoanID", "DefSpell_Key", "DefSpell_Num", "TimeInDefSpell","DefSpell_Age", 
                                    "DefSpellResol_Type_Hist")); gc()
rm(datCredit_real); gc()





# ------ 3. Graphing default quantities with WOFF being the event 

# --- 3.1 TruEnd 

# --- Compute Kaplan-Meier survival estimates (product-limit) for WOFF-event | Spell-level with right-censoring & left-truncation
# Left-truncated spells will have TimeInDefSpell > 1 while the rest will have TimeInDefSpell = 1, both of
#   which are adjusted during modelling by subtracting 1 (since stop must be > start for KM-estimation)
#   Note: At TimeInDefSpell = 1, already 1 month has lapsed, therefore, the true "start" of a normal account is actually 
#     at TimeInDefSpell = 0. Therefore, subtracting one is defensible.
# All competing events preclude the main event from happening and are therefore considered as censored
# ID is set as performing spell key, with no stratification

kmDef_woff_real_spell1c_TruEnd <- survfit(Surv(time=TimeInDefSpell-1, time2=DefSpell_Age, event=DefSpellResol_Type_Hist=="WOFF",
                                            type="counting") ~ 1, id=DefSpell_Key, data=datSurv_TruEnd)
summary(kmDef_woff_real_spell1c_TruEnd)$table
surv_summary(kmDef_woff_real_spell1c_TruEnd)

# --- Discrete baseline hazard function: h(t)
# Create plotting data object
haz_dat_TruEnd <- data.table(Time=kmDef_woff_real_spell1c_TruEnd$time, AtRisk_n=kmDef_woff_real_spell1c_TruEnd$n.risk, 
                             Event_n = kmDef_woff_real_spell1c_TruEnd$n.event, Censored_n=kmDef_woff_real_spell1c_TruEnd$n.censor,
                             hazard=kmDef_woff_real_spell1c_TruEnd$n.event/kmDef_woff_real_spell1c_TruEnd$n.risk, 
                             CumulHazard = kmDef_woff_real_spell1c_TruEnd$cumhaz, Group="1",
                             Surv_KM = kmDef_woff_real_spell1c_TruEnd$surv) %>% 
  filter(Event_n > 0 | Censored_n >0) %>%
  # Discrete-time variants
  mutate(CumulHazard_Disc = -cumsum(log(1-hazard)), Surv_KM_Disc = cumprod(1-hazard)) %>% 
  mutate(Event_KM_Disc = 1-Surv_KM_Disc) %>% as.data.table()
haz_dat_TruEnd[, Surv_KM_Disc_prev:= shift(Surv_KM_Disc, n=1, type="lag"), by=list(Group)]
haz_dat_TruEnd[Time==Time[1], hazard2 := 1- Surv_KM_Disc]
haz_dat_TruEnd[Time>Time[1], hazard2 := 1 - Surv_KM_Disc/Surv_KM_Disc_prev]
all.equal(haz_dat_TruEnd$hazard, haz_dat_TruEnd$hazard2) # Should be TRUE
all.equal(haz_dat_TruEnd$Surv_KM, haz_dat_TruEnd$Surv_KM_Disc) # Should be TRUE
all.equal(haz_dat_TruEnd$CumulHazard, haz_dat_TruEnd$CumulHazard_Disc)

# --- 3.2 NoTruEnd 

# --- Compute Kaplan-Meier survival estimates (product-limit) for WOFF-event | Spell-level with right-censoring & left-truncation
# Left-truncated spells will have TimeInDefSpell > 1 while the rest will have TimeInDefSpell = 1, both of
#   which are adjusted during modelling by subtracting 1 (since stop must be > start for KM-estimation)
#   Note: At TimeInDefSpell = 1, already 1 month has lapsed, therefore, the true "start" of a normal account is actually 
#     at TimeInDefSpell = 0. Therefore, subtracting one is defensible.
# All competing events preclude the main event from happening and are therefore considered as censored
# ID is set as performing spell key, with no stratification

kmDef_woff_real_spell1c_NoTruEnd <- survfit(Surv(time=TimeInDefSpell-1, time2=DefSpell_Age, event=DefSpellResol_Type_Hist=="WOFF",
                                                 type="counting") ~ 1, id=DefSpell_Key, data=datSurv_NoTruEnd)
summary(kmDef_woff_real_spell1c_NoTruEnd)$table
surv_summary(kmDef_woff_real_spell1c_NoTruEnd)


# --- Discrete baseline hazard function: h(t)
# Create plotting data object
haz_dat_NoTruEnd <- data.table(Time=kmDef_woff_real_spell1c_NoTruEnd$time, AtRisk_n=kmDef_woff_real_spell1c_NoTruEnd$n.risk, 
                               Event_n = kmDef_woff_real_spell1c_NoTruEnd$n.event, Censored_n=kmDef_woff_real_spell1c_NoTruEnd$n.censor,
                               hazard=kmDef_woff_real_spell1c_NoTruEnd$n.event/kmDef_woff_real_spell1c_NoTruEnd$n.risk, 
                               CumulHazard = kmDef_woff_real_spell1c_NoTruEnd$cumhaz, Group="1",
                               Surv_KM = kmDef_woff_real_spell1c_NoTruEnd$surv) %>% 
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

# --- 3.3 Combining the datasets with and without the TruEnd into a single dataset
Combined <- rbind(data.table(haz_dat_TruEnd, Dataset = "a_TruEnd"), data.table(haz_dat_NoTruEnd, Dataset = "b_NoTruEnd"))



# ------ 4. Creating graphs

# --- 4.1 Cumulative incidence function => F(t) = 1-S(t) = 1-y = 1-KM(t)

# Aggregate to period-level (reporting date)
plot.sample1 <- rbind(Combined[Dataset == "a_TruEnd",list(Time,Dataset,Value = 1-Surv_KM)],
                      Combined[Dataset == "b_NoTruEnd",list(Time,Dataset,Value = 1-Surv_KM)])

# --- 4.1.1 TIME 1: between 0 and 60 months
Combined_Ft_p1_1 <- plot.sample1[Time<=60,]

# - Calculate aggregated MAEs
AEs1_1 <- Combined_Ft_p1_1[,list(AE = abs(diff(Value))), by=list(Time)]
(MAE1_1 <- mean(AEs1_1$AE, na.rm=T))

# - Graphing parameters
chosenFont <- "Cambria"
col.v <- brewer.pal(n=3, name = "Set2")[c(1,2,3)]
label.v <- c("a_TruEnd"=bquote(italic(A)[t]*": TruEnd "),
             "b_NoTruEnd"=bquote(italic(B)[t]*": No TruEnd "))
linetype.v <- c("solid", "dashed")
aggrSeries1_1 <- max(Combined_Ft_p1_1$Value, na.rm=T)


# - Cumulative incidence function => F(t) = 1-S(t) = 1-y = 1-KM(t)
(gsurv1c_Combined_Ft_1_WOFF <- ggplot(Combined_Ft_p1_1, aes(x=Time, y=Value)) + geom_line(aes(colour=factor(Dataset), linetype=factor(Dataset))) +
    theme_bw() + 
    labs(x=bquote("Default spell age (months) "*italic(t)), y=bquote("Cumulative incidence funciton [WOFF] "*italic(F(t)))) + 
    annotate("text", x=12, y=aggrSeries1_1*0.9, size=3, family=chosenFont,
             label=paste0("'MAE between '*italic(A[t])*' and '*italic(B[t])*': ", sprintf("%.3f", MAE1_1*100), "%'"), parse=T) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom", axis.text.x=element_text(angle=90)) + 
    scale_color_brewer(palette="Dark2", name="Dataset", labels=label.v) + 
    scale_fill_brewer(palette="Dark2", name="Dataset", labels=label.v)+
    scale_linetype_manual(name="Dataset", values=linetype.v, labels=label.v) +
    scale_y_continuous(labels = percent))

  
dpi <- 220
ggsave(print(gsurv1c_Combined_Ft_1_WOFF,newpage=F), file=paste0(genFigPath,"Ft_1_MAE_WOFF.png"),width=1200/dpi, height=1000/dpi,dpi=dpi)


# --- 4.1.2 TIME 2: between 60 and 120 months
Combined_Ft_p1_2 <- plot.sample1[Time >= 61 & Time <= 120,]

# - Calculate aggregated MAEs
AEs1_2 <- Combined_Ft_p1_2[,list(AE = abs(diff(Value))), by=list(Time)]
(MAE1_2 <- mean(AEs1_2$AE, na.rm=T))

# - Graphing parameters
chosenFont <- "Cambria"
col.v <- brewer.pal(n=3, name = "Set2")[c(1,2,3)]
label.v <- c("a_TruEnd"=bquote(italic(A)[t]*": TruEnd "),
             "b_NoTruEnd"=bquote(italic(B)[t]*": No TruEnd "))
linetype.v <- c("solid", "dashed")
aggrSeries1_2 <- max(Combined_Ft_p1_2$Value, na.rm=T)


# - Cumulative incidence function => F(t) = 1-S(t) = 1-y = 1-KM(t)
(gsurv1c_Combined_Ft_2_WOFF <- ggplot(Combined_Ft_p1_2, aes(x=Time, y=Value)) + geom_line(aes(colour=factor(Dataset), linetype=factor(Dataset))) +
    theme_bw() + 
    labs(x=bquote("Default spell age (months) "*italic(t)),y=bquote("Cumulative incidence funciton [WOFF] "*italic(F(t)))) + 
    annotate("text", x=72, y=aggrSeries1_2*0.98, size=3, family=chosenFont,
             label=paste0("'MAE between '*italic(A[t])*' and '*italic(B[t])*': ", sprintf("%.3f", MAE1_2*100), "%'"), parse=T) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom", axis.text.x=element_text(angle=90)) + 
    scale_color_brewer(palette="Dark2", name="Dataset", labels=label.v) + 
    scale_fill_brewer(palette="Dark2", name="Dataset", labels=label.v)+
    scale_linetype_manual(name="Dataset", values=linetype.v, labels=label.v) +
    scale_y_continuous(labels = percent))


dpi <- 220
ggsave(print(gsurv1c_Combined_Ft_2_WOFF,newpage=F), file=paste0(genFigPath,"Ft_2_MAE_WOFF.png"),width=1200/dpi, height=1000/dpi,dpi=dpi)





# --- 4.2 Hazard functions

# Aggregate to period-level (reporting date)
plot.sample2 <- rbind(Combined[Dataset == "a_TruEnd",list(Time,Dataset,Value = hazard)],
                      Combined[Dataset == "b_NoTruEnd",list(Time,Dataset,Value = hazard)])

# --- 4.2.1 TIME 1: between 0 and 60 months
Combined_ht_p2_1 <- plot.sample2[Time<=60,]

# - Calculate aggregated MAEs
AEs2_1 <- Combined_ht_p2_1[,list(AE = abs(diff(Value))), by=list(Time)]
(MAE2_1 <- mean(AEs2_1$AE, na.rm=T))

# - Graphing parameters
chosenFont <- "Cambria"
col.v <- brewer.pal(n=3, name = "Set2")[c(1,2,3)]
label.v <- c("a_TruEnd"=bquote(italic(A)[t]*": TruEnd "),
             "b_NoTruEnd"=bquote(italic(B)[t]*": No TruEnd "))
linetype.v <- c("solid", "dashed")
aggrSeries2_1 <- max(Combined_ht_p2_1$Value, na.rm=T)

#Hazard rate
(gsurv1c_Combined_ht_1_WOFF <- ggplot(Combined_ht_p2_1, aes(x=Time, y=Value)) + geom_line(aes(colour=factor(Dataset), linetype=factor(Dataset))) +
    theme_bw() + 
    labs(x=bquote("Default spell age (months) "*italic(t)), y=bquote("Estimated hazard function [WOFF] "*italic(h(t)))) + 
    annotate("text", x=50, y=aggrSeries2_1*0.1, size=3, family=chosenFont,
             label=paste0("'MAE between '*italic(A[t])*' and '*italic(B[t])*': ", sprintf("%.3f", MAE2_1*100), "%'"), parse=T) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom", axis.text.x=element_text(angle=90)) + 
    scale_color_brewer(palette="Dark2", name="Dataset", labels=label.v) + 
    scale_fill_brewer(palette="Dark2", name="Dataset", labels=label.v)+
    scale_linetype_manual(name="Dataset", values=linetype.v, labels=label.v) +
    scale_y_continuous(labels = percent))


dpi <- 220
ggsave(print(gsurv1c_Combined_ht_1_WOFF,newpage=F), file=paste0(genFigPath,"ht_1_MAE_WOFF.png"),width=1200/dpi, height=1000/dpi,dpi=dpi)



# --- 4.2.2 TIME 2: between 61 and 120 months
Combined_ht_p2_2 <- plot.sample2[Time >= 61 & Time <= 120,]

# - Calculate aggregated MAEs
AEs2_2 <- Combined_ht_p2_2[,list(AE = abs(diff(Value))), by=list(Time)]
(MAE2_2 <- mean(AEs2_2$AE, na.rm=T))

# - Graphing parameters
chosenFont <- "Cambria"
col.v <- brewer.pal(n=3, name = "Set2")[c(1,2,3)]
label.v <- c("a_TruEnd"=bquote(italic(A)[t]*": TruEnd "),
             "b_NoTruEnd"=bquote(italic(B)[t]*": No TruEnd "))
linetype.v <- c("solid", "dashed")
aggrSeries2_1 <- max(Combined_ht_p2_2$Value, na.rm=T)


# - Cumulative incidence function => F(t) = 1-S(t) = 1-y = 1-KM(t)
(gsurv1c_Combined_ht_2_WOFF <- ggplot(Combined_ht_p2_2, aes(x=Time, y=Value)) + geom_line(aes(colour=factor(Dataset), linetype=factor(Dataset))) +
    theme_bw() + 
    labs(x=bquote("Default spell age (months) "*italic(t)), y=bquote("Estimated hazard function [WOFF] "*italic(h(t)))) +  
    annotate("text", x=110, y=aggrSeries2_1*0.96, size=3, family=chosenFont,
             label=paste0("'MAE between '*italic(A[t])*' and '*italic(B[t])*': ", sprintf("%.3f", MAE2_2*100), "%'"), parse=T) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom", axis.text.x=element_text(angle=90)) + 
    scale_color_brewer(palette="Dark2", name="Dataset", labels=label.v) + 
    scale_fill_brewer(palette="Dark2", name="Dataset", labels=label.v) +
    scale_linetype_manual(name="Dataset", values=linetype.v, labels=label.v) +
    scale_y_continuous(labels = percent))


dpi <- 220
ggsave(print(gsurv1c_Combined_ht_2_WOFF,newpage=F), file=paste0(genFigPath,"ht_2_MAE_WOFF.png"),width=1200/dpi, height=1000/dpi,dpi=dpi)








# ------ 5. Graphing default quantities with Cure being the event 

# --- 5.1 TruEnd 

# --- Compute Kaplan-Meier survival estimates (product-limit) for Cured-event | Spell-level with right-censoring & left-truncation
# Left-truncated spells will have TimeInDefSpell > 1 while the rest will have TimeInDefSpell = 1, both of
#   which are adjusted during modelling by subtracting 1 (since stop must be > start for KM-estimation)
#   Note: At TimeInDefSpell = 1, already 1 month has lapsed, therefore, the true "start" of a normal account is actually 
#     at TimeInDefSpell = 0. Therefore, subtracting one is defensible.
# All competing events preclude the main event from happening and are therefore considered as censored
# ID is set as performing spell key, with no stratification

kmDef_Cured_real_spell1c_TruEnd <- survfit(Surv(time=TimeInDefSpell-1, time2=DefSpell_Age, event=DefSpellResol_Type_Hist=="Cured",
                                               type="counting") ~ 1, id=DefSpell_Key, data=datSurv_TruEnd)
summary(kmDef_Cured_real_spell1c_TruEnd)$table
surv_summary(kmDef_Cured_real_spell1c_TruEnd)

# --- Discrete baseline hazard function: h(t)
# Create plotting data object
haz_dat_TruEnd <- data.table(Time=kmDef_Cured_real_spell1c_TruEnd$time, AtRisk_n=kmDef_Cured_real_spell1c_TruEnd$n.risk, 
                             Event_n = kmDef_Cured_real_spell1c_TruEnd$n.event, Censored_n=kmDef_Cured_real_spell1c_TruEnd$n.censor,
                             hazard=kmDef_Cured_real_spell1c_TruEnd$n.event/kmDef_Cured_real_spell1c_TruEnd$n.risk, 
                             CumulHazard = kmDef_Cured_real_spell1c_TruEnd$cumhaz, Group="1",
                             Surv_KM = kmDef_Cured_real_spell1c_TruEnd$surv) %>% 
  filter(Event_n > 0 | Censored_n >0) %>%
  # Discrete-time variants
  mutate(CumulHazard_Disc = -cumsum(log(1-hazard)), Surv_KM_Disc = cumprod(1-hazard)) %>% 
  mutate(Event_KM_Disc = 1-Surv_KM_Disc) %>% as.data.table()
haz_dat_TruEnd[, Surv_KM_Disc_prev:= shift(Surv_KM_Disc, n=1, type="lag"), by=list(Group)]
haz_dat_TruEnd[Time==Time[1], hazard2 := 1- Surv_KM_Disc]
haz_dat_TruEnd[Time>Time[1], hazard2 := 1 - Surv_KM_Disc/Surv_KM_Disc_prev]
all.equal(haz_dat_TruEnd$hazard, haz_dat_TruEnd$hazard2) # Should be TRUE
all.equal(haz_dat_TruEnd$Surv_KM, haz_dat_TruEnd$Surv_KM_Disc) # Should be TRUE
all.equal(haz_dat_TruEnd$CumulHazard, haz_dat_TruEnd$CumulHazard_Disc)

# --- 5.2 NoTruEnd 

# --- Compute Kaplan-Meier survival estimates (product-limit) for Cured-event | Spell-level with right-censoring & left-truncation
# Left-truncated spells will have TimeInDefSpell > 1 while the rest will have TimeInDefSpell = 1, both of
#   which are adjusted during modelling by subtracting 1 (since stop must be > start for KM-estimation)
#   Note: At TimeInDefSpell = 1, already 1 month has lapsed, therefore, the true "start" of a normal account is actually 
#     at TimeInDefSpell = 0. Therefore, subtracting one is defensible.
# All competing events preclude the main event from happening and are therefore considered as censored
# ID is set as performing spell key, with no stratification

kmDef_Cured_real_spell1c_NoTruEnd <- survfit(Surv(time=TimeInDefSpell-1, time2=DefSpell_Age, event=DefSpellResol_Type_Hist=="Cured",
                                                 type="counting") ~ 1, id=DefSpell_Key, data=datSurv_NoTruEnd)
summary(kmDef_Cured_real_spell1c_NoTruEnd)$table
surv_summary(kmDef_Cured_real_spell1c_NoTruEnd)


# --- Discrete baseline hazard function: h(t)
# Create plotting data object
haz_dat_NoTruEnd <- data.table(Time=kmDef_Cured_real_spell1c_NoTruEnd$time, AtRisk_n=kmDef_Cured_real_spell1c_NoTruEnd$n.risk, 
                               Event_n = kmDef_Cured_real_spell1c_NoTruEnd$n.event, Censored_n=kmDef_Cured_real_spell1c_NoTruEnd$n.censor,
                               hazard=kmDef_Cured_real_spell1c_NoTruEnd$n.event/kmDef_Cured_real_spell1c_NoTruEnd$n.risk, 
                               CumulHazard = kmDef_Cured_real_spell1c_NoTruEnd$cumhaz, Group="1",
                               Surv_KM = kmDef_Cured_real_spell1c_NoTruEnd$surv) %>% 
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

# --- 5.3 Combining the datasets with and without the TruEnd into a single dataset
Combined <- rbind(data.table(haz_dat_TruEnd, Dataset = "a_TruEnd"), data.table(haz_dat_NoTruEnd, Dataset = "b_NoTruEnd"))



# ------ 6. Creating graphs

# --- 6.1 Cumulative incidence function => F(t) = 1-S(t) = 1-y = 1-KM(t)

# Aggregate to period-level (reporting date)
plot.sample1 <- rbind(Combined[Dataset == "a_TruEnd",list(Time,Dataset,Value = 1-Surv_KM)],
                      Combined[Dataset == "b_NoTruEnd",list(Time,Dataset,Value = 1-Surv_KM)])

# --- 6.1.1 TIME 1: between 0 and 60 months
Combined_Ft_p1_1 <- plot.sample1[Time<=60,]

# - Calculate aggregated MAEs
AEs1_1 <- Combined_Ft_p1_1[,list(AE = abs(diff(Value))), by=list(Time)]
(MAE1_1 <- mean(AEs1_1$AE, na.rm=T))

# - Graphing parameters
chosenFont <- "Cambria"
col.v <- brewer.pal(n=3, name = "Set2")[c(1,2,3)]
label.v <- c("a_TruEnd"=bquote(italic(A)[t]*": TruEnd "),
             "b_NoTruEnd"=bquote(italic(B)[t]*": No TruEnd "))
linetype.v <- c("solid", "dashed")
aggrSeries1_1 <- max(Combined_Ft_p1_1$Value, na.rm=T)


# - Cumulative incidence function => F(t) = 1-S(t) = 1-y = 1-KM(t)
(gsurv1c_Combined_Ft_1_Cured <- ggplot(Combined_Ft_p1_1, aes(x=Time, y=Value)) + geom_line(aes(colour=factor(Dataset), linetype=factor(Dataset))) +
    theme_bw() + 
    labs(x=bquote("Default spell age (months) "*italic(t)), y=bquote("Cumulative incidence funciton [Cured] "*italic(F(t)))) + 
    annotate("text", x=15, y=aggrSeries1_1*0.95, size=3, family=chosenFont,
             label=paste0("'MAE between '*italic(A[t])*' and '*italic(B[t])*': ", sprintf("%.3f", MAE1_1*100), "%'"), parse=T) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom", axis.text.x=element_text(angle=90)) + 
    scale_color_brewer(palette="Dark2", name="Dataset", labels=label.v) + 
    scale_fill_brewer(palette="Dark2", name="Dataset", labels=label.v)+
    scale_linetype_manual(name="Dataset", values=linetype.v, labels=label.v) +
    scale_y_continuous(labels = percent))


dpi <- 220
ggsave(print(gsurv1c_Combined_Ft_1_Cured,newpage=F), file=paste0(genFigPath,"Ft_1_MAE_Cured.png"),width=1200/dpi, height=1000/dpi,dpi=dpi)


# --- 6.1.2 TIME 2: between 60 and 120 months
Combined_Ft_p1_2 <- plot.sample1[Time >= 61 & Time <= 120,]

# - Calculate aggregated MAEs
AEs1_2 <- Combined_Ft_p1_2[,list(AE = abs(diff(Value))), by=list(Time)]
(MAE1_2 <- mean(AEs1_2$AE, na.rm=T))

# - Graphing parameters
chosenFont <- "Cambria"
col.v <- brewer.pal(n=3, name = "Set2")[c(1,2,3)]
label.v <- c("a_TruEnd"=bquote(italic(A)[t]*": TruEnd "),
             "b_NoTruEnd"=bquote(italic(B)[t]*": No TruEnd "))
linetype.v <- c("solid", "dashed")
aggrSeries1_2 <- max(Combined_Ft_p1_2$Value, na.rm=T)


# - Cumulative incidence function => F(t) = 1-S(t) = 1-y = 1-KM(t)
(gsurv1c_Combined_Ft_2_Cured <- ggplot(Combined_Ft_p1_2, aes(x=Time, y=Value)) + geom_line(aes(colour=factor(Dataset), linetype=factor(Dataset))) +
    theme_bw() + 
    labs(x=bquote("Default spell age (months) "*italic(t)),y=bquote("Cumulative incidence funciton [Cured] "*italic(F(t)))) + 
    annotate("text", x=75, y=aggrSeries1_2*0.992, size=3, family=chosenFont,
             label=paste0("'MAE between '*italic(A[t])*' and '*italic(B[t])*': ", sprintf("%.3f", MAE1_2*100), "%'"), parse=T) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom", axis.text.x=element_text(angle=90)) + 
    scale_color_brewer(palette="Dark2", name="Dataset", labels=label.v) + 
    scale_fill_brewer(palette="Dark2", name="Dataset", labels=label.v)+
    scale_linetype_manual(name="Dataset", values=linetype.v, labels=label.v) +
    scale_y_continuous(labels = percent))


dpi <- 220
ggsave(print(gsurv1c_Combined_Ft_2_Cured,newpage=F), file=paste0(genFigPath,"Ft_2_MAE_Cured.png"),width=1200/dpi, height=1000/dpi,dpi=dpi)





# --- 6.2 Hazard functions

# Aggregate to period-level (reporting date)
plot.sample2 <- rbind(Combined[Dataset == "a_TruEnd",list(Time,Dataset,Value = hazard)],
                      Combined[Dataset == "b_NoTruEnd",list(Time,Dataset,Value = hazard)])

# --- 6.2.1 TIME 1: between 0 and 60 months
Combined_ht_p2_1 <- plot.sample2[Time<=60,]

# - Calculate aggregated MAEs
AEs2_1 <- Combined_ht_p2_1[,list(AE = abs(diff(Value))), by=list(Time)]
(MAE2_1 <- mean(AEs2_1$AE, na.rm=T))

# - Graphing parameters
chosenFont <- "Cambria"
col.v <- brewer.pal(n=3, name = "Set2")[c(1,2,3)]
label.v <- c("a_TruEnd"=bquote(italic(A)[t]*": TruEnd "),
             "b_NoTruEnd"=bquote(italic(B)[t]*": No TruEnd "))
linetype.v <- c("solid", "dashed")
aggrSeries2_1 <- max(Combined_ht_p2_1$Value, na.rm=T)

#Hazard rate
(gsurv1c_Combined_ht_1_Cured <- ggplot(Combined_ht_p2_1, aes(x=Time, y=Value)) + geom_line(aes(colour=factor(Dataset), linetype=factor(Dataset))) +
    theme_bw() + 
    labs(x=bquote("Default spell age (months) "*italic(t)), y=bquote("Estimated hazard function [Cured] "*italic(h(t)))) + 
    annotate("text", x=45, y=aggrSeries2_1*0.999, size=3, family=chosenFont,
             label=paste0("'MAE between '*italic(A[t])*' and '*italic(B[t])*': ", sprintf("%.3f", MAE2_1*100), "%'"), parse=T) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom", axis.text.x=element_text(angle=90)) + 
    scale_color_brewer(palette="Dark2", name="Dataset", labels=label.v) + 
    scale_fill_brewer(palette="Dark2", name="Dataset", labels=label.v)+
    scale_linetype_manual(name="Dataset", values=linetype.v, labels=label.v) +
    scale_y_continuous(labels = percent))


dpi <- 220
ggsave(print(gsurv1c_Combined_ht_1_Cured,newpage=F), file=paste0(genFigPath,"ht_1_MAE_Cured.png"),width=1200/dpi, height=1000/dpi,dpi=dpi)



# --- 6.2.2 TIME 2: between 61 and 120 months
Combined_ht_p2_2 <- plot.sample2[Time >= 61 & Time <= 120,]

# - Calculate aggregated MAEs
AEs2_2 <- Combined_ht_p2_2[,list(AE = abs(diff(Value))), by=list(Time)]
(MAE2_2 <- mean(AEs2_2$AE, na.rm=T))

# - Graphing parameters
chosenFont <- "Cambria"
col.v <- brewer.pal(n=3, name = "Set2")[c(1,2,3)]
label.v <- c("a_TruEnd"=bquote(italic(A)[t]*": TruEnd "),
             "b_NoTruEnd"=bquote(italic(B)[t]*": No TruEnd "))
linetype.v <- c("solid", "dashed")
aggrSeries2_1 <- max(Combined_ht_p2_2$Value, na.rm=T)


# - Cumulative incidence function => F(t) = 1-S(t) = 1-y = 1-KM(t)
(gsurv1c_Combined_ht_2_Cured <- ggplot(Combined_ht_p2_2, aes(x=Time, y=Value)) + geom_line(aes(colour=factor(Dataset), linetype=factor(Dataset))) +
    theme_bw() + 
    labs(x=bquote("Default spell age (months) "*italic(t)), y=bquote("Estimated hazard function [Cured] "*italic(h(t)))) +  
    annotate("text", x=80, y=aggrSeries2_1*0.96, size=3, family=chosenFont,
             label=paste0("'MAE between '*italic(A[t])*' and '*italic(B[t])*': ", sprintf("%.3f", MAE2_2*100), "%'"), parse=T) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom", axis.text.x=element_text(angle=90)) + 
    scale_color_brewer(palette="Dark2", name="Dataset", labels=label.v) + 
    scale_fill_brewer(palette="Dark2", name="Dataset", labels=label.v) +
    scale_linetype_manual(name="Dataset", values=linetype.v, labels=label.v) +
    scale_y_continuous(labels = percent))


dpi <- 220
ggsave(print(gsurv1c_Combined_ht_2_Cured,newpage=F), file=paste0(genFigPath,"ht_2_MAE_Cured.png"),width=1200/dpi, height=1000/dpi,dpi=dpi)

