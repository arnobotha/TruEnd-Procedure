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


# ------ 1. Preliminaries

# - graphing parameters
chosenFont <- "Cambria"
vCol <- brewer.pal(10, "Paired")[c(10)]




# ------ 2. Get the relevant data for testing TruEnd and NoTruEnd

# --- 2.1 TruENd 

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




# ------ 3. Calculate Kaplan-Meier estimator of survival probability
 
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




# ------ 4. Data preparation towards graphing survival quantities

# --- 4.1 Data preparation: TruEnd
# Includes alternative calculation methods by which the discrete baseline hazard h(t) is calculated
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


# --- 4.2 Data preparation: NoTruEnd
# Includes alternative calculation methods by which the discrete baseline hazard h(t) is calculated
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


# --- 4.3 Data fusion

# - Combining the datasets with and without the TruEnd into a single dataset
Combined <- rbind(data.table(haz_dat_TruEnd, Dataset = "a_TruEnd"), data.table(haz_dat_NoTruEnd, Dataset = "b_NoTruEnd"))

# Aggregate to period-level (reporting date) | Survival probability
plot.sample1 <- rbind(Combined[Dataset == "a_TruEnd",list(Time,Dataset,Value = 1-Surv_KM)],
                      Combined[Dataset == "b_NoTruEnd",list(Time,Dataset,Value = 1-Surv_KM)])

# Aggregate to period-level (reporting date) | hazard rate
plot.sample2 <- rbind(Combined[Dataset == "a_TruEnd",list(Time,Dataset,Value = hazard)],
                      Combined[Dataset == "b_NoTruEnd",list(Time,Dataset,Value = hazard)])





# ------ 5. Graph: Cumulative incidence function => F(t) = 1-S(t) = 1-y = 1-KM(t)

# --- 5.1 Time span [0,60] months
Combined_Ft_p1_1 <- plot.sample1[Time<=60,]

# - Calculate aggregated MAEs
AEs1_1 <- Combined_Ft_p1_1[,list(AE = abs(diff(Value))), by=list(Time)]
(MAE1_1 <- mean(AEs1_1$AE, na.rm=T))

# - Graphing parameters
chosenFont <- "Cambria"
vCol <- brewer.pal(n=3, name = "Set2")[c(1,2,3)]
vLabel <- c("a_TruEnd"=bquote(italic(A)[t]*": TruEnd "),
             "b_NoTruEnd"=bquote(italic(B)[t]*": No TruEnd "))
linetype.v <- c("solid", "dashed")
aggrSeries1_1 <- max(Combined_Ft_p1_1$Value, na.rm=T)

# - Cumulative incidence function => F(t) = 1-S(t) = 1-y = 1-KM(t)
(gsurv1c_Combined_Ft_1 <- ggplot(Combined_Ft_p1_1, aes(x=Time, y=Value)) + geom_line(aes(colour=factor(Dataset), linetype=factor(Dataset))) +
    theme_bw() + 
    labs(x=bquote("Default spell age (months) "*italic(t)), y=bquote("Cumulative incidence funciton [WOFF] "*italic(F(t)))) + 
    annotate("text", x=15, y=aggrSeries1_1*0.9, size=3, family=chosenFont,
             label=paste0("'MAE between '*italic(A[t])*' and '*italic(B[t])*': ", sprintf("%.3f", MAE1_1*100), "%'"), parse=T) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom") + 
    scale_color_brewer(palette="Dark2", name="Dataset", labels=vLabel) + 
    scale_fill_brewer(palette="Dark2", name="Dataset", labels=vLabel)+
    scale_linetype_manual(name="Dataset", values=linetype.v, labels=vLabel) +
    scale_y_continuous(labels = percent))
  
dpi <- 260
ggsave(print(gsurv1c_Combined_Ft_1,newpage=F), file=paste0(genFigPath,"Ft_1_MAE.png"),width=1200/dpi, height=1000/dpi,dpi=dpi)



# --- 5.2 Time span (60,120] months
Combined_Ft_p1_2 <- plot.sample1[Time >= 61 & Time <= 120,]

# - Calculate aggregated MAEs
AEs1_2 <- Combined_Ft_p1_2[,list(AE = abs(diff(Value))), by=list(Time)]
(MAE1_2 <- mean(AEs1_2$AE, na.rm=T))

# - Graphing parameters
chosenFont <- "Cambria"
vCol <- brewer.pal(n=3, name = "Set2")[c(1,2,3)]
vLabel <- c("a_TruEnd"=bquote(italic(A)[t]*": TruEnd "),
             "b_NoTruEnd"=bquote(italic(B)[t]*": No TruEnd "))
linetype.v <- c("solid", "dashed")
aggrSeries1_2 <- max(Combined_Ft_p1_2$Value, na.rm=T)

# - Cumulative incidence function => F(t) = 1-S(t) = 1-y = 1-KM(t)
(gsurv1c_Combined_Ft_2 <- ggplot(Combined_Ft_p1_2, aes(x=Time, y=Value)) + geom_line(aes(colour=factor(Dataset), linetype=factor(Dataset))) +
    theme_bw() + 
    labs(x=bquote("Default spell age (months) "*italic(t)),y=bquote("Cumulative incidence funciton [WOFF] "*italic(F(t)))) + 
    annotate("text", x=75, y=aggrSeries1_2*0.98, size=3, family=chosenFont,
             label=paste0("'MAE between '*italic(A[t])*' and '*italic(B[t])*': ", sprintf("%.3f", MAE1_2*100), "%'"), parse=T) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom") + 
    scale_color_brewer(palette="Dark2", name="Dataset", labels=vLabel) + 
    scale_fill_brewer(palette="Dark2", name="Dataset", labels=vLabel)+
    scale_linetype_manual(name="Dataset", values=linetype.v, labels=vLabel) +
    scale_y_continuous(labels = percent))

dpi <- 260
ggsave(print(gsurv1c_Combined_Ft_2,newpage=F), file=paste0(genFigPath,"Ft_2_MAE.png"),width=1200/dpi, height=1000/dpi,dpi=dpi)



# --- 5.3 Time span (121,240] months
Combined_Ft_p1_3 <- plot.sample1[Time >= 121 & Time <= 240,]

# - Calculate aggregated MAEs
AEs1_3 <- Combined_Ft_p1_3[,list(AE = abs(diff(Value))), by=list(Time)]
(MAE1_3 <- mean(AEs1_3$AE, na.rm=T))

# - Graphing parameters
chosenFont <- "Cambria"
vCol <- brewer.pal(n=3, name = "Set2")[c(1,2,3)]
vLabel <- c("a_TruEnd"=bquote(italic(A)[t]*": TruEnd "),
            "b_NoTruEnd"=bquote(italic(B)[t]*": No TruEnd "))
linetype.v <- c("solid", "dashed")
aggrSeries1_2 <- max(Combined_Ft_p1_3$Value, na.rm=T)

# - Cumulative incidence function => F(t) = 1-S(t) = 1-y = 1-KM(t)
(gsurv1c_Combined_Ft_3 <- ggplot(Combined_Ft_p1_3, aes(x=Time, y=Value)) + geom_line(aes(colour=factor(Dataset), linetype=factor(Dataset))) +
    theme_bw() + 
    labs(x="Default spell age (months)", y=bquote("Cumulative incidence funciton [WOFF] "*italic(F(t)))) + 
    annotate("text", x=150, y=aggrSeries1_2*0.99, size=3, family=chosenFont,
             label=paste0("'MAE between '*italic(A[t])*' and '*italic(B[t])*': ", sprintf("%.3f", MAE1_3*100), "%'"), parse=T) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom") + 
    scale_color_brewer(palette="Dark2", name="Dataset", labels=vLabel) + 
    scale_fill_brewer(palette="Dark2", name="Dataset", labels=vLabel)+
    scale_linetype_manual(name="Dataset", values=linetype.v, labels=vLabel))

dpi <- 260
ggsave(print(gsurv1c_Combined_Ft_3,newpage=F), file=paste0(genFigPath,"Ft_3_MAE.png"),width=1200/dpi, height=1000/dpi,dpi=dpi)





# ------ 6. Graph: Empirical hazard functions


# --- 6.1 Time span [0,60] months
Combined_ht_p2_1 <- plot.sample2[Time<=60,]

# - Calculate aggregated MAEs
AEs2_1 <- Combined_ht_p2_1[,list(AE = abs(diff(Value))), by=list(Time)]
(MAE2_1 <- mean(AEs2_1$AE, na.rm=T))

# - Graphing parameters
chosenFont <- "Cambria"
vCol <- brewer.pal(n=3, name = "Set2")[c(1,2,3)]
vLabel <- c("a_TruEnd"=bquote(italic(A)[t]*": TruEnd "),
             "b_NoTruEnd"=bquote(italic(B)[t]*": No TruEnd "))
linetype.v <- c("solid", "dashed")
aggrSeries2_1 <- max(Combined_ht_p2_1$Value, na.rm=T)

# Hazard rate
(gsurv1c_Combined_ht_1 <- ggplot(Combined_ht_p2_1, aes(x=Time, y=Value)) + geom_line(aes(colour=factor(Dataset), linetype=factor(Dataset))) +
    theme_bw() + 
    labs(x=bquote("Default spell age (months) "*italic(t)), y=bquote("Estimated hazard function [WOFF] "*italic(h(t)))) + 
    annotate("text", x=40, y=aggrSeries2_1*0.1, size=3, family=chosenFont,
             label=paste0("'MAE between '*italic(A[t])*' and '*italic(B[t])*': ", sprintf("%.3f", MAE2_1*100), "%'"), parse=T) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom") + 
    scale_color_brewer(palette="Dark2", name="Dataset", labels=vLabel) + 
    scale_fill_brewer(palette="Dark2", name="Dataset", labels=vLabel)+
    scale_linetype_manual(name="Dataset", values=linetype.v, labels=vLabel) +
    scale_y_continuous(labels = percent))

dpi <- 260
ggsave(print(gsurv1c_Combined_ht_1,newpage=F), file=paste0(genFigPath,"ht_1_MAE.png"),width=1200/dpi, height=1000/dpi,dpi=dpi)



# --- 6.2 Time span (60,120] months
Combined_ht_p2_2 <- plot.sample2[Time >= 61 & Time <= 120,]

# - Calculate aggregated MAEs
AEs2_2 <- Combined_ht_p2_2[,list(AE = abs(diff(Value))), by=list(Time)]
(MAE2_2 <- mean(AEs2_2$AE, na.rm=T))

# - Graphing parameters
chosenFont <- "Cambria"
vCol <- brewer.pal(n=3, name = "Set2")[c(1,2,3)]
vLabel <- c("a_TruEnd"=bquote(italic(A)[t]*": TruEnd "),
             "b_NoTruEnd"=bquote(italic(B)[t]*": No TruEnd "))
linetype.v <- c("solid", "dashed")
aggrSeries2_1 <- max(Combined_ht_p2_2$Value, na.rm=T)

# Hazard rate
(gsurv1c_Combined_ht_2 <- ggplot(Combined_ht_p2_2, aes(x=Time, y=Value)) + geom_line(aes(colour=factor(Dataset), linetype=factor(Dataset))) +
    theme_bw() + 
    labs(x=bquote("Default spell age (months) "*italic(t)), y=bquote("Estimated hazard function [WOFF] "*italic(h(t)))) +  
    annotate("text", x=105, y=aggrSeries2_1*0.96, size=3, family=chosenFont,
             label=paste0("'MAE between '*italic(A[t])*' and '*italic(B[t])*': ", sprintf("%.3f", MAE2_2*100), "%'"), parse=T) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom") + 
    scale_color_brewer(palette="Dark2", name="Dataset", labels=vLabel) + 
    scale_fill_brewer(palette="Dark2", name="Dataset", labels=vLabel) +
    scale_linetype_manual(name="Dataset", values=linetype.v, labels=vLabel) +
    scale_y_continuous(labels = percent))

dpi <- 260
ggsave(print(gsurv1c_Combined_ht_2,newpage=F), file=paste0(genFigPath,"ht_2_MAE.png"),width=1200/dpi, height=1000/dpi,dpi=dpi)



# --- 6.3 Time span (120,240] months
Combined_ht_p2_3 <- plot.sample2[Time >= 121 & Time <= 240,]

# - Calculate aggregated MAEs
AEs2_3 <- Combined_ht_p2_3[,list(AE = abs(diff(Value))), by=list(Time)]
(MAE2_3 <- mean(AEs2_3$AE, na.rm=T))

# - Graphing parameters
chosenFont <- "Cambria"
vCol <- brewer.pal(n=3, name = "Set2")[c(1,2,3)]
vLabel <- c("a_TruEnd"=bquote(italic(A)[t]*": TruEnd "),
             "b_NoTruEnd"=bquote(italic(B)[t]*": No TruEnd "))
linetype.v <- c("solid", "dashed")
aggrSeries2_1 <- max(Combined_ht_p2_3$Value, na.rm=T)

# Hazard rate
(gsurv1c_Combined_ht_3 <- ggplot(Combined_ht_p2_3, aes(x=Time, y=Value)) + geom_line(aes(colour=factor(Dataset), linetype=factor(Dataset))) +
    theme_bw() + 
    labs(x="Default spell age (months)", y=bquote("Estimated hazard function [WOFF] "*italic(h(t)))) + 
    annotate("text", x=150, y=aggrSeries2_1*0.8, size=3, family=chosenFont,
             label=paste0("'MAE between '*italic(A[t])*' and '*italic(B[t])*': ", sprintf("%.3f", MAE2_3*100), "%'"), parse=T) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom") + 
    scale_color_brewer(palette="Dark2", name="Dataset", labels=vLabel) + 
    scale_fill_brewer(palette="Dark2", name="Dataset", labels=vLabel) +
    scale_linetype_manual(name="Dataset", values=linetype.v, labels=vLabel))

dpi <- 260
ggsave(print(gsurv1c_Combined_ht_3,newpage=F), file=paste0(genFigPath,"ht_3_MAE.png"),width=1200/dpi, height=1000/dpi,dpi=dpi)




# ------ 7. General cleanup
rm(AEs1_1, AEs1_2, AEs1_3, AEs2_1, AEs2_2, AEs2_3, Combined, Combined_Ft_p1_1, Combined_Ft_p1_2, Combined_Ft_p1_3,
   Combined_ht_p2_1, Combined_ht_p2_2, Combined_ht_p2_3, datSurv_NoTruEnd, datSurv_TruEnd, 
   gsurv1c_Combined_Ft_1, gsurv1c_Combined_Ft_2, gsurv1c_Combined_Ft_3, gsurv1c_Combined_ht_1, gsurv1c_Combined_ht_2, gsurv1c_Combined_ht_3,
   haz_dat_TruEnd, haz_dat_NoTruEnd, plot.sample1, plot.sample2, vLabel, kmDef_woff_real_spell1c_TruEnd, kmDef_woff_real_spell1c_NoTruEnd)
gc()
