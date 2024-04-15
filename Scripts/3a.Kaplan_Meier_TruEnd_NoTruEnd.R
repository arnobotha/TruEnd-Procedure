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
 
# --- 3.1 TruEnd | Write-off event
# Compute Kaplan-Meier survival estimates (product-limit) for WOFF-event | Spell-level with right-censoring & left-truncation
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


# --- 3.2 NoTruEnd | Write-off event
# Compute Kaplan-Meier survival estimates (product-limit) for WOFF-event | Spell-level with right-censoring & left-truncation
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


# --- 3.3 TruEnd | Cure event
# Compute Kaplan-Meier survival estimates (product-limit) for WOFF-event | Spell-level with right-censoring & left-truncation
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


# --- 3.4 NoTruEnd | Cure event
# Compute Kaplan-Meier survival estimates (product-limit) for WOFF-event | Spell-level with right-censoring & left-truncation
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




# ------ 4. Data preparation towards graphing survival quantities

# --- 4.1 Data preparation: TruEnd | Write-off event
# Includes alternative calculation methods by which the discrete baseline hazard h(t) is calculated
datHaz_woff_TruEnd <- data.table(Time=kmDef_woff_real_spell1c_TruEnd$time, AtRisk_n=kmDef_woff_real_spell1c_TruEnd$n.risk, 
                             Event_n = kmDef_woff_real_spell1c_TruEnd$n.event, Censored_n=kmDef_woff_real_spell1c_TruEnd$n.censor,
                             hazard=kmDef_woff_real_spell1c_TruEnd$n.event/kmDef_woff_real_spell1c_TruEnd$n.risk, 
                             CumulHazard = kmDef_woff_real_spell1c_TruEnd$cumhaz, Group="1",
                             Surv_KM = kmDef_woff_real_spell1c_TruEnd$surv) %>% 
  filter(Event_n > 0 | Censored_n >0) %>%
  # Discrete-time variants
  mutate(CumulHazard_Disc = -cumsum(log(1-hazard)), Surv_KM_Disc = cumprod(1-hazard)) %>% 
  mutate(Event_KM_Disc = 1-Surv_KM_Disc) %>% as.data.table()
datHaz_woff_TruEnd[, Surv_KM_Disc_prev:= shift(Surv_KM_Disc, n=1, type="lag"), by=list(Group)]
datHaz_woff_TruEnd[Time==Time[1], hazard2 := 1- Surv_KM_Disc]
datHaz_woff_TruEnd[Time>Time[1], hazard2 := 1 - Surv_KM_Disc/Surv_KM_Disc_prev]
n_surv <- NROW(datHaz_woff_TruEnd)
all.equal(datHaz_woff_TruEnd$hazard[1:(n_surv-1)], datHaz_woff_TruEnd$hazard2[1:(n_surv-1)]) # Should be TRUE
all.equal(datHaz_woff_TruEnd$Surv_KM, datHaz_woff_TruEnd$Surv_KM_Disc) # Should be TRUE
all.equal(datHaz_woff_TruEnd$CumulHazard, datHaz_woff_TruEnd$CumulHazard_Disc) # Typically FALSE
plot((datHaz_woff_TruEnd$CumulHazard-datHaz_woff_TruEnd$CumulHazard_Disc))


# --- 4.2 Data preparation: NoTruEnd | Write-off event
# Includes alternative calculation methods by which the discrete baseline hazard h(t) is calculated
datHaz_woff_NoTruEnd <- data.table(Time=kmDef_woff_real_spell1c_NoTruEnd$time, AtRisk_n=kmDef_woff_real_spell1c_NoTruEnd$n.risk, 
                               Event_n = kmDef_woff_real_spell1c_NoTruEnd$n.event, Censored_n=kmDef_woff_real_spell1c_NoTruEnd$n.censor,
                               hazard=kmDef_woff_real_spell1c_NoTruEnd$n.event/kmDef_woff_real_spell1c_NoTruEnd$n.risk, 
                               CumulHazard = kmDef_woff_real_spell1c_NoTruEnd$cumhaz, Group="1",
                               Surv_KM = kmDef_woff_real_spell1c_NoTruEnd$surv) %>% 
  filter(Event_n > 0 | Censored_n >0) %>%
  # Discrete-time variants
  mutate(CumulHazard_Disc = -cumsum(log(1-hazard)), Surv_KM_Disc = cumprod(1-hazard)) %>% 
  mutate(Event_KM_Disc = 1-Surv_KM_Disc) %>% as.data.table()
datHaz_woff_NoTruEnd[, Surv_KM_Disc_prev:= shift(Surv_KM_Disc, n=1, type="lag"), by=list(Group)]
datHaz_woff_NoTruEnd[Time==Time[1], hazard2 := 1- Surv_KM_Disc]
datHaz_woff_NoTruEnd[Time>Time[1], hazard2 := 1 - Surv_KM_Disc/Surv_KM_Disc_prev]
n_surv <- NROW(datHaz_woff_NoTruEnd)
all.equal(datHaz_woff_NoTruEnd$hazard[1:(n_surv-1)], datHaz_woff_NoTruEnd$hazard2[1:(n_surv-1)]) # Should be TRUE
all.equal(datHaz_woff_NoTruEnd$Surv_KM, datHaz_woff_NoTruEnd$Surv_KM_Disc) # Should be TRUE
all.equal(datHaz_woff_NoTruEnd$CumulHazard, datHaz_woff_NoTruEnd$CumulHazard_Disc) # Typically FALSE
plot((datHaz_woff_NoTruEnd$CumulHazard-datHaz_woff_NoTruEnd$CumulHazard_Disc))


# --- 4.3 Data preparation: TruEnd | Cure event
# Includes alternative calculation methods by which the discrete baseline hazard h(t) is calculated
datHaz_cured_TruEnd <- data.table(Time=kmDef_Cured_real_spell1c_TruEnd$time, AtRisk_n=kmDef_Cured_real_spell1c_TruEnd$n.risk, 
                                 Event_n = kmDef_Cured_real_spell1c_TruEnd$n.event, Censored_n=kmDef_Cured_real_spell1c_TruEnd$n.censor,
                                 hazard=kmDef_Cured_real_spell1c_TruEnd$n.event/kmDef_Cured_real_spell1c_TruEnd$n.risk, 
                                 CumulHazard = kmDef_Cured_real_spell1c_TruEnd$cumhaz, Group="1",
                                 Surv_KM = kmDef_Cured_real_spell1c_TruEnd$surv) %>% 
  filter(Event_n > 0 | Censored_n >0) %>%
  # Discrete-time variants
  mutate(CumulHazard_Disc = -cumsum(log(1-hazard)), Surv_KM_Disc = cumprod(1-hazard)) %>% 
  mutate(Event_KM_Disc = 1-Surv_KM_Disc) %>% as.data.table()
datHaz_cured_TruEnd[, Surv_KM_Disc_prev:= shift(Surv_KM_Disc, n=1, type="lag"), by=list(Group)]
datHaz_cured_TruEnd[Time==Time[1], hazard2 := 1- Surv_KM_Disc]
datHaz_cured_TruEnd[Time>Time[1], hazard2 := 1 - Surv_KM_Disc/Surv_KM_Disc_prev]
n_surv <- NROW(datHaz_cured_TruEnd)
all.equal(datHaz_cured_TruEnd$hazard[1:(n_surv-1)], datHaz_cured_TruEnd$hazard2[1:(n_surv-1)]) # Should be TRUE
all.equal(datHaz_cured_TruEnd$Surv_KM, datHaz_cured_TruEnd$Surv_KM_Disc) # Should be TRUE
all.equal(datHaz_cured_TruEnd$CumulHazard, datHaz_cured_TruEnd$CumulHazard_Disc) # Typically FALSE
plot((datHaz_cured_TruEnd$CumulHazard-datHaz_cured_TruEnd$CumulHazard_Disc))


# --- 4.4 Data preparation: NoTruEnd | Cure event
# Includes alternative calculation methods by which the discrete baseline hazard h(t) is calculated
datHaz_cured_NoTruEnd <- data.table(Time=kmDef_Cured_real_spell1c_NoTruEnd$time, AtRisk_n=kmDef_Cured_real_spell1c_NoTruEnd$n.risk, 
                                   Event_n = kmDef_Cured_real_spell1c_NoTruEnd$n.event, Censored_n=kmDef_Cured_real_spell1c_NoTruEnd$n.censor,
                                   hazard=kmDef_Cured_real_spell1c_NoTruEnd$n.event/kmDef_Cured_real_spell1c_NoTruEnd$n.risk, 
                                   CumulHazard = kmDef_Cured_real_spell1c_NoTruEnd$cumhaz, Group="1",
                                   Surv_KM = kmDef_Cured_real_spell1c_NoTruEnd$surv) %>% 
  filter(Event_n > 0 | Censored_n >0) %>%
  # Discrete-time variants
  mutate(CumulHazard_Disc = -cumsum(log(1-hazard)), Surv_KM_Disc = cumprod(1-hazard)) %>% 
  mutate(Event_KM_Disc = 1-Surv_KM_Disc) %>% as.data.table()
datHaz_cured_NoTruEnd[, Surv_KM_Disc_prev:= shift(Surv_KM_Disc, n=1, type="lag"), by=list(Group)]
datHaz_cured_NoTruEnd[Time==Time[1], hazard2 := 1- Surv_KM_Disc]
datHaz_cured_NoTruEnd[Time>Time[1], hazard2 := 1 - Surv_KM_Disc/Surv_KM_Disc_prev]
n_surv <- NROW(datHaz_cured_NoTruEnd)
all.equal(datHaz_cured_NoTruEnd$hazard[1:(n_surv-1)], datHaz_cured_NoTruEnd$hazard2[1:(n_surv-1)]) # Should be TRUE
all.equal(datHaz_cured_NoTruEnd$Surv_KM, datHaz_cured_NoTruEnd$Surv_KM_Disc) # Should be TRUE
all.equal(datHaz_cured_NoTruEnd$CumulHazard, datHaz_cured_NoTruEnd$CumulHazard_Disc) # Typically FALSE
plot((datHaz_cured_NoTruEnd$CumulHazard-datHaz_cured_NoTruEnd$CumulHazard_Disc))



# --- 4.5 Data fusion

# - Combining the datasets with and without the TruEnd into a single dataset
Combined_woff <- rbind(data.table(datHaz_woff_TruEnd, Dataset = "a_TruEnd"), data.table(datHaz_woff_NoTruEnd, Dataset = "b_NoTruEnd"))
Combined_cured <- rbind(data.table(datHaz_cured_TruEnd, Dataset = "a_TruEnd"), data.table(datHaz_cured_NoTruEnd, Dataset = "b_NoTruEnd"))

# Aggregate to period-level (reporting date) | Survival probability
datWoff_cumulInc <- rbind(Combined_woff[Dataset == "a_TruEnd",list(Time,Dataset,Value = 1-Surv_KM)],
                      Combined_woff[Dataset == "b_NoTruEnd",list(Time,Dataset,Value = 1-Surv_KM)])
datCured_cumulInc <- rbind(Combined_cured[Dataset == "a_TruEnd",list(Time,Dataset,Value = 1-Surv_KM)],
                           Combined_cured[Dataset == "b_NoTruEnd",list(Time,Dataset,Value = 1-Surv_KM)])

# Aggregate to period-level (reporting date) | hazard rate
datWoff_hazard <- rbind(Combined_woff[Dataset == "a_TruEnd",list(Time,Dataset,Value = hazard)],
                      Combined_woff[Dataset == "b_NoTruEnd",list(Time,Dataset,Value = hazard)])
datCured_hazard <- rbind(Combined_cured[Dataset == "a_TruEnd",list(Time,Dataset,Value = hazard)],
                         Combined_cured[Dataset == "b_NoTruEnd",list(Time,Dataset,Value = hazard)])





# ------ 5. Graph: Cumulative lifetime distribution function => F(t) = 1-S(t) = 1-y = 1-KM(t)

# --- 5.1 Time span [0,60] months | Write-off event
datWoff_cumulInc_1 <- datWoff_cumulInc[Time<=60,]

# - Calculate aggregated MAEs
AES_Woff_cumulInc_1 <- datWoff_cumulInc_1[,list(AE = abs(diff(Value))), by=list(Time)]
(MAE_Woff_cumulInc_1 <- mean(AES_Woff_cumulInc_1$AE, na.rm=T))

# - Graphing parameters
chosenFont <- "Cambria"
vCol <- brewer.pal(n=3, name = "Set2")[c(1,2,3)]
vLabel <- c("a_TruEnd"=bquote(italic(F)[T]*": TruEnd "),
             "b_NoTruEnd"=bquote(italic(F)[F]*": No TruEnd "))
linetype.v <- c("solid", "dashed")
aggrSeries1_1 <- max(datWoff_cumulInc_1$Value, na.rm=T)

# - Cumulative lifetime distribution => F(t) = 1-S(t) = 1-y = 1-KM(t)
(gsurv1c_Combined_Ft_1 <- ggplot(datWoff_cumulInc_1, aes(x=Time, y=Value)) + geom_line(aes(colour=factor(Dataset), linetype=factor(Dataset))) +
    theme_bw() + 
    labs(x=bquote("Default spell age (months) "*italic(t)), y=bquote("Cumulative lifetime "*italic(F(t))*" [Write-off]") ) + 
    annotate("text", x=15, y=aggrSeries1_1*0.9, size=3, family=chosenFont,
             label=paste0("'MAE between '*italic(F)[T]*' and '*italic(F)[F]*': ", sprintf("%.3f", MAE_Woff_cumulInc_1*100), "%'"), parse=T) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom") + 
    scale_color_brewer(palette="Dark2", name="Dataset", labels=vLabel) + 
    scale_fill_brewer(palette="Dark2", name="Dataset", labels=vLabel)+
    scale_linetype_manual(name="Dataset", values=linetype.v, labels=vLabel) +
    scale_y_continuous(labels = percent))
  
dpi <- 260
ggsave(print(gsurv1c_Combined_Ft_1,newpage=F), file=paste0(genFigPath,"Ft_Woff1.png"),width=1200/dpi, height=1000/dpi,dpi=dpi)



# --- 5.2 Time span (60,120] months | Write-off event
datWoff_cumulInc_2 <- datWoff_cumulInc[Time >= 61 & Time <= 120,]

# - Calculate aggregated MAEs
AES_Woff_cumulInc_2 <- datWoff_cumulInc_2[,list(AE = abs(diff(Value))), by=list(Time)]
(MAE_Woff_cumulInc_2 <- mean(AES_Woff_cumulInc_2$AE, na.rm=T))

# - Graphing parameters
chosenFont <- "Cambria"
vCol <- brewer.pal(n=3, name = "Set2")[c(1,2,3)]
vLabel <- c("a_TruEnd"=bquote(italic(F)[T]*": TruEnd "),
            "b_NoTruEnd"=bquote(italic(F)[F]*": No TruEnd "))
linetype.v <- c("solid", "dashed")
aggrSeries1_2 <- max(datWoff_cumulInc_2$Value, na.rm=T)

# - Cumulative lifetime distribution  => F(t) = 1-S(t) = 1-y = 1-KM(t)
(gsurv1c_Combined_Ft_2 <- ggplot(datWoff_cumulInc_2, aes(x=Time, y=Value)) + geom_line(aes(colour=factor(Dataset), linetype=factor(Dataset))) +
    theme_bw() + 
    labs(x=bquote("Default spell age (months) "*italic(t)),y=bquote("Cumulative lifetime "*italic(F(t))*" [Write-off]")) + 
    annotate("text", x=75, y=aggrSeries1_2*0.98, size=3, family=chosenFont,
             label=paste0("'MAE between '*italic(F)[T]*' and '*italic(F)[F]*': ", sprintf("%.3f", MAE_Woff_cumulInc_2*100), "%'"), parse=T) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom") + 
    scale_color_brewer(palette="Dark2", name="Dataset", labels=vLabel) + 
    scale_fill_brewer(palette="Dark2", name="Dataset", labels=vLabel)+
    scale_linetype_manual(name="Dataset", values=linetype.v, labels=vLabel) +
    scale_y_continuous(labels = percent))

dpi <- 260
ggsave(print(gsurv1c_Combined_Ft_2,newpage=F), file=paste0(genFigPath,"Ft_Woff2.png"),width=1200/dpi, height=1000/dpi,dpi=dpi)



# --- 5.3 Time span (121,240] months | Write-off event
datWoff_cumulInc_3 <- datWoff_cumulInc[Time >= 121 & Time <= 240,]

# - Calculate aggregated MAEs
AES_Woff_cumulInc_3 <- datWoff_cumulInc_3[,list(AE = abs(diff(Value))), by=list(Time)]
(MAE_Woff_cumulInc_3 <- mean(AES_Woff_cumulInc_3$AE, na.rm=T))

# - Graphing parameters
chosenFont <- "Cambria"
vCol <- brewer.pal(n=3, name = "Set2")[c(1,2,3)]
vLabel <- c("a_TruEnd"=bquote(italic(F)[T]*": TruEnd "),
            "b_NoTruEnd"=bquote(italic(F)[F]*": No TruEnd "))
linetype.v <- c("solid", "dashed")
aggrSeries1_2 <- max(datWoff_cumulInc_3$Value, na.rm=T)

# - Cumulative lifetime distribution => F(t) = 1-S(t) = 1-y = 1-KM(t)
(gsurv1c_Combined_Ft_3 <- ggplot(datWoff_cumulInc_3, aes(x=Time, y=Value)) + geom_line(aes(colour=factor(Dataset), linetype=factor(Dataset))) +
    theme_bw() + 
    labs(x=bquote("Default spell age (months) "*italic(t)), y=bquote("Cumulative lifetime "*italic(F(t))*" [Write-off]")) + 
    annotate("text", x=150, y=aggrSeries1_2*0.99, size=3, family=chosenFont,
             label=paste0("'MAE between '*italic(F)[T]*' and '*italic(F)[F]*': ", sprintf("%.3f", MAE_Woff_cumulInc_3*100), "%'"), parse=T) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom") + 
    scale_color_brewer(palette="Dark2", name="Dataset", labels=vLabel) + 
    scale_fill_brewer(palette="Dark2", name="Dataset", labels=vLabel)+
    scale_linetype_manual(name="Dataset", values=linetype.v, labels=vLabel) +
    scale_y_continuous(labels = percent))

dpi <- 260
ggsave(print(gsurv1c_Combined_Ft_3,newpage=F), file=paste0(genFigPath,"Ft_Woff3.png"),width=1200/dpi, height=1000/dpi,dpi=dpi)



# --- 5.4 Time span [0,60] months | Cured event
datCured_cumulInc_1 <- datCured_cumulInc[Time<=60,]

# - Calculate aggregated MAEs
AES_Cured_cumulInc_1 <- datCured_cumulInc_1[,list(AE = abs(diff(Value))), by=list(Time)]
(MAE_Cured_cumulInc_1 <- mean(AES_Cured_cumulInc_1$AE, na.rm=T))

# - Graphing parameters
chosenFont <- "Cambria"
vCol <- brewer.pal(n=3, name = "Set2")[c(1,2,3)]
vLabel <- c("a_TruEnd"=bquote(italic(F)[T]*": TruEnd "),
            "b_NoTruEnd"=bquote(italic(F)[F]*": No TruEnd "))
linetype.v <- c("solid", "dashed")
aggrSeries1_1 <- max(datCured_cumulInc_1$Value, na.rm=T)

# - Cumulative lifetime distribution => F(t) = 1-S(t) = 1-y = 1-KM(t)
(gsurv1c_Combined_Ft_1_cured <- ggplot(datCured_cumulInc_1, aes(x=Time, y=Value)) + geom_line(aes(colour=factor(Dataset), linetype=factor(Dataset))) +
    theme_bw() + 
    labs(x=bquote("Default spell age (months) "*italic(t)), y=bquote("Cumulative lifetime "*italic(F(t))*" [Cure]")) + 
    annotate("text", x=15, y=aggrSeries1_1*0.9, size=3, family=chosenFont,
             label=paste0("'MAE between '*italic(F)[T]*' and '*italic(F)[F]*': ", sprintf("%.3f", MAE_Cured_cumulInc_1*100), "%'"), parse=T) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom") + 
    scale_color_brewer(palette="Dark2", name="Dataset", labels=vLabel) + 
    scale_fill_brewer(palette="Dark2", name="Dataset", labels=vLabel)+
    scale_linetype_manual(name="Dataset", values=linetype.v, labels=vLabel) +
    scale_y_continuous(labels = percent))

dpi <- 260
ggsave(print(gsurv1c_Combined_Ft_1_cured,newpage=F), file=paste0(genFigPath,"Ft_Cured1.png"),width=1200/dpi, height=1000/dpi,dpi=dpi)



# --- 5.5 Time span (60,120] months | Cured event
datCured_cumulInc_2 <- datCured_cumulInc[Time >= 61 & Time <= 120,]

# - Calculate aggregated MAEs
AES_Cured_cumulInc_2 <- datCured_cumulInc_2[,list(AE = abs(diff(Value))), by=list(Time)]
(MAE_Cured_cumulInc_2 <- mean(AES_Cured_cumulInc_2$AE, na.rm=T))

# - Graphing parameters
chosenFont <- "Cambria"
vCol <- brewer.pal(n=3, name = "Set2")[c(1,2,3)]
vLabel <- c("a_TruEnd"=bquote(italic(F)[T]*": TruEnd "),
            "b_NoTruEnd"=bquote(italic(F)[F]*": No TruEnd "))
linetype.v <- c("solid", "dashed")
aggrSeries1_2 <- max(datCured_cumulInc_2$Value, na.rm=T)

# - Cumulative lifetime distribution  => F(t) = 1-S(t) = 1-y = 1-KM(t)
(gsurv1c_Combined_Ft_2_cured <- ggplot(datCured_cumulInc_2, aes(x=Time, y=Value)) + geom_line(aes(colour=factor(Dataset), linetype=factor(Dataset))) +
    theme_bw() + 
    labs(x=bquote("Default spell age (months) "*italic(t)),y=bquote("Cumulative lifetime "*italic(F(t))*" [Cure]")) + 
    annotate("text", x=75, y=aggrSeries1_2*0.98, size=3, family=chosenFont,
             label=paste0("'MAE between '*italic(F)[T]*' and '*italic(F)[F]*': ", sprintf("%.3f", MAE_Cured_cumulInc_2*100), "%'"), parse=T) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom") + 
    scale_color_brewer(palette="Dark2", name="Dataset", labels=vLabel) + 
    scale_fill_brewer(palette="Dark2", name="Dataset", labels=vLabel)+
    scale_linetype_manual(name="Dataset", values=linetype.v, labels=vLabel) +
    scale_y_continuous(labels = percent))

dpi <- 260
ggsave(print(gsurv1c_Combined_Ft_2_cured,newpage=F), file=paste0(genFigPath,"Ft_Cured2.png"),width=1200/dpi, height=1000/dpi,dpi=dpi)



# --- 5.6 Time span (121,240] months | Cured event
datCured_cumulInc_3 <- datCured_cumulInc[Time >= 121 & Time <= 240,]

# - Calculate aggregated MAEs
AES_Cured_cumulInc_3 <- datCured_cumulInc_3[,list(AE = abs(diff(Value))), by=list(Time)]
(MAE_Cured_cumulInc_3 <- mean(AES_Cured_cumulInc_3$AE, na.rm=T))

# - Graphing parameters
chosenFont <- "Cambria"
vCol <- brewer.pal(n=3, name = "Set2")[c(1,2,3)]
vLabel <- c("a_TruEnd"=bquote(italic(F)[T]*": TruEnd "),
            "b_NoTruEnd"=bquote(italic(F)[F]*": No TruEnd "))
linetype.v <- c("solid", "dashed")
aggrSeries1_2 <- max(datCured_cumulInc_3$Value, na.rm=T)

# - Cumulative lifetime distribution  => F(t) = 1-S(t) = 1-y = 1-KM(t)
(gsurv1c_Combined_Ft_3_cured <- ggplot(datCured_cumulInc_3, aes(x=Time, y=Value)) + geom_line(aes(colour=factor(Dataset), linetype=factor(Dataset))) +
    theme_bw() + 
    labs(x=bquote("Default spell age (months) "*italic(t)), y=bquote("Cumulative lifetime "*italic(F(t))*" [Cure]")) + 
    annotate("text", x=200, y=aggrSeries1_2*0.99, size=3, family=chosenFont,
             label=paste0("'MAE between '*italic(F)[T]*' and '*italic(F)[F]*': ", sprintf("%.3f", MAE_Cured_cumulInc_3*100), "%'"), parse=T) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom") + 
    scale_color_brewer(palette="Dark2", name="Dataset", labels=vLabel) + 
    scale_fill_brewer(palette="Dark2", name="Dataset", labels=vLabel)+
    scale_linetype_manual(name="Dataset", values=linetype.v, labels=vLabel) +
    scale_y_continuous(labels = percent))

dpi <- 260
ggsave(print(gsurv1c_Combined_Ft_3_cured,newpage=F), file=paste0(genFigPath,"Ft_Cured3.png"),width=1200/dpi, height=1000/dpi,dpi=dpi)







# ------ 6. Graph: Empirical hazard functions


# --- 6.1 Time span [0,60] months | Write-off event
datWoff_hazard_1 <- datWoff_hazard[Time<=60,]

# - Calculate aggregated MAEs
AES_Woff_hazard_1 <- datWoff_hazard_1[,list(AE = abs(diff(Value))), by=list(Time)]
(MAE_Woff_hazard_1 <- mean(AES_Woff_hazard_1$AE, na.rm=T))

# - Graphing parameters
chosenFont <- "Cambria"
vCol <- brewer.pal(n=3, name = "Set2")[c(1,2,3)]
vLabel <- c("a_TruEnd"=bquote(italic(h)[T]*": TruEnd "),
            "b_NoTruEnd"=bquote(italic(h)[F]*": No TruEnd "))
linetype.v <- c("solid", "dashed")
aggrSeries2_1 <- max(datWoff_hazard_1$Value, na.rm=T)

# Hazard rate
(gsurv1c_Combined_ht_1 <- ggplot(datWoff_hazard_1, aes(x=Time, y=Value)) + geom_line(aes(colour=factor(Dataset), linetype=factor(Dataset))) +
    theme_bw() + 
    labs(x=bquote("Default spell age (months) "*italic(t)), y=bquote("Estimated hazard "*italic(h(t))*" [Write-off]")) + 
    annotate("text", x=40, y=aggrSeries2_1*0.5, size=3, family=chosenFont,
             label=paste0("'MAE between '*italic(h)[T]*' and '*italic(h)[F]*': ", sprintf("%.3f", MAE_Woff_hazard_1*100), "%'"), parse=T) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom") + 
    scale_color_brewer(palette="Dark2", name="Dataset", labels=vLabel) + 
    scale_fill_brewer(palette="Dark2", name="Dataset", labels=vLabel)+
    scale_linetype_manual(name="Dataset", values=linetype.v, labels=vLabel) +
    scale_y_continuous(labels = percent))

dpi <- 260
ggsave(print(gsurv1c_Combined_ht_1,newpage=F), file=paste0(genFigPath,"ht_Woff1.png"),width=1200/dpi, height=1000/dpi,dpi=dpi)



# --- 6.2 Time span (60,120] months | Write-off event
datWoff_hazard_2 <- datWoff_hazard[Time >= 61 & Time <= 120,]

# - Calculate aggregated MAEs
AES_Woff_hazard_2 <- datWoff_hazard_2[,list(AE = abs(diff(Value))), by=list(Time)]
(MAE_Woff_hazard_2 <- mean(AES_Woff_hazard_2$AE, na.rm=T))

# - Graphing parameters
chosenFont <- "Cambria"
vCol <- brewer.pal(n=3, name = "Set2")[c(1,2,3)]
vLabel <- c("a_TruEnd"=bquote(italic(h)[T]*": TruEnd "),
            "b_NoTruEnd"=bquote(italic(h)[F]*": No TruEnd "))
linetype.v <- c("solid", "dashed")
aggrSeries2_1 <- max(datWoff_hazard_2$Value, na.rm=T)

# Hazard rate
(gsurv1c_Combined_ht_2 <- ggplot(datWoff_hazard_2, aes(x=Time, y=Value)) + geom_line(aes(colour=factor(Dataset), linetype=factor(Dataset))) +
    theme_bw() + 
    labs(x=bquote("Default spell age (months) "*italic(t)), y=bquote("Estimated hazard "*italic(h(t))*" [Write-off]")) +  
    annotate("text", x=105, y=aggrSeries2_1*0.96, size=3, family=chosenFont,
             label=paste0("'MAE between '*italic(h)[T]*' and '*italic(h)[F]*': ", sprintf("%.3f", MAE_Woff_hazard_2*100), "%'"), parse=T) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom") + 
    scale_color_brewer(palette="Dark2", name="Dataset", labels=vLabel) + 
    scale_fill_brewer(palette="Dark2", name="Dataset", labels=vLabel) +
    scale_linetype_manual(name="Dataset", values=linetype.v, labels=vLabel) +
    scale_y_continuous(labels = percent))

dpi <- 260
ggsave(print(gsurv1c_Combined_ht_2,newpage=F), file=paste0(genFigPath,"ht_Woff2.png"),width=1200/dpi, height=1000/dpi,dpi=dpi)



# --- 6.3 Time span (120,240] months | Write-off event
datWoff_hazard_3 <- datWoff_hazard[Time >= 121 & Time <= 240,]

# - Calculate aggregated MAEs
AES_Woff_hazard_3 <- datWoff_hazard_3[,list(AE = abs(diff(Value))), by=list(Time)]
(MAE_Woff_hazard_3 <- mean(AES_Woff_hazard_3$AE, na.rm=T))

# - Graphing parameters
chosenFont <- "Cambria"
vCol <- brewer.pal(n=3, name = "Set2")[c(1,2,3)]
vLabel <- c("a_TruEnd"=bquote(italic(h)[T]*": TruEnd "),
            "b_NoTruEnd"=bquote(italic(h)[F]*": No TruEnd "))
linetype.v <- c("solid", "dashed")
aggrSeries2_1 <- max(datWoff_hazard_3$Value, na.rm=T)

# Hazard rate
(gsurv1c_Combined_ht_3 <- ggplot(datWoff_hazard_3, aes(x=Time, y=Value)) + geom_line(aes(colour=factor(Dataset), linetype=factor(Dataset))) +
    theme_bw() + 
    labs(x=bquote("Default spell age (months) "*italic(t)), y=bquote("Estimated hazard "*italic(h(t)*" [Write-off]"))) + 
    annotate("text", x=150, y=aggrSeries2_1*0.8, size=3, family=chosenFont,
             label=paste0("'MAE between '*italic(h)[T]*' and '*italic(h)[F]*': ", sprintf("%.3f", MAE_Woff_hazard_3*100), "%'"), parse=T) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom") + 
    scale_color_brewer(palette="Dark2", name="Dataset", labels=vLabel) + 
    scale_fill_brewer(palette="Dark2", name="Dataset", labels=vLabel) +
    scale_linetype_manual(name="Dataset", values=linetype.v, labels=vLabel) +
    scale_y_continuous(labels = percent))

dpi <- 260
ggsave(print(gsurv1c_Combined_ht_3,newpage=F), file=paste0(genFigPath,"ht_Woff3.png"),width=1200/dpi, height=1000/dpi,dpi=dpi)



# --- 6.4 Time span [0,60] months | Cured event
datCured_hazard_1 <- datCured_hazard[Time<=60,]

# - Calculate aggregated MAEs
AES_Cured_hazard_1 <- datCured_hazard_1[,list(AE = abs(diff(Value))), by=list(Time)]
(MAE_Cured_hazard_1 <- mean(AES_Cured_hazard_1$AE, na.rm=T))

# - Graphing parameters
chosenFont <- "Cambria"
vCol <- brewer.pal(n=3, name = "Set2")[c(1,2,3)]
vLabel <- c("a_TruEnd"=bquote(italic(h)[T]*": TruEnd "),
            "b_NoTruEnd"=bquote(italic(h)[F]*": No TruEnd "))
linetype.v <- c("solid", "dashed")
aggrSeries2_1 <- max(datCured_hazard_1$Value, na.rm=T)

# Hazard rate
(gsurv1c_Combined_ht_1_cured <- ggplot(datCured_hazard_1, aes(x=Time, y=Value)) + geom_line(aes(colour=factor(Dataset), linetype=factor(Dataset))) +
    theme_bw() + 
    labs(x=bquote("Default spell age (months) "*italic(t)), y=bquote("Estimated hazard "*italic(h(t))*" [Write-off]")) + 
    annotate("text", x=40, y=aggrSeries2_1*0.1, size=3, family=chosenFont,
             label=paste0("'MAE between '*italic(h)[T]*' and '*italic(h)[F]*': ", sprintf("%.3f", MAE_Cured_hazard_1*100), "%'"), parse=T) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom") + 
    scale_color_brewer(palette="Dark2", name="Dataset", labels=vLabel) + 
    scale_fill_brewer(palette="Dark2", name="Dataset", labels=vLabel)+
    scale_linetype_manual(name="Dataset", values=linetype.v, labels=vLabel) +
    scale_y_continuous(labels = percent))

dpi <- 260
ggsave(print(gsurv1c_Combined_ht_1_cured,newpage=F), file=paste0(genFigPath,"ht_Cured1.png"),width=1200/dpi, height=1000/dpi,dpi=dpi)



# --- 6.5 Time span (60,120] months | Cured event
datCured_hazard_2 <- datCured_hazard[Time >= 61 & Time <= 120,]

# - Calculate aggregated MAEs
AES_Cured_hazard_2 <- datCured_hazard_2[,list(AE = abs(diff(Value))), by=list(Time)]
(MAE_Cured_hazard_2 <- mean(AES_Cured_hazard_2$AE, na.rm=T))

# - Graphing parameters
chosenFont <- "Cambria"
vCol <- brewer.pal(n=3, name = "Set2")[c(1,2,3)]
vLabel <- c("a_TruEnd"=bquote(italic(h)[T]*": TruEnd "),
            "b_NoTruEnd"=bquote(italic(h)[F]*": No TruEnd "))
linetype.v <- c("solid", "dashed")
aggrSeries2_1 <- max(datCured_hazard_2$Value, na.rm=T)

# Hazard rate
(gsurv1c_Combined_ht_2_cured <- ggplot(datCured_hazard_2, aes(x=Time, y=Value)) + geom_line(aes(colour=factor(Dataset), linetype=factor(Dataset))) +
    theme_bw() + 
    labs(x=bquote("Default spell age (months) "*italic(t)), y=bquote("Estimated hazard "*italic(h(t))*" [Write-off]")) +  
    annotate("text", x=80, y=aggrSeries2_1*0.8, size=3, family=chosenFont,
             label=paste0("'MAE between '*italic(h)[T]*' and '*italic(h)[F]*': ", sprintf("%.3f", MAE_Cured_hazard_2*100), "%'"), parse=T) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom") + 
    scale_color_brewer(palette="Dark2", name="Dataset", labels=vLabel) + 
    scale_fill_brewer(palette="Dark2", name="Dataset", labels=vLabel) +
    scale_linetype_manual(name="Dataset", values=linetype.v, labels=vLabel) +
    scale_y_continuous(labels = percent))

dpi <- 260
ggsave(print(gsurv1c_Combined_ht_2_cured,newpage=F), file=paste0(genFigPath,"ht_Cured2.png"),width=1200/dpi, height=1000/dpi,dpi=dpi)



# --- 6.6 Time span (120,240] months | Cured event
datCured_hazard_3 <- datCured_hazard[Time >= 121 & Time <= 240,]

# - Calculate aggregated MAEs
AES_Cured_hazard_3 <- datCured_hazard_3[,list(AE = abs(diff(Value))), by=list(Time)]
(MAE_Cured_hazard_3 <- mean(AES_Cured_hazard_3$AE, na.rm=T))

# - Graphing parameters
chosenFont <- "Cambria"
vCol <- brewer.pal(n=3, name = "Set2")[c(1,2,3)]
vLabel <- c("a_TruEnd"=bquote(italic(h)[T]*": TruEnd "),
            "b_NoTruEnd"=bquote(italic(h)[F]*": No TruEnd "))
linetype.v <- c("solid", "dashed")
aggrSeries2_1 <- max(datCured_hazard_3$Value, na.rm=T)

# Hazard rate
(gsurv1c_Combined_ht_3_cured <- ggplot(datCured_hazard_3, aes(x=Time, y=Value)) + geom_line(aes(colour=factor(Dataset), linetype=factor(Dataset))) +
    theme_bw() + 
    labs(x=bquote("Default spell age (months) "*italic(t)), y=bquote("Estimated hazard "*italic(h(t))*" [Write-off]")) + 
    annotate("text", x=150, y=aggrSeries2_1*0.8, size=3, family=chosenFont,
             label=paste0("'MAE between '*italic(h)[T]*' and '*italic(h)[F]*': ", sprintf("%.3f", MAE_Cured_hazard_3*100), "%'"), parse=T) + 
    theme(text=element_text(family=chosenFont),legend.position = "bottom") + 
    scale_color_brewer(palette="Dark2", name="Dataset", labels=vLabel) + 
    scale_fill_brewer(palette="Dark2", name="Dataset", labels=vLabel) +
    scale_linetype_manual(name="Dataset", values=linetype.v, labels=vLabel) +
    scale_y_continuous(labels = percent))

dpi <- 260
ggsave(print(gsurv1c_Combined_ht_3_cured,newpage=F), file=paste0(genFigPath,"ht_Cured3.png"),width=1200/dpi, height=1000/dpi,dpi=dpi)




# ------ 7. General cleanup
rm(AES_Woff_cumulInc_1, AES_Woff_cumulInc_2, AES_Woff_cumulInc_3, AES_Woff_hazard_1, AES_Woff_hazard_2, AES_Woff_hazard_3, 
   AES_Cured_cumulInc_1, AES_Cured_cumulInc_2, AES_Cured_cumulInc_3, AES_Cured_hazard_1, AES_Cured_hazard_2, AES_Cured_hazard_3,
   Combined_woff, Combined_cured, datWoff_cumulInc, datWoff_hazard, datCured_cumulInc, datCured_hazard,
   datHaz_woff_TruEnd, datHaz_woff_NoTruEnd, datHaz_cured_TruEnd, datHaz_cured_NoTruEnd,
   datWoff_cumulInc_1, datWoff_cumulInc_2, datWoff_cumulInc_3, datCured_cumulInc_1, datCured_cumulInc_2, datCured_cumulInc_3,
   datWoff_hazard_1, datWoff_hazard_2, datWoff_hazard_3, datCured_hazard_1, datCured_hazard_2, datCured_hazard_3, datSurv_NoTruEnd, datSurv_TruEnd,
   gsurv1c_Combined_Ft_1, gsurv1c_Combined_Ft_2, gsurv1c_Combined_Ft_3, 
   gsurv1c_Combined_Ft_1_cured, gsurv1c_Combined_Ft_2_cured, gsurv1c_Combined_Ft_3_cured,
   gsurv1c_Combined_ht_1, gsurv1c_Combined_ht_2, gsurv1c_Combined_ht_3, 
   gsurv1c_Combined_ht_1_cured, gsurv1c_Combined_ht_2_cured, gsurv1c_Combined_ht_3_cured, 
   vLabel, kmDef_woff_real_spell1c_TruEnd, kmDef_woff_real_spell1c_NoTruEnd,
   kmDef_Cured_real_spell1c_TruEnd, kmDef_Cured_real_spell1c_NoTruEnd)
gc()
