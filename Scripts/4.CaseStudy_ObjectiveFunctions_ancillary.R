# ================================== TruEnd-procedure ===================================
# Formulating candidate objective functions using a single loan
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: TruEnd-procedure
# SCRIPT AUTHOR(S): Dr Arno Botha, Prof. Tanja Verster
# ---------------------------------------------------------------------------------------
# -- Script dependencies:
#   - 0.Setup.R
#   - 1.Data_Import.R
#   - 2a.Data_Prepare_Credit_Basic.R
#   - 2b.Date_Prepare_Credit_Advanced.R

# -- Inputs:
#   - datCredit_real | Enhanced version of input dataset (script 2b)
#
# -- Outputs:
#   - <analytics>
# =======================================================================================



# ------ 1. Setup


# --- 0. Preliminaries

caseStudy_Name <- "1Loan"

# - Confirm raw data is loaded into memory
if (!exists('datCredit_real')) unpack.ffdf(paste0(genPath,"creditdata_final1c"), tempPath)

# - Filter specific example
datSingle <- subset(datCredit_real, ExclusionID == 0 & LoanID %in% c(3000006071745),
                   select=c("LoanID", "Counter", "Date", "Principal", "Balance", "WOff_Ind"))

# - memory optimisation
rm(datCredit_real); gc()

# - Create Balance-to-Principal ratio as a candidate control variable
datSingle[Principal > 0, Principal_Ratio := Balance / Principal]

# - Save snapshot for external experimentation
write_xlsx(datSingle, paste0(genObjPath, "Extract_", caseStudy_Name, ".xlsx"))

# - Save to disk (zip) for quick disk-based retrieval later
pack.ffdf(paste0(genObjPath,"Extract_", caseStudy_Name), datSingle)



# --- 1. Preliminaries

# - Confirm extracted case study is loaded into memory
if (!exists('datSingle')) unpack.ffdf(paste0(genObjPath,"Extract_", caseStudy_Name), tempPath)

# - Parameters for TruEnd-procedure
tau <- 6 # length of non-TZB period that should precede an isolated TZB-regime, for M2-purposes

# - Abstract the balance matrix from data for calculation of M1 and M2 measures later
# NOTE: rows are periods (up to maximum observed period), columns are loan accounts
vecBalance <- as.numeric(datSingle$Balance)

# - Prepare data structures for measures M1 and M2
M1.v <- rep(NA,times=3)
M2.v <- copy(M1.v)




# ------ 2. Analysis where TZB-starts are directly pre-selected

# - Consider the following starting points of TZB-regimes
# 1: too early; 2: ideal; 3: too late
t_z.v <- c(56, 62, 66) # picked using expert judgement
scen.v <- c("1. Too early", "2. Ideal", "3. Too late")
falseEnd <- length(vecBalance) # "False"/observed end or loan age

# - calculate M1 and M2 measures accordingly
for (i in 1:length(t_z.v)) {
  # - Measure 1: mean TZB-balance, where found
  M1.v[i] <-  mean(vecBalance[t_z.v[i]:falseEnd],na.rm=T)
  
  # - Measure 2: non-TZB mean balance
  M2.v[i] <- mean(vecBalance[((t_z.v[i]-1)-tau):(t_z.v[i]-1)], na.rm=T)
}


# - Candidate objective function 1
# A premature t_z-point will produce larger M1-values, i.e., we should minimise M1, or equivalently, maximise -M1
# A delayed t_z-point will produce lower M2-values, i.e., we should maximise M2.
# Therefore, pursuing both optimisations imply maximising (M2 - M1)
# Best t_z given by maximising:
plot(f.obj1 <- M2.v - M1.v,type="b")
### RESULTS: Yields an intuitive optimum at 'ideal'


# - Candidate objective function 2
# M2's domain will typically be much larger than that of M1.
# This implies that changes in M2 will affect f.obj1 much more than changes in M1,
# which complicates the optimisation of f.obj1 with regard to M1
# Therefore, define weights for each measure to downscale and upscale the influences of M2 and M1 respectively
# These weights can be preset and left outside of the optimisation itself, just as a practical expedient for now
w1 <- 1 # weight for M1 with its small domain
w2 <- 0.1 # weight for M2 with its large domain, should logically be < w1
# Best t_z given by maximising:
plot(f.obj2 <- w2*M2.v - w1*M1.v,type="b")
### RESULTS: Yields an intuitive optimum at 'ideal'


# - Candidate objective function 3
# Calculating the 'contamination' degree to which M1 is contaminated by M2
# Best t_z given by minimising:
plot(f.obj3 <- M1.v / (M1.v + M2.v), type="b")
### RESULTS: Yields an intuitive optimum at 'ideal'


# - Create plotting object
datPlot <- rbind(data.table(Scenario=scen.v, T_z=t_z.v, Value=M1.v, Type="a_M1", Type2="a_Measure"),
                data.table(Scenario=scen.v, T_z=t_z.v, Value=M2.v, Type="b_M2", Type2="a_Measure"),
                data.table(Scenario=scen.v, T_z=t_z.v, Value=f.obj1, Type="c_Func1", Type2="b_ObjFunc"),
                data.table(Scenario=scen.v, T_z=t_z.v, Value=f.obj2, Type="d_Func2", Type2="b_ObjFunc"),
                data.table(Scenario=scen.v, T_z=t_z.v, Value=f.obj3, Type="e_Func3", Type2="b_ObjFunc"))

# - graphing parameters
chosenFont <- "Cambria"
vCol <- brewer.pal(9, "Set1")
vLinewidth <- c(0.25, 0.75)
vLinetype <- c("dotted", "solid")
vLabel1 <- list("a_M1" = bquote(italic(M[1])), 
                "b_M2" = bquote(italic(M[2])),
                "c_Func1" = bquote(italic(f[1])),
                "d_Func2" = bquote(italic(f[2])),
                "e_Func3" = bquote(italic(f[3])))
vLabel2 <- c("Measures", "Candidate functions")

# - Create main graph
(g2 <- ggplot(datPlot, aes(x=Scenario, y=Value, group=Type)) + theme_minimal() + 
  theme(text=element_text(family=chosenFont), legend.position = "bottom") + 
  labs(y="Function value", x=bquote('Scenario for selecting '*italic(t[z])*'-point' ) ) + 
  geom_line(aes(colour=Type, linewidth=Type2, linetype=Type2)) + 
  geom_point(aes(colour=Type, shape=Type), size=1.5) + 
  scale_colour_manual(name="Functions", values=vCol, labels=vLabel1) + 
  scale_shape_discrete(name="Functions", labels=vLabel1) + 
  scale_linewidth_manual(name = "Type", values=vLinewidth, labels=vLabel2) + 
  scale_linetype_manual(name = "Type", values=vLinetype, labels=vLabel2) + 
  scale_y_continuous(labels = comma) + 
  guides(colour=guide_legend(nrow=2), linetype=guide_legend(nrow=2, ncol=1))
)

# - save plot
dpi <- 175
ggsave(g2, file=paste0(genFigPath,"/TruEnd-CandidateObjFunctions.png"),width=1200/dpi, height=1000/dpi,dpi=dpi, bg="white")

