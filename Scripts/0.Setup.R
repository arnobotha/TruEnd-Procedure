# =================================== SETUP =============================================
# Setting up R environment, parameters, and function definitions
# ---------------------------------------------------------------------------------------
# PROJECT TITLE: TruEnd-procedure
# SCRIPT AUTHOR(S): Dr Arno Botha, Roelinde Bester

# DESCRIPTION: 
# This script installs and loads various libraries and packages, compiles all
# custom functions, and set requisite parameters.
# ---------------------------------------------------------------------------------------
# -- Inputs:
#   - DelinqM.R | Delinquency measures and related functions
# =======================================================================================



# ================ 0. Library setup

# ------ Install and load packages
# - data access and big data management
require(haven) # for SAS imports
require(ETLUtils)
require(ffbase)
require(ff)
tempPath <- "C:/TempData"; options("fftempdir"=tempPath)

# for data wrangling
require(tidyr)
require(dplyr)
require(data.table)
require(lubridate)
require(readr)
require(bit64) # for very big numeric values
require(foreach); require(doParallel) # for multi-threaded computing
require(stringr) # common string operations, e.g, str_pad
require(purrr) # mapping functions from tidyverse in working with matrices, lists
require(writexl) #for exporting to Excel

# for analyses & modelling
require(Hmisc)
require(survival) # for survival modelling

#for plots
require(ggplot2)
require(scales)
require(ggthemes)
require(RColorBrewer)
require(extrafont) #remotes::install_version("Rttf2pt1", version = "1.3.8"); Sys.setenv(R_GSCMD="C:/Program Files/gs/gs9.55.0/bin/gswin32c.exe"); font_import(); loadfonts(); loadfonts(device="win")
require(survminer)
require(gridExtra)



# ================ 1. Parametrisation

# - general R options
options(scipen=999) # Suppress showing scientific notation

# - Parameters used in calculating delinquency measures
sc.Thres <- 0.9; # repayment ratio - g1
d <- 3 # default threshold for g0/g1-measures of delinquency (payments in arrears)
k <- 6 # Probation period


# -- Path variables | General

# - Common path for saving big data objects
genPath <- "C:/Data/TruEnd-Procedure_Data/"

# - Common path for importing raw data
genRawPath <- "C:/Data/"


# -- Path variables | User-dependent

if (Sys.getenv("USERNAME") %in% c("Arno Botha", "arnos")) {
  
  # - Custom path where R-scripts are saved
  path_cust <- "E:/Backupz/Google Drive/WorkLife/Analytix/R&D Codebases/TruEnd-Procedure/Scripts/"
  
  # - Common path for storing important R-objects as back-up
  genObjPath <- "E:/Backupz/Google Drive/WorkLife/Analytix/R&D Codebases/TruEnd-Procedure/Objects/"
  
  # - Common path for saving important analytics (e.g., sampling)
  genFigPath <- "E:/Backupz/Google Drive/WorkLife/Analytix/R&D Codebases/TruEnd-Procedure/Figures/"
  
  # - Common path for saving big data objects
  genPath <- "E:/DataDump/RetailMortgages-FNB/TruEnd-Procedure_Data/"
  
  # - Common path for importing raw data
  genRawPath <- "E:/DataDump/RetailMortgages-FNB/"
  
} else if (Sys.getenv("USERNAME") == "R5422965") {
  
  # - Custom path where R-scripts are saved
  path_cust <- "C:/Users/R5422965/OneDrive - FRG/TruEnd-Procedure/Scripts/"
  
  # - Common path for storing important R-objects as back-up
  genObjPath <- "C:/Users/R5422965/OneDrive - FRG/TruEnd-Procedure/Objects/"
  
  # - Common path for saving important analytics (e.g., sampling)
  genFigPath <- "C:/Users/R5422965/OneDrive - FRG/TruEnd-Procedure/Figures/"

  
} else {
  stop("User-specific paths not set for current user: ", Sys.getenv("USERNAME"), ". Please fix in Setup script (0.Setup.R) before continuing")
}




# ================ 2. Custom functions

# ------ Custom function definitions
# - Load all custom functions defined in a separate R-script
source(paste0(path_cust,"0a.CustomFunctions.R"))

# - Compile Delinquency Calculation Functions (CD, MD/DoD)
source(paste0(path_cust,'DelinqM.R'))

# - Compile the TruEnd-suite of evaluation (and auxiliary) functions
source(paste0(path_cust,'TruEnd.R'))

