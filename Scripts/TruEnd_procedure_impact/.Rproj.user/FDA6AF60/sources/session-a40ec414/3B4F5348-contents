# ============================== CUSTOM FUNCTIONS ==============================
# Defining custom functions used across various projects
# ------------------------------------------------------------------------------
# PROJECT TITLE: TruEnd-procedure
# SCRIPT AUTHOR(S): Dr Arno Botha

# DESCRIPTION:
# This script defines various functions that are used elsewhere in this project
# or, indeed, used across other projects. Functions are grouped thematically.
# ==============================================================================



# -------- Ternary functions
# from https://stackoverflow.com/questions/8790143/does-the-ternary-operator-exist-in-r
`%?%` <- function(x, y) list(x = x, y = y)
`%:%` <- function(xy, z) if(xy$x) xy$y else z



# -------- Utility functions
# - Mode function (R doesn't have a built-int one)
getmode <- function(v) {
  uniqv <- unique(v);
  # discard any missingness
  uniqv <- uniqv[complete.cases(uniqv)]
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# - Memory function using 'gdata' package
getMemUsage <- function(limit=1000){
  require(gdata); require(scales)
  # - Get list of significant object sizes occupied in memory, order ascendingly
  totUsage <- ll()
  memSize <- subset(totUsage, KB >= limit)
  memSize$MB <- memSize$KB/1000
  gc(verbose=F)
  cat("Total memory used: ", comma(sum(totUsage$KB)/1000), "MB\n")
  cat("Big objects size: ", comma(sum(memSize$MB)), "MB\n\n")
  return(  memSize[order(memSize$KB), c(1,3)])
}



# -------- Cleaning functions

# Custom function that curates a main vector [x] to equal the previous/most-recent non-
# missing element in a given vector
imputeLastKnown <- function (x) {
  # -- Testing purposes
  # x <- Lookup$ZeroBal_Remain_Ind; x_lead <- Lookup$ZeroBal_Remain_Ind_lead
  # x <- c(0,0,0,1,1,1,0,1)
  # x <- c(0,0,0,1,1,1,0,NA)
  # x <- c(0,0,0,1,1,1,1,NA)
  # x <- c(0,0,0,1,NA,1,0,NA)
  # x <- c(0,NA)
  
  firstOne <- which(is.na(x))[1]
  if (!is.na(firstOne) & firstOne > 1) {
    x[firstOne] <- x[firstOne-1]
    # call function recursively to fix earlier missing-cases
    return( imputeLastKnown(x))
  } else { # no missing value found, return original vector
    return(x)
  }
}


# Custom function that curates a main vector [x] where x[1] is missing.
# This is achieve by finding the first non-missing element and back-filling that value
imputeFirstKnown <- function(x) {
  # -- Testing purposes
  # x <- c(NA, NA, 2,3,4)
  firstOne <- which(!is.na(x))[1]
  if (!is.na(firstOne) & firstOne > 1) {
    x[1:(firstOne-1)] <- x[firstOne]
    return(x)
  } else { # no non-missing value found, return original vector
    return(x)
  }
}



