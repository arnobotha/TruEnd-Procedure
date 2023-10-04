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



# -------- LGD functions

# - Custom function to calculate the present value of a series of cash flows [x], given 
# a vector of nominal interest rates [rate] that correspond to [x]
# NPV = \sum_{t=0}^{n}{x_t * (1+r_t)^{-t}}
calcNPV <- function(x, rate) { sum(x * (1+rate)^(-1*(0:(length(x)-1))), na.rm=T)}

# - Custom function for calculating the derivative of the NVP wrt rate (r): d(npv)/dr | "derivative" of the calcNPV()-function
# Step 1: For a single t-value: d/dr ( x_t * (1+r_t)^{-t} ) = x_t * d/dr ( (1+r_t)^{-t} )
# Step 2: Using chain rule on d/dr ( (1+r_t)^{-t} ):  -t * (1+r_t)^{-t-1} * d/dr (1+r_t) = -t * (1+r_t)^{-t-1} * (1)
# Step 3: Combine Steps 1-2: d/dr ( x_t * (1+r_t)^{-t} ) is therefore = x_t * (-t) * (1+r_t)^{-t-1}
# Step 4: Derivative of NPV is the sum of derivatives of each t-value, i.e., 
#   d(npv)/dr = \sum_{t=0}^{n}{   d/dr ( x_t * (1+r_t)^{-t} )  }
# Step 5: Substitute Step 3's result into Step 4: d(npv)/dr = \sum_{t=0}^{n}{ x_t * (-t) * (1+r_t)^{-t-1}  }
calcNPV_deriv <- function(x, rate) {
  sum(x * (-1*(0:(length(x)-1))) * (1+rate)^(-1*(1:length(x))), na.rm=T)
}

# - Custom function for calculating the second derivative of the NVP wrt rate (r): d(npv)/dr | "derivative" of the calcNPV_deriv()-function
# Similar process as to that of calcNPV_deriv():
# d2(npv)/dr2 = \sum_{t=0}^{n}{ x_t * (t) * (t+1) * (1+r_t)^{-t-2}  }
calcNPV_deriv2 <- function(x, rate) {
  sum(x * (0:(length(x)-1)) * (1:(length(x))) * (1+rate)^(-1*(2:(length(x)+1))), na.rm=T)
}

# - Custom function for calculating the internal rate of return
calcIRR <- function(x, irr_init = 0.1, tolerance=1e-7, max_iter=1000, method="Newton-Raphson") {
  # - Testing conditions
  # x <- c(-4115,1000,1100,1200); method <- "Newton-Raphson"
  
  irr <- irr_init # initial guess
  
  # - Execute based on given optimisation method
  if (method == "Newton-Raphson") {
    # - Newton-Raphson method | see https://en.wikipedia.org/wiki/Newton%27s_method
    
    for (i in 1:max_iter) {
      obj <- calcNPV(x,irr) # calculate objective function at current rate
      # Update rate
      irr <- irr - obj / calcNPV_deriv(x,irr)
      # Check convergence
      if (abs(obj) < tolerance) {
        cat(paste0("\nConverged after ", i, " iterations. IRR: ", sprintf("%.2f",irr*100), "%\n"))
        return(irr)
      }
    }
    cat("\nNewton-Raphson's method did not converge .. retrying with Halley's method.");
    calcIRR(x, irr_init, tolerance=tolerance, max_iter=max_iter, method="Halley")
    
  } else if (method == "Halley") {
    # - Hallye's method | see https://en.wikipedia.org/wiki/Halley%27s_method
    
    for (i in 1:max_iter) {
      
      obj <- calcNPV(x,irr) # calculate objective function at current rate
      deriv1 <- calcNPV_deriv(x,irr) # calculate 1st derivative of objective function
      deriv2 <- calcNPV_deriv2(x,irr) # calculate 2nd derivative of objective function
      
      # Update rate
      irr <- irr - (2*obj*deriv1) / (2*deriv1^2 - obj*deriv2)
      
      # Check convergence
      if (abs(obj) < tolerance) {
        cat(paste0("\nConverged after ", i, " iterations. IRR: ", sprintf("%.2f",irr*100), "%\n"))
        return(irr)
      }
    }
    warning("Halley's method did not converge"); return(irr)
  }
  else {stop("Specified method not implemented!")}
}

# - Unit tests
# calcIRR(c(-4115,1000,1100,1200)); calcIRR(c(-4115,1000,1100,1200), method="Halley")
### RESULTS: Corresponds to IRR function in Excel and an online calculator, given the same cash flows

