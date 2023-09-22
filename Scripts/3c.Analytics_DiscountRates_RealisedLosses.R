# ------ Setup

# - Load example data
lookup <- read.csv("Example1_DefaultSpell.csv")
lookup2 <- read.csv("Example1_AllHistory.csv")



# ------ Define custom functions for calculating the Internal Rate of Return (IRR)

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
calcIRR(x=c(-4115,1000,1100,1200)); calcIRR(x=c(-4115,1000,1100,1200), method="Halley")
### RESULTS: Corresponds to IRR function in Excel and an online calculator, given the same cash flows



# -- Method 1: Calculate realised loss rate using contract rate as discount rate
# PV of cash flows
calcNPV(lookup$Receipt_Inf, lookup$InterestRate_Nom/12)
# realised loss rate
sprintf("%.2f", (1 - calcNPV(lookup$Receipt_Inf, lookup$InterestRate_Nom/12)/lookup$Balance[1])*100)
### RESULTS: -1.18% loss

# -- Calculate internal rate of return on expected instalment vector | Last default spell
discRate1 <- calcIRR(c(-lookup$Balance[1], lookup$Instalment), method="Halley")
### RESULTS: -22.05% IRR

# -- Calculate internal rate of return on expected instalment vector | All history
discRate2 <- calcIRR(c(-lookup2$Balance[1], lookup2$Instalment), method="Halley")
### RESULTS: -0.35% IRR

# -- Calculate internal rate of return on expected instalment vector | All history up to start of last default spell
lookup3 <- subset(lookup2, DefSpell_Num < max(lookup2$DefSpell_Num, na.rm=T))
discRate3 <- calcIRR(c(-lookup3$Balance[1], lookup3$Instalment), method="Halley")
### RESULTS: -0.8% IRR

# -- Method 2: Calculate realised loss rate using IRR as discount rate
sprintf("%.2f", (1 - calcNPV(lookup$Receipt_Inf, discRate1/12)/lookup$Balance[1])*100)
sprintf("%.2f", (1 - calcNPV(lookup$Receipt_Inf, discRate2/12)/lookup$Balance[1])*100)
### RESULTS: [discRate1]: -33.69%; [discRate2]: -9.6%

# This is test


