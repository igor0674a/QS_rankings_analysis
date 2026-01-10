####################################################################################################
#  Unveiling Hidden Structures in the QS World University Rankings
#  Multivariate analysis of the 2026 edition and notes of caution in interpretation
####################################################################################################

####################################################################################################
#  Script purpose
#  --------------
#  This script:
#   1) Loads the QS World University Rankings 2026 dataset
#   2) Selects a subset of ranking indicators and categorical descriptors used in the analysis
#   3) Filters the dataset to the top 705 universities
#   4) Audits missing values (overall, by variable, and by row)
#   5) Performs multiple imputation using MICE, with CART ("cart") applied only to a subset of variables
#   6) Exports an imputed dataset to a local directory as a CSV file
#
#  Notes for reproducibility
#  -------------------------
#  - The script sets a random seed before imputation to ensure reproducible results.
#  - Output path is a local Windows directory (setwd(...)); update it to your environment.
#  - CART imputation is used only for selected variables; all other variables are left unchanged.
#
#  Dependencies
#  ------------
#  - tidyverse: used for data manipulation (slice_head)
#  - mice: used for multiple imputation by chained equations
#
####################################################################################################


####################################################################################################
## 1. Load the data
####################################################################################################
# Reads the dataset directly from Dropbox (direct-download URL).
# Expected: a CSV file containing QS indicator scores and categorical descriptors.
MyData <- read.csv(
  "https://www.dropbox.com/scl/fi/epn3y9ad4fntb5hpuirfw/UniversityRankings2026_v2.csv?rlkey=li0jbw7g8xek7tcla7dk6q2r3&dl=1",
  header = TRUE
)

####################################################################################################
## 1.1 Select the variables for the analysis
####################################################################################################
# Define the variables used in subsequent steps:
# - Indicator scores (numeric): overall and pillar/indicator components
# - Structural descriptors (categorical): SIZE, FOCUS, RES, STATUS
# IMPORTANT: These names must match column names in `MyData` exactly.
selected_vars <- c(
  "Overall_SCORE",
  "Citations_Faculty_Score",
  "International_Research_Network_Score",
  "Academic_Reputation_Score",
  "Employer_Reputation_Score",
  "Faculty_Student_RScore",
  "International_Faculty_Score",
  "International_Students_Score",
  "Employment_Outcomes_Score",
  "Sustainability_Score",
  "SIZE",
  "FOCUS",
  "RES",
  "STATUS"
)

####################################################################################################
## 2. Function for identifying and counting missing values
####################################################################################################
# Utility function for auditing missingness in a data.frame.
#
# --------------------
# total_missing        : total number of NA values in the dataset
# missing_by_variable  : NA count per column (named numeric vector)
# missing_by_row       : NA count per row (numeric vector)
count_missing <- function(df) {
  stopifnot(is.data.frame(df))
  list(
    total_missing = sum(is.na(df)),
    missing_by_variable = colSums(is.na(df)),
    missing_by_row = rowSums(is.na(df))
  )
}


####################################################################################################
## 3. Filtering the top 705 universities
####################################################################################################
# Loads tidyverse for dplyr pipes and `slice_head`.
library(tidyverse)

# Filters the dataset to the first 705 rows (assumed to correspond to the top-705 universities).
MyData_705 <- MyData %>%
  slice_head(n = 705)

# Missingness audit restricted to the analysis variables.
count_missing(MyData_705[, selected_vars])



####################################################################################################
## 4. Data imputation (MICE)
####################################################################################################
# Objective
# ---------
# Perform multiple imputation via MICE, using CART ("cart") only for a subset of variables that
# contain missing values, while leaving all other variables unimputed.
#
# Key objects created in this section
# -----------------------------------
# variables_to_impute : character vector of variable names targeted for imputation
# meth                : method vector controlling which variables are imputed and how
# pred                : predictor matrix controlling which variables predict which imputations
# imp                 : mids object returned by mice()
# MyData_imputed       : completed dataset extracted from the mids object
####################################################################################################

# 4.1 Load the mice library
library(mice)

# Indicate the variables for which to impute missing values.
# Only these variables will be assigned an imputation method (CART).
variables_to_impute <- c(
  "International_Faculty_Score",
  "International_Students_Score",
  "Sustainability_Score",
  "International_Research_Network_Score"
)

# 4.2 Initialize method vector
# - `make.method(MyData)` creates a default method vector aligned with `MyData` columns.
# - Setting all entries to "" disables imputation for every variable.
# - Assigning "cart" to selected variables enables CART-based imputation only for those.
meth <- make.method(MyData)
meth[] <- ""                          # no imputation anywhere
meth[variables_to_impute] <- "cart"   # CART only for selected vars


#---------------------------------------------------------------------------------------------------
# 4.3 Predictor matrix
#---------------------------------------------------------------------------------------------------
# The predictor matrix controls which variables are used to predict missing values in each target.
# By default, `make.predictorMatrix(MyData)` enables broad prediction structure.
pred <- make.predictorMatrix(MyData)

# 4.4 Prevent variables from predicting themselves
# Ensures the diagonal is zero so a variable cannot be used as its own predictor.
diag(pred) <- 0


#---------------------------------------------------------------------------------------------------
# 4.5 Run MICE
#---------------------------------------------------------------------------------------------------
# Reproducibility: set a random seed before running mice().
set.seed(123)

# Run multiple imputations:
# - m = 5     : number of imputed datasets generated
# - maxit = 5 : number of iterations per imputation
# - printFlag : prints progress and diagnostic messages
imp <- mice(
  data = MyData,
  method = meth,
  predictorMatrix = pred,
  m = 5,
  maxit = 5,
  printFlag = TRUE
)


#---------------------------------------------------------------------------------------------------
# 4.6 Extract and save a completed dataset
#---------------------------------------------------------------------------------------------------
# Extract a single completed dataset from the mids object.
# action = 1 selects the first imputed dataset among the m=5 imputations.
MyData_imputed <- complete(imp, action = 1)

# Post-imputation missingness check (should be 0 for imputed variables, unless constrained otherwise).
count_missing(MyData_imputed)

# Output directory (local Windows path).
# Update this path as needed for your environment before running.
setwd("C:/Users/igor/Dropbox/01_KFUPM/03_IRC_KBS/01_QS_Rankings MVA/03_Data")

# Export the imputed dataset as CSV.
write.csv(MyData_imputed, "UniversityRankings2026_imp_v2.csv")
