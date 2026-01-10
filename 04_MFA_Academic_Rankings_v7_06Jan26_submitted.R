####################################################################################################
#  Unveiling hidden structures in the QS World University Rankings:
#  A multivariate analysis of the 2026 edition and notes of caution in interpretation
####################################################################################################

####################################################################################################
#  Script overview
#  ---------------
#  This script reproduces the descriptive and multivariate analyses reported in the manuscript and
#  generates the corresponding figures/tables:
#
#   Step 1) Data preprocessing
#          - Load the imputed QS 2026 dataset
#          - Recode grouping variables (SIZE, FOCUS, RES, STATUS) with ordered labels
#          - Filter the top 705 institutions (Overall_SCORE available only for these observations)
#          - Define variable sets used throughout the script
#
#   Step 2) Correlation matrices (Figures 1A and 1B)
#          - Compute correlations for indicator scores within SIZE subgroups (XL vs S)
#          - Plot correlation heatmaps with numeric labels
#
#   Step 3) Boxplot visualization (Figure 2)
#          - Boxplot of Overall_SCORE across SIZE (fill by FOCUS)
#
#   Step 4) Hypothesis test (exportable results table)
#          - Dunn’s post-hoc test for Overall_SCORE across FOCUS categories (BH adjustment)
#          - Export results to CSV
#
#   Step 5) Unsupervised multivariate analysis (Figures 3A and 3B)
#          - PCA on indicator scores for XL and S subgroups
#          - Variable contribution biplots
#
#   Step 6) Contribution bar charts (Figures 4 and 5)
#          - Top contributing indicators to PC1 and PC2 for XL and S subgroups
#
#  Reproducibility notes
#  --------------------
#  - This script reads the input dataset from a Dropbox direct-download URL (dl=1).
#  - Plots are displayed interactively using x11(). If you are on Windows, macOS, RStudio Server,
#    or Quarto, you may need to remove/replace x11() depending on your graphics device.
#  - File outputs are written to the current working directory unless otherwise specified.
#
#  Expected input columns (minimum)
#  -------------------------------
#  Numeric indicators:
#   Overall_SCORE, Citations_Faculty_Score, International_Research_Network_Score,
#   Academic_Reputation_Score, Employer_Reputation_Score, Faculty_Student_RScore,
#   International_Faculty_Score, International_Students_Score, Employment_Outcomes_Score,
#   Sustainability_Score
#
#  Categorical descriptors:
#   SIZE, FOCUS, RES, STATUS
#
#  Identifiers (for PCA rownames):
#   Institution_Name
####################################################################################################


####################################################################################################
# Libraries
####################################################################################################
# NOTE: Some libraries are loaded again later in the script for clarity near usage blocks.
#       Keeping them here ensures dependencies are explicit at the top.
library(factoextra)
library(tidyverse)
library(FactoMineR)
library(corrplot)


####################################################################################################
# Step 1. Data pre-processing
####################################################################################################

####################################################################################################
# 1.1 Reading the data for the analysis
####################################################################################################
# Loads the (already imputed) QS World University Rankings 2026 dataset (v2) from Dropbox.
# `fileEncoding = "latin1"` is used to avoid issues with special characters in institution names.
myData <- read.csv(
  "https://www.dropbox.com/scl/fi/l0x49xxwp9vbafftycfb9/UniversityRankings2026_imp_v2.csv?rlkey=7zg8on7q0bprc3jj2zp99p6i2&dl=1",
  fileEncoding = "latin1")


####################################################################################################
# 1.2 Mapping groups: SIZE, FOCUS, RES and STATUS
####################################################################################################
# Recode categorical grouping variables into ordered, human-readable labels.
# Prefixes (01_, 02_, ...) are intentional to preserve ordering in plots and summaries.
#
# SIZE  : institutional size category
# FOCUS : institutional focus category
# RES   : research intensity category
# STATUS: institutional ownership/governance
myData <- myData %>%
  mutate(SIZE = recode(
    SIZE,
    "S"  = "01_Small",
    "M"  = "02_Medium",
    "L"  = "03_Large",
    "XL" = "04_Extra_Large"
  ))

myData <- myData %>%
  mutate(FOCUS = recode(
    FOCUS,
    "SP" = "01_Specialist",
    "FO" = "02_Focused",
    "CO" = "03_Comprehensive",
    "FC" = "04_Fully_Comprehensive"
  ))

myData <- myData %>%
  mutate(RES = recode(
    RES,
    "LO" = "01_Low",
    "MD" = "02_Medium",
    "HI" = "03_High",
    "VH" = "04_Very_High"
  ))

myData <- myData %>%
  mutate(STATUS = recode(
    STATUS,
    "Private for Profit"     = "01_Private_Profit",
    "Private not for Profit" = "02_Private_NOT_profit",
    "Public"                 = "03_Public"
  ))

# Quick distribution checks (ensures recoding worked and levels look reasonable).
summary.factor(myData$SIZE)
summary.factor(myData$FOCUS)
summary.factor(myData$RES)
summary.factor(myData$STATUS)


####################################################################################################
# 1.3 Keeping the top 705 universities
####################################################################################################
# The manuscript restricts analysis to the top 705 institutions because Overall_SCORE is only
# available for this subset in the Original QS WUR dataset.
myData705 <- myData %>%
  slice_head(n = 705)


####################################################################################################
# 1.4 Filtering the selected variables for the analysis
####################################################################################################
# `selected_vars` is used for downstream plotting and tests where both indicator scores and
# grouping variables are required.
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
# Step 2. Correlation matrices 
# This script generates Figures 1A and 1B
####################################################################################################
# Purpose
# -------
# Create correlation heatmaps of the QS indicator scores for two SIZE subgroups:
#  - Extra Large institutions (Figure 1A)
#  - Small institutions (Figure 1B)
#
# Implementation details
# ----------------------
# - Correlations are computed using complete observations only (use = "complete.obs")
# - Heatmaps show numeric correlation values and use a diverging palette
####################################################################################################

####################################################################################################
# 2.1 Filtering variables for visualization
####################################################################################################
# `selected_cor` includes only numeric indicators to avoid correlation errors with categorical vars.
selected_cor <- c(
  "Overall_SCORE",
  "Citations_Faculty_Score",
  "International_Research_Network_Score",
  "Academic_Reputation_Score",
  "Employer_Reputation_Score",
  "Faculty_Student_RScore",
  "International_Faculty_Score",
  "International_Students_Score",
  "Employment_Outcomes_Score",
  "Sustainability_Score"
)


####################################################################################################
# 2.2 Generating visualizations (required libraries for the correlation figures)
####################################################################################################
library(ggcorrplot)
library(patchwork)
library(ggplot2)


####################################################################################################
# 2.3 Function to compute correlation and plot using ggcorrplot
####################################################################################################
# create_corr_plot()
# ------------------
# Computes a correlation matrix and returns a formatted ggcorrplot object.
#
# Parameters
# ----------
# datos : data.frame
#   Data containing only numeric columns used for correlation computation.
# title : character
#   Plot title label (e.g., "XL", "S").
#
# Returns
# -------
# A ggplot object (ggcorrplot) ready for display or composition via patchwork.
create_corr_plot <- function(datos, title) {
  corr_matrix <- cor(datos, use = "complete.obs")
  
  ggcorrplot(
    corr_matrix,
    #hc.order = TRUE,
    method = "square",
    outline.color = "white",
    ggtheme = theme_minimal(),
    lab = TRUE,
    lab_size = 3.0,
    colors = c("red", "white", "lightblue")
  ) +
    labs(title = title) +
    theme(
      plot.title   = element_text(hjust = 0.5, size = 10, face = "bold"),
      legend.text  = element_text(size = 10),
      legend.title = element_text(size = 14),
      axis.text.x  = element_text(size = 10, angle = 45, hjust = 1),
      axis.text.y  = element_text(size = 10)
    )
}

# Confirm SIZE levels after recoding (useful sanity check before subsetting by SIZE).
summary.factor(myData705$SIZE)


####################################################################################################
# 2.4 Generating Figure 1A (Extra Large institutions)
####################################################################################################
# Ensure numeric type for correlation calculations (protects against accidental character import).
myData705$Overall_SCORE <- as.numeric(myData705$Overall_SCORE)

# SIZE = "XL"
plot1 <- create_corr_plot(
  myData705[myData705$SIZE == "04_Extra_Large", selected_cor],
  "XL"
)
plot1


####################################################################################################
# 2.5 Generating Figure 1B (Small institutions)
####################################################################################################
# SIZE = "S"
plot4 <- create_corr_plot(
  myData705[myData705$SIZE == "01_Small", selected_cor],
  "S"
)
plot4


####################################################################################################
# 2.6 Compose and display Figures 1A and 1B (as presented in the manuscript)
####################################################################################################
# Patchwork syntax: (plot1 | plot4) arranges plots side-by-side.
combined1 <- (plot1 | plot4)
x11()
combined1



####################################################################################################
# Step 3. Boxplot (Figure 2)
####################################################################################################
# Purpose
# -------
# Visualize the distribution of Overall_SCORE across institutional SIZE categories,
# with boxes filled by FOCUS category.
#
# Note
# ----
# The y-axis is restricted to [20, 100] to match manuscript figure scaling.
####################################################################################################

####################################################################################################
# 3.1 Function for creating the boxplot
####################################################################################################
# create_boxplot()
# ----------------
# Returns a ggplot2 boxplot for Overall_SCORE by SIZE with fill mapped to FOCUS.
#
# Parameters
# ----------
# datos : data.frame
#   Dataset containing SIZE, Overall_SCORE, and FOCUS at minimum.
# title : character
#   Plot title.
#
# Returns
# -------
# A ggplot object.
create_boxplot <- function(datos, title = "Boxplot of Overall_SCORE across SIZE") {
  ggplot(datos, aes(x = SIZE, y = Overall_SCORE, fill = FOCUS)) +
    geom_boxplot(outlier.shape = NA) +   # hide outliers completely (match manuscript style)
    labs(title = title, x = "RES", y = "Overall Score", fill = "Focus") +
    coord_cartesian(ylim = c(20, 100)) + # limit y-axis range
    theme_minimal(base_size = 12) +
    theme(
      plot.title   = element_text(hjust = 0.5, size = 12, face = "bold"),
      axis.text.x  = element_text(size = 13),
      axis.text.y  = element_text(size = 13),
      legend.position = "right",
      legend.title    = element_text(size = 13, face = "bold"),
      legend.text     = element_text(size = 12)
    )
}

####################################################################################################
# 3.2 Generate Figure 2 (as presented in the manuscript)
####################################################################################################
boxplot1 <- create_boxplot(myData705[, selected_vars])
boxplot1 <- boxplot1 + theme_classic()
boxplot1



####################################################################################################
# Step 4. Hypothesis test for Overall_SCORE across FOCUS categories
####################################################################################################
# Purpose
# -------
# Following an omnibus nonparametric test (not shown here), this step runs Dunn’s test for
# pairwise comparisons across FOCUS categories with Benjamini–Hochberg (BH) p-value adjustment.
#
# Output
# ------
# A CSV file containing the Dunn test results is written to the working directory.
####################################################################################################

####################################################################################################
# 4.1 Conducting the test
####################################################################################################
library(rstatix)

# Run Dunn's test and store results
dunn_results <- dunn_test(
  myData705,
  Overall_SCORE ~ FOCUS,
  p.adjust.method = "BH"
)

####################################################################################################
# 4.2 Export hypothesis testing table to CSV
####################################################################################################
# The exported table can be used directly to populate manuscript results tables/appendices.
write.csv(
  dunn_results,
  file = "Dunn_Test_OverallScore_by_Focus.csv",
  row.names = FALSE
)


####################################################################################################
# Step 5. Unsupervised machine-assisted multivariate analysis (PCA)
# This script generates Figures 3A and 3B
####################################################################################################
# Purpose
# -------
# Run PCA on the indicator scores for two SIZE subgroups (XL and S) and visualize variable
# contributions (fviz_pca_var) colored by contribution.
#
# Implementation detail
# ---------------------
# Institution_Name is used to label rows to make PCA outputs interpretable.
####################################################################################################

####################################################################################################
# 5.1 Filtering Extra Large universities (XL)
####################################################################################################
qs_pca_XL <- myData705[myData705$SIZE == "04_Extra_Large", c(selected_cor, "Institution_Name")]
rownames(qs_pca_XL) <- qs_pca_XL$Institution_Name

####################################################################################################
# 5.2 Filtering Small universities (S)
####################################################################################################
qs_pca_S <- myData705[myData705$SIZE == "01_Small", c(selected_cor, "Institution_Name")]
rownames(qs_pca_S) <- qs_pca_S$Institution_Name


####################################################################################################
# 5.3 Calculating PCA for Extra Large universities
####################################################################################################
res.pca_XL <- PCA(qs_pca_XL[, selected_cor], graph = FALSE)

####################################################################################################
# 5.4 Calculating PCA for Small universities
####################################################################################################
res.pca_S <- PCA(qs_pca_S[, selected_cor], graph = FALSE)


####################################################################################################
# 5.5 Generate visualizations (Figures 3A and 3B)
####################################################################################################
library(ggplot2)
library(gridExtra)

x11()
p1 <- fviz_pca_var(
  res.pca_XL,
  col.var = "contrib",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE,
  title = "XL Univ"
)

p2 <- fviz_pca_var(
  res.pca_S,
  col.var = "contrib",
  gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE,
  title = "S Univ"
)

####################################################################################################
# 5.6 Compose Figures 3A and 3B (as presented in the manuscript)
####################################################################################################
grid.arrange(p1, p2, ncol = 2)



####################################################################################################
# Step 6. Bar charts showing the contribution of indicators to PC1 and PC2
# This script generates Figures 4 and 5
####################################################################################################
# Purpose
# -------
# Visualize the top 10 indicator contributions to:
#  - PC1 (axes = 1)
#  - PC2 (axes = 2)
# for each SIZE subgroup PCA (XL and S).
####################################################################################################

####################################################################################################
# 6.1 Generate Figure 4 (XL Univ contributions to PC1 and PC2)
####################################################################################################
x11()
p1 <- fviz_contrib(res.pca_XL, choice = "var", axes = 1, top = 10, title = "XL Univ")
p2 <- fviz_contrib(res.pca_XL, choice = "var", axes = 2, top = 10, title = "XL Univ")
grid.arrange(p1, p2, ncol = 2)


####################################################################################################
# 6.2 Generate Figure 5 (S Univ contributions to PC1 and PC2)
####################################################################################################
x11()
p1 <- fviz_contrib(res.pca_S, choice = "var", axes = 1, top = 10, title = "S Univ")
p2 <- fviz_contrib(res.pca_S, choice = "var", axes = 2, top = 10, title = "S Univ")
grid.arrange(p1, p2, ncol = 2)



####################################################################################################
# End of the script
####################################################################################################

