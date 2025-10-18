#####################################################################
#   Unveiling hidden structures in the QS World University Rankings: 
#   a multivariate analysis of the 2026 edition and notes of caution 
#   in interpretation
#####################################################################


library(factoextra)
library(tidyverse)
library(FactoMineR)
library(corrplot)


#####################################################################
# Step 1.   data pre processing
#####################################################################
myData <- read.csv("https://www.dropbox.com/scl/fi/1axalwkr9l9nmdyrsjlwc/UniversityRankings2025_imp.csv?rlkey=rfxy4z5r15bkgcq4ewxa2wnsa&dl=1",
                   fileEncoding = "latin1")

myData <- myData %>%
  mutate(SIZE = recode(SIZE,
                       "S"  = "01_Small",
                       "M"  = "02_Medium",
                       "L"  = "03_Large",
                       "XL" = "04_Extra_Large"))
myData <- myData %>%
  mutate(FOCUS = recode(FOCUS,
                        "SP"  = "01_Specialist",
                        "FO"  = "02_Focused",
                        "CO"  = "03_Comprehensive",
                        "FC"  = "04_Fully_Comprehensive"))

myData <- myData %>%
  mutate(RES = recode(RES,
                      "LO"  = "01_Low",
                      "MD"  = "02_Medium",
                      "HI"  = "03_High",
                      "VH"  = "04_Very_High"))

myData <- myData %>%
  mutate(STATUS = recode(STATUS,
                         "Private for Profit"      = "01_Private_Profit",
                         "Private not for Profit"  = "02_Private_NOT_profit",
                         "Public"                  = "03_Public"))

summary.factor(myData$SIZE)
summary.factor(myData$FOCUS)
summary.factor(myData$RES)
summary.factor(myData$STATUS)

myData705 <- myData %>% 
          slice_head(n = 705)

selected_vars <- c(
  "Overall_SCORE", #ok
  "Citations_Faculty_Score", #ok
  "International_Research_Network_Score", #ok
  "Academic_Reputation_Score", #ok
  "Employer_Reputation_Score", #ok 
  "Faculty_Student_RScore", #ok
  "International_Faculty_Score", #ok
  "International_Students_Score", #ok
  "Employment_Outcomes_Score",  #ok 
  "Sustainability_Score", #ok
  "SIZE",
  "FOCUS",
  "RES",
  "STATUS")



#####################################################################
#####################################################################
# Step 2.  Correlation matrices
#####################################################################
#####################################################################
selected_cor <- c(
  "Overall_SCORE", "Citations_Faculty_Score", "International_Research_Network_Score",  
  "Academic_Reputation_Score", "Employer_Reputation_Score",  "Faculty_Student_RScore",   
  "International_Faculty_Score","International_Students_Score", 
  "Employment_Outcomes_Score", "Sustainability_Score")



#########################################3
#### corplots for size
# Load required libraries
library(ggcorrplot)
library(patchwork)  # For combining ggplots
library(ggplot2)


# Function to compute correlation and plot using ggcorrplot
create_corr_plot <- function(datos, title) {
  corr_matrix <- cor(datos, use = "complete.obs")
  
  ggcorrplot(corr_matrix, 
             #hc.order = TRUE, 
             method = "square",
             outline.color = "white",
             ggtheme = theme_minimal(),
             lab = TRUE,
             lab_size = 3.0,
             colors = c("red", "white", "lightblue")) +
    labs(title = title) +
    theme(
      plot.title   = element_text(hjust = 0.5, size = 10, face = "bold"),
      legend.text  = element_text(size = 10),   # 
      legend.title = element_text(size = 14),   # 
      axis.text.x  = element_text(size = 10, angle = 45, hjust = 1), # 
      axis.text.y  = element_text(size = 10)    # smaller y labels
    )
}

summary.factor(myData705$SIZE)
# SIZE = "XL"
plot1 <- create_corr_plot(myData[myData$SIZE == '04_Extra_Large', selected_cor], "XL")
plot1


# SIZE = "L"
plot2 <- create_corr_plot(myData[myData$SIZE == '03_Large', selected_cor], "L")
#plot2


# SIZE = "M"
plot3 <- create_corr_plot(myData[myData$SIZE == '02_Medium', selected_cor], "M")
#plot3


# SIZE = "S"
plot4 <- create_corr_plot(myData705[myData705$SIZE == '01_Small', selected_cor], "S")
plot4


# This line produces Figures 1A and 1B exactly as presented in the manuscript
combined1 <- (plot1 | plot4) 
x11()
combined1

###################################################################################
#### Step 3. Boxplot for of Overall_SCORE across SIZE                            
###################################################################################
create_boxplot <- function(datos, title = "Boxplot of Overall_SCORE across SIZE") {
  ggplot(datos, aes(x = SIZE, y = Overall_SCORE, fill = FOCUS)) +
    geom_boxplot(outlier.shape = NA) +   # hide outliers completely
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

# This line produces Figure 2,  exactly as presented in the manuscript
boxplot1 <- create_boxplot(myData705[, selected_vars])
boxplot1 <- boxplot1 + theme_classic()
boxplot1


## This line generates descriptive statistics complementary to the boxplot.
library(crosstable)
crosstable(myData705, Overall_SCORE, by=c(FOCUS, SIZE)) %>%
  as_flextable(keep_id=TRUE)


### This line conducts the hypothesis test for Overall Score across Focus categories. 
library(rstatix)
dunn_test(myData705, Overall_SCORE ~ FOCUS , p.adjust.method="BH")


###############################################################################
### Step 4. Unsupervised machine-assisted multivariate analysis
###############################################################################

qs_pca_XL <- myData705[myData705$SIZE=="04_Extra_Large",c(selected_cor,"Institution_Name")]
qs_pca_S <- myData705[myData705$SIZE=="01_Small",c(selected_cor,"Institution_Name")]

qs_pca_M <- myData[myData$SIZE=="02_Medium",c(selected_cor,"Institution_Name")]
qs_pca_L <- myData[myData$SIZE=="03_Large",c(selected_cor,"Institution_Name")]


rownames(qs_pca_XL) <- qs_pca_XL$Institution_Name
rownames(qs_pca_S) <- qs_pca_S$Institution_Name

rownames(qs_pca_M) <- qs_pca_M$Institution_Name
rownames(qs_pca_L) <- qs_pca_L$Institution_Name



res.pca_XL <- PCA(qs_pca_XL[,selected_cor],  graph = FALSE)
res.pca_S <- PCA(qs_pca_S[,selected_cor],  graph = FALSE)

res.pca_M <- PCA(qs_pca_M[,selected_cor],  graph = FALSE)
res.pca_L <- PCA(qs_pca_L[,selected_cor],  graph = FALSE)



library(ggplot2)
library(gridExtra)
x11()
p1 <- fviz_pca_var(res.pca_XL, col.var="contrib",
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                   repel = TRUE,
                   title="XL Univ")

p2 <- fviz_pca_var(res.pca_S, col.var="contrib",
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                   repel = TRUE,
                   title="S Univ")

# This line generates Figures 3A and 3B as presented in the manuscript.
grid.arrange(p1, p2, ncol = 2)



###############################################################################
### Step 5. Bar charts showing the contribution of indicators to PC1 and PC2.
###############################################################################
## This line generates Figure 4 as shown in the manuscript.
x11()
p1 <- fviz_contrib(res.pca_XL, choice = "var", axes = 1, top = 10, title='XL Univ')
p2 <- fviz_contrib(res.pca_XL, choice = "var", axes = 2, top = 10, title='XL Univ')
# This line generates Figure 5 as shown in the manuscript.
grid.arrange(p1, p2, ncol = 2)


## This line generates Figure 5 as shown in the manuscript.
x11()
p1 <- fviz_contrib(res.pca_S, choice = "var", axes = 1, top = 10, title='S Univ')
p2 <- fviz_contrib(res.pca_S, choice = "var", axes = 2, top = 10, title='S Univ')
# Arrange plots in one page (side by side)
grid.arrange(p1, p2, ncol = 2)



