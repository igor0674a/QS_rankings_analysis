## Unveiling Hidden Structures in QS World University Rankings (2026)

**A reproducible R analysis exploring latent structures in the QS World University Rankings, 2026 edition — with notes of caution in interpretation.**

What this repo contains

**Data preprocessing (04_MFA_Academic_Rankings_v7_imp)**
1. Loads the QS World University Rankings 2026 dataset, selects core indicator and institutional variables, and restricts the sample to the top 705 ranked universities.  
2. Audits missing data systematically by computing missingness at the dataset, variable, and institutional levels prior to analysis.  
3. Performs targeted multiple imputation using MICE with CART applied only to a predefined subset of indicators exhibiting missing values.  
4. Ensures reproducibility by explicitly controlling the imputation methods, predictor matrix, and random seed used in the imputation process.  
5. Exports a cleaned, imputed, analysis-ready dataset for use in subsequent multivariate analyses and figure generation.  

**Data analysis (04_MFA_Academic_Rankings_v7_06Jan26)**

1. Loads and prepares **QS indicator data** (2026 edition)  
2. Builds **correlation heatmaps** by institutional **SIZE**  
3. Produces **boxplots** of *Overall Score* across **SIZE × FOCUS**, with descriptive tables  
4. Runs **PCA (FactoMineR)** by SIZE to reveal latent dimensions  
5. Visualizes **variable contributions** to **PC1** and **PC2**

**Figures correspond to those in the manuscript:**
1. Fig 1A–1B: **Correlation matrices (XL vs S)**.
2. Fig 2: **Boxplot of Overall Score** across SIZE × FOCUS.
3. Fig 3A–3B: **PCA indicators-maps** (XL vs S).
4. Fig 4–5: **Indicator contributions** to PC1 and PC2 (XL and S).

**Data**
- Source: QS World University Rankings 2026 (available on demand from: https://www.topuniversities.com/).
- Scope: First 705 rows include Overall Score and are used in the core analyses.
- Indicators (scores, 0–100):
    - Overall Score
    - Citations per Faculty
    - International Research Network
    - Academic Reputation
    - Employer Reputation
    - Faculty/Student
    - International Faculty
    - International Students
    - Employment Outcomes
    - Sustainability
- Metadata:
    - SIZE
    - FOCUS
    - Research intensity (RES)
    - STATUS

**Methods snapshot**
- Correlation analysis of numerical indicators by SIZE groups.
- Nonparametric comparisons: Boxplots + descriptive tables; post-hoc Dunn test with BH adjustment.
- PCA (FactoMineR) per SIZE with factoextra visualizations; contribution bar charts highlight drivers of PC1/PC2.

## Reproducibility Notes
- Scripts are designed to be run sequentially in a clean R session
- Analyses rely on widely used R packages (*tidyverse*, *FactoMineR*, *factoextra*, *ggplot2*)
- No proprietary software is required beyond QS data available on demand from: https://www.topuniversities.com/

## Intended Use
This repository is intended for:
- Researchers studying **global university rankings**
- Scholars interested in **indicator systems and stratification**
- Methodological work on **latent structure and multivariate analysis**
- Transparent replication of published results
