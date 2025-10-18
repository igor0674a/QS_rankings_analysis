## Unveiling Hidden Structures in QS World University Rankings (2026)

**A reproducible R analysis exploring latent structures in the QS World University Rankings, 2026 edition — with notes of caution in interpretation.**

What this repo contains

**End-to-end R workflow that:**
1. Loads and prepares **QS indicator data** (2026 edition, imputed)  
2. Builds **correlation heatmaps** by institutional **SIZE**  
3. Produces **boxplots** of *Overall Score* across **SIZE × FOCUS**, with descriptive tables  
4. Runs **PCA (FactoMineR)** by SIZE to reveal latent dimensions  
5. Visualizes **variable contributions** to **PC1** and **PC2**

**Figures correspond to those in the mentioned manuscript:**
1. Fig 1A–1B: **Correlation matrices (XL vs S)**.
2. Fig 2: **Boxplot of Overall Score** across SIZE × FOCUS.
3. Fig 3A–3B: **PCA indicators-maps** (XL vs S).
4. Fig 4–5: **Indicator contributions** to PC1 and PC2 (XL and S).

**Data**
- Source: QS World University Rankings 2026 (publicly available from: https://www.topuniversities.com/).
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
