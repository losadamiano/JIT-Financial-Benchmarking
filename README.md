# Financial Benchmarking: JIT Strategy in Italian Manufacturing Startups

## Executive Summary
This project investigates whether postponing production in Italian manufacturing startups represents an effective and sustainable strategy, aligning with **Just In Time (JIT)** principles. 

By leveraging financial data from the **AIDA (Bureau van Dijk)** database, the study provides data-driven evidence for entrepreneurs deciding between an immediate market entry or a strategically delayed launch to optimize capital structure and operational efficiency.

---

## Methodology
The analysis follows a rigorous quantitative workflow using **Multivariate Statistical Analysis**:

### Data Collection & Pre-processing
* **Source:** AIDA Database (Bureau van Dijk).
* **Sample:** Italian manufacturing startups (ATECO C sector).
* **Feature Engineering:** Calculation of key financial ratios in **R**:
  * `Current Ratio` & `Quick Ratio` (Liquidity analysis)
  * `Inventory / Revenue` (Operational efficiency)
  * `Fixed Asset Ratio` (Capital intensity)

### Principal Component Analysis (PCA)
To handle the multidimensional nature of financial statements (11+ indicators), a PCA was performed to reduce dimensionality while preserving maximum variance.
* **Selection:** 3 Principal Components were selected based on the **Scree Plot (Elbow Method)**.
* **Optimization:** Applied **Varimax Rotation** to improve the interpretability of the loadings, ensuring each financial indicator clearly maps to a specific component.

### Trajectory & Distance Analysis
I measured the financial evolution of firms between 2022 and 2023 by calculating the **Euclidean Distance** in the PCA plane:

$$Distanza = \sqrt{\Delta PC_1^2 + \Delta PC_2^2}$$

This metric quantifies the "displacement" or volatility of a firm's financial profile over time.

---

## 💡 Key Findings
* **Financial Stability:** Companies following a **Just In Time (Immediate Production)** model showed significantly lower displacement in the PCA space. This indicates a more stable financial trajectory and consistent management of working capital.
* **Volatility of Delayed Entry:** Startups that postponed production (Group 0) exhibited higher variance and larger "trajectories," suggesting that while delaying might allow for preparation, it introduces higher financial volatility during the first years of activity.
* **Operational Consistency:** JIT firms maintained better control over inventory-to-revenue ratios, minimizing waste and optimizing the cash conversion cycle.

---

## Tech Stack
* **Programming Language:** R
* **Libraries:**
  * `FactoMineR` & `factoextra` (PCA & Visualization)
  * `tidyverse` (Data Wrangling)
  * `psych` (Varimax Rotation)
  * `ggplot2` (Advanced Plotting)
* **Data Source:** AIDA (Bureau van Dijk)

---

## 🚀 How to Use
1. Ensure you have **R** (version 4.0 or higher) and **RStudio** installed.
2. Install the required packages by running:  
   `install.packages(c("tidyverse", "FactoMineR", "factoextra", "psych", "readxl", "writexl"))`
3. Place the dataset **data_finale.xlsx** (or your AIDA export) in the working directory.
4. Run the script **Project_PCA.R** to execute the PCA, generate the rotated component matrices, and produce the displacement plots.

---
**Author:** Damiano Losa 
**Field:** Quantitative Finance / Corporate Data Science




