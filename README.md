                                                                              #####Latitudinal Patterns in Cryptic Species Diversity of Tardigrades Using BIN-Based Global Assessments#####
-This project analyzes cryptic diversity in tardigrade species across polar and temperate zones using BIN/species ratios.

 📁 Dataset
- `tardigrade_data.tsv` — Tab-separated dataset of species observations and coordinates.
- Source: [https://boldsystems.org/]

 🔧 Tools & Packages
- **Language**: R
- **Packages**: tidyverse, ggridges, viridis, assertthat, maps

 📊 Workflow Overview
1. Data cleaning and filtering
2. Latitude/longitude processing and zone classification
3. Site-level BIN/species ratio computation
4. Statistical testing (Wilcoxon)
5. Visualizations (ridge plots, bar plots, maps)

 📈 Key Results
- No statistically significant difference in BIN/species ratio between zones
- Notable sampling imbalance and geographic bias identified

 📂 Files
- `cryptic_diversity_analysis.R`: Main script

Figures
-Distribution of BIN:Species Ratio by Zone and Wilxocon tests
-Relative frequency of cryptic diversity by zone
-Sampling sites of tardigrades by zone

 🧠 Notes
- Zone classification is based on latitudinal thresholds via [meteoblue](https://content.meteoblue.com/...).
- Script heavily commented for reproducibility and understanding.
