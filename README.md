                                                                              #####Latitudinal Patterns in Cryptic Species Diversity of Tardigrades Using BIN-Based Global Assessments#####

**Description**

This project examines cryptic species diversity in tardigrades across polar and temperate regions using BIN/species ratios derived from global DNA barcode records. The workflow uses R for data preprocessing, spatial classification, statistical testing, and visualization of latitudinal biodiversity patterns.

**Workflow Overview**


    1. Data Cleaning and Spatial Processing (R Script)

Import and clean raw tardigrade occurrence data.

Process latitude and longitude coordinates.

Classify sampling sites into polar and temperate zones based on meteoblue-defined latitudinal thresholds.

    2. Cryptic Diversity Assessment (R Script)

Calculate BIN/species ratios at the site level.

Perform Wilcoxon rank-sum test to compare ratios between zones.

Generate visualizations: ridge plots, bar plots, and geographic maps of sampling locations.

**Datasets Used**

tardigrade_data.tsv — Tab-separated dataset containing species observations and coordinates.
(Source: BOLD Systems)

**Packages Used**

R Environment

tidyverse – Data wrangling and plotting

ggridges – Ridge plot visualization

viridis – Color palettes

assertthat – Data validation

maps – Geographic plotting

**Key Results**

No statistically significant difference in BIN/species ratio between polar and temperate zones.

Sampling bias and uneven geographic representation observed across regions.

Visual outputs include:

Distribution_BIN_species_ratio_by_zone.png — Ridge plot showing BIN/species ratio distributions

Relative_frequency_cryptic_diversity_by_zone.png — Bar plot of relative cryptic diversity

Tardigrade_sampling_sites_map.png — Map of sampling site locations by zone

**Files in This Repository**

cryptic_diversity_analysis.R – Main analysis script for data processing, statistical testing, and visualization.

**Important Notes**

Zone classification is based on latitudinal cutoffs defined by meteoblue.

Analysis highlights data gaps and sampling biases that may influence cryptic diversity estimates.

Script includes extensive commenting for transparency and reproducibility.
