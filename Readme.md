[![Live Preview](https://img.shields.io/badge/Live-Preview-blue)](https://htmlpreview.github.io/?https://github.com/jokobus/nyc_bnb/NYC_bnb.html)

# New York City BnB Pricing Optimization & Predictive Modeling

An end-to-end data science and predictive analytics pipeline evaluated across New York County short-term rental listings. This project uncovers spatial, structural, and behavioral indicators driving market valuation and implements high-precision machine learning workflows to forecast rental pricing structures.

The entire analysis, documentation, and modeling infrastructure is self-contained within a production-grade Quarto Document (`.qmd`) utilizing aggressive performance optimization frameworks.

---

## Key Highlights & Architectural Features

* **Geospatial Intelligence Layer:** Intersects coordinate-level data with micro-market behaviors using `ggplot2` isolation layers and an interactive `leaflet` engine tracking major critical points of interest (POIs).
* **Production-Grade Processing Optimization:** Leverages `doParallel` and `makePSOCKcluster` to distribute heavy kNN data imputation and ensemble grid-searches across multi-core CPU architectures.
* **Robust Caching System:** Implements programmatic local caching (`.cache/*.rds`) across compute-heavy transformations to prevent redundant retraining loops and maximize environment execution efficiency.
* **Advanced Model Exploration & Segmentation:** Moves beyond baseline linear models into ANOVA-driven decision trees, 15-fold cross-validation grid searches, and a **Spatially Segmented K-Means Cluster Ensemble Framework**.

---

## Performance Matrix & Model Selection

Five distinct algorithms were built, cross-validated, and verified against an out-of-sample testing partition ($70/30$ Split). The **Random Forest Ensemble** architecture achieved the highest predictive precision.

| Model Variant | Strategy / Topology | Testing Set RMSE | Status |
| --- | --- | --- | --- |
| **Linear Regression** | Ordinary Least Squares Baseline | $116.4534$ | Benchmark |
| **Standard CART Tree** | ANOVA-Based Splitting Matrix | $113.1646$ | Deprecated |
| **CV-Tuned Tree** | 15-Fold Grid Search Optimization ($\alpha = \text{cp}$) | $102.6055$ | Baseline |
| **Random Forest** | Multi-Tree Bootstrap Aggregation ($N=150$) | **88.1119** | **Selected Production Model** |
| **K-Means Divided RF** | 3-Center Spatial Partitioning + Regional RF | $89.4412$ | Experimental |

---

## Repository Blueprint & Dependencies

```bash
├── bnb_pricing_pipeline.qmd   # Core analytics, engine architecture, & visualizations
├── bnb_data.csv               # Historical training dataset
├── bnb_data_eval.csv          # Out-of-sample evaluation target instances
└── .cache/                    # Model & data caching layer for performance optimization
```

To reproduce this pipeline, open your R environment and ensure the following libraries are installed:

```r
install.packages(c("dplyr", "ggplot2", "leaflet", "randomForest", 
                   "caret", "e1071", "rpart", "rpart.plot", 
                   "doParallel", "caTools", "flexclust"))
```

---

## Core Technical Discoveries & Engineering Solutions

### 1. The Manhattan Truncation Ceiling Anomaly

During exploratory validation, a diagnostic check isolated exactly 67 listings capped at precisely **$999/night**, with zero entries recording above it. 89.5% of these listings sat inside Manhattan, signaling an upper-bound truncation data artifact. Identifying this prevented structural bias from bleeding into the downstream estimators.

### 2. Spatially Aware Imputation Verification

To treat systemic missingness (`bedrooms`, `beds`), parallelized kNN pathways were benchmarked with and without spatial parameters. Cross-Imputation validation yielded an RMSE variance delta of $<0.09$, proving that coordinate features did not inject geographic bias into geometric structural traits.

```r
# Minimal variance proving structural feature stability during spatial kNN processing
Beds Imputation Delta Variance RMSE: 0.0512
Bedrooms Imputation Delta Variance RMSE: 0.0934
```

### 3. Predictive Production Deployment

The final architecture combines historical samples, structures, and timestamps to automatically output out-of-sample estimations against unseen evaluation tables (`bnb_data_eval.csv`).

---

## Execution

To compile the comprehensive dashboard, interactive geographical maps, and output data results, render the Quarto document from your command terminal:

**Option 1:** Generate the static final HTML portfolio files locally
```bash
quarto render bnb_pricing_pipeline.qmd
```
**Option 2:** Spin up a local live development server to explore the code interactively on http://localhost:5946/
```bash
quarto preview bnb_pricing_pipeline.qmd --no-browser --no-watch-inputs
```