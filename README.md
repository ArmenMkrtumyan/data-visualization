
# Water Data Dashboard - README

## Overview
The **Water Data Dashboard** is an R Shiny application designed to visualize and analyze water quality data collected from various regions in Armenia. The dashboard provides insights into pollution trends, spatial distribution, and temporal patterns of water quality parameters like BOD5, COD, and NH4. It includes categorical, time series, and spatial analyses, with interactive visualizations to support policy-making and environmental conservation efforts.

---

## Features

### Tabs and Functionalities

1. **Introduction (Metadata)**
   - Dataset overview and explanation of water quality parameters.
   - Distribution of sampling by region with bar plots and maps.
   - Monthly sampling frequency visualization.

2. **Categorical Variables**
   - Boxplots of water quality parameters by region.
   - Heatmaps showing correlations among:
     - Nutrient parameters (e.g., NH4, NO3, PO4).
     - Metal concentrations (e.g., Zn, Cu, Pb).
     - General water quality parameters (e.g., TDS, EC).

3. **Time Series**
   - Interactive time series plots for parameters like Dissolved Oxygen, BOD5, COD, and NH4.
   - Monthly aggregated data visualization to analyze trends over time.

4. **Spatial Data**
   - Elevation data analysis using DEM (Digital Elevation Model).
   - Pollution Index visualization on an interactive map using `tmap`.

5. **PCA & Clustering Analysis**
   - Principal Component Analysis (PCA) for dimensionality reduction.
   - K-means clustering of sampling stations based on scaled water quality parameters.
   - Visualization of clusters and their regional distribution.

---

## Installation and Setup

### Prerequisites
1. **R**: Install the latest version of R from [CRAN](https://cran.r-project.org/).
2. **RStudio** (optional): Recommended for a better development experience.
3. **R Packages**: Install the following packages:
   ```R
   install.packages(c("shiny", "shinydashboard", "ggplot2", "dplyr", "sf", "terra", "tmap", "mapview", "plotly", "lubridate", "reshape2", "factoextra"))
   ```

### Data Preparation
1. Place the required datasets in a directory named `Data` within the app's root folder:
   - `data_imputed.csv`: Preprocessed dataset with water quality parameters.
   - `numeric_data.csv`: Numeric-only dataset for PCA and clustering.
   - `Armenia_Marzes.shp`: Shapefile for Armenia's regional boundaries.
   - `ArmDEM.tif`: Digital Elevation Model file for elevation data.

### Run the App
1. Save the app script as `app.R`.
2. In R or RStudio, set the working directory to the folder containing `app.R` and run:
   ```R
   shiny::runApp()
   ```

---

## Usage Instructions

### Interaction
- Navigate between tabs using the sidebar menu.
- Use dropdowns to select specific parameters for analysis.
- Hover over plots for detailed tooltips and data insights.

### Outputs
- **Plots**: Boxplots, heatmaps, time series, maps, and PCA biplots.
- **Tables**: Cluster-region mapping and PCA summaries.

---