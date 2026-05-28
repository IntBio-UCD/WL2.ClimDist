# Code and data for reproducing, "Adaptational lag at high elevations depends on life stage in a California wildflower."

Authors: Brandie Quarles-Chidyagwai, Sarah Ashlock, Johanna Schmitt, Julin N. Maloof, Jennifer R. Gremer

Accepted for Publication in <i>Journal of Ecology<i/> 

## Corresponding author contact:
-  bmquarles@ucdavis.edu

## Software version and package information
This code has been verified to run on R version 4.5.3. To run the code, you will need packages: raster v. 3.6-32, tidyverse v. 2.0.0, ggrepel v. 0.9.8, corrplot 0.95, vegan v. 2.7-3, ggfortify v. 0.4.19, viridis v. 0.6.5, elevatr v. 0.99.1, terra v. 1.9-11, sf v. 1.1-0, giscoR v. 1.1.0, marmap v. 1.0.12, boot v. 1.3-32, broom v. 1.0.12, geosphere v. 1.6-8, lmerTest v. 3.2-1, corrplot v. 0.95, broom.mixed v. 0.2.9.7, tidymodels v. 1.4.1, multilevelmod v. 1.0.0, performance v. 0.16.0, glmmTMB v. 1.1.14, bbmle v. 1.0.25.1, ggpubr v. 0.6.3, zoo v. 1.8-15.

## The Files
Folders in this repository:
- Raw.Data
- Processed.Data
- Scripts
- Figures

### Raw.Data
This folder includes all data collected and used for analyses in this study. This data is also archived in Dryad. 

### Processed.Data
This folder includes all data files output by the analysis scripts. This data is also archived in Dryad. 

### Figures
This folder includes all figures output by the anlysis scripts, including the figures in the manuscript. 

### Scripts 
This folder includes all scripts used for the anlyses described in the manuscript. See below for the order to use the scripts. 

1. Clim_vars_all_sites_years_timeframes.Rmd
2. Clim_corrs_pcas.Rmd
3. Clim_dist_calcs.Rmd
4. Clim_dist_fitness_prep.Rmd
5. Clim_dist_fitness_models.Rmd
6. Total_Fitness.Rmd
7. Clim_dist_fitness_figs.Rmd
8. WklyClim_Mort.Rmd
