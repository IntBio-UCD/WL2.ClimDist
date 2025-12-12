---
title: "Clim_dist_fitness_models"
author: "Brandie QC"
date: "2025-12-12"
output: 
  html_document: 
    keep_md: true
---



# Code for models relating climate distance to fitness (Table 2-3)

## Libraries

``` r
library(tidyverse)
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.1.4     ✔ readr     2.1.5
## ✔ forcats   1.0.0     ✔ stringr   1.5.1
## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
## ✔ purrr     1.0.2     
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

``` r
library(lmerTest) #mixed models
```

```
## Loading required package: lme4
## Loading required package: Matrix
## 
## Attaching package: 'Matrix'
## 
## The following objects are masked from 'package:tidyr':
## 
##     expand, pack, unpack
## 
## 
## Attaching package: 'lmerTest'
## 
## The following object is masked from 'package:lme4':
## 
##     lmer
## 
## The following object is masked from 'package:stats':
## 
##     step
```

``` r
library(broom.mixed) #tidy mixed model results 
library(tidymodels) #model workflows 
```

```
## ── Attaching packages ────────────────────────────────────── tidymodels 1.2.0 ──
## ✔ broom        1.0.7     ✔ rsample      1.2.1
## ✔ dials        1.3.0     ✔ tune         1.2.1
## ✔ infer        1.0.7     ✔ workflows    1.1.4
## ✔ modeldata    1.4.0     ✔ workflowsets 1.1.0
## ✔ parsnip      1.2.1     ✔ yardstick    1.3.1
## ✔ recipes      1.1.0     
## ── Conflicts ───────────────────────────────────────── tidymodels_conflicts() ──
## ✖ scales::discard() masks purrr::discard()
## ✖ Matrix::expand()  masks tidyr::expand()
## ✖ dplyr::filter()   masks stats::filter()
## ✖ recipes::fixed()  masks stringr::fixed()
## ✖ dplyr::lag()      masks stats::lag()
## ✖ Matrix::pack()    masks tidyr::pack()
## ✖ yardstick::spec() masks readr::spec()
## ✖ recipes::step()   masks lmerTest::step(), stats::step()
## ✖ Matrix::unpack()  masks tidyr::unpack()
## ✖ recipes::update() masks Matrix::update(), stats::update()
## • Search for functions across packages at https://www.tidymodels.org/find/
```

``` r
library(multilevelmod)
library(performance) #for calculating rsq for glmmerMod objects
```

```
## 
## Attaching package: 'performance'
## 
## The following objects are masked from 'package:yardstick':
## 
##     mae, rmse
```

## Read in Fitness Data

``` r
wl2_establishment <- read_csv("../Processed.Data/WL2_Establishment.csv")
```

```
## Rows: 1573 Columns: 54
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr   (8): block, BedLoc, bed, bed.col, Genotype, pop, bud.date, elevation.g...
## dbl  (45): bed.row, mf, rep, elev_m, Lat, Long, GD_Recent_Wtr_Year_2023, GD_...
## date  (1): death.date
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
first_yr_surv <- read_csv("../Processed.Data/WL2_Y1Surv.csv")
```

```
## Rows: 728 Columns: 55
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr   (8): block, BedLoc, bed, bed.col, Genotype, pop, bud.date, elevation.g...
## dbl  (46): bed.row, mf, rep, elev_m, Lat, Long, GD_Recent_Wtr_Year_2023, GD_...
## date  (1): death.date
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
winter_surv <- read_csv("../Processed.Data/WL2_WinterSurv.csv")
```

```
## Rows: 469 Columns: 51
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (7): block, BedLoc, bed, Genotype, pop, death.date, elevation.group
## dbl (44): mf, rep, elev_m, Lat, Long, GD_Recent_Wtr_Year_2023, GD_Historic_W...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
wl2_surv_to_bud_y2 <- read_csv("../Processed.Data/WL2_Surv_to_Rep_Y2.csv")
```

```
## Rows: 135 Columns: 49
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (5): pop, Genotype, block, death.date, elevation.group
## dbl (44): mf, rep, elev_m, Lat, Long, GD_Recent_Wtr_Year_2023, GD_Historic_W...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
wl2_fruits_y2 <- read_csv("../Processed.Data/WL2_Fruits_Y2.csv")
```

```
## Rows: 74 Columns: 50
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (4): pop, Genotype, block, elevation.group
## dbl (46): mf, rep, y2_flowers, y2_fruits, FrFlN_y2, elev_m, Lat, Long, GD_Re...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
wl2_prob_rep <- read_csv("../Processed.Data/WL2_ProbFit.csv") %>% 
  rename(AvgGD_Recent_Wtr_Year=`AvgGD_Recent_Water Year`,
         AvgGD_Recent_GrwSsn=`AvgGD_Recent_Growth Season`,
         AvgGD_Historic_Wtr_Year=`AvgGD_Historic_Water Year`,
         AvgGD_Historic_GrwSsn=`AvgGD_Historic_Growth Season`)
```

```
## Rows: 1573 Columns: 63
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (7): block, BedLoc, bed, bed.col, Genotype, pop, elevation.group
## dbl (56): bed.row, mf, rep, elev_m, Lat, Long, GD_Recent_Wtr_Year_2023, GD_H...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
wlt_tot_fitness <- read_csv("../Processed.Data/WL2_TotalRepOutput.csv") %>% 
  rename(AvgGD_Recent_Wtr_Year=`AvgGD_Recent_Water Year`,
         AvgGD_Recent_GrwSsn=`AvgGD_Recent_Growth Season`,
         AvgGD_Historic_Wtr_Year=`AvgGD_Historic_Water Year`,
         AvgGD_Historic_GrwSsn=`AvgGD_Historic_Growth Season`)
```

```
## Rows: 99 Columns: 52
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (7): block, BedLoc, bed, bed.col, Genotype, pop, elevation.group
## dbl (45): bed.row, mf, rep, elev_m, Lat, Long, GD_Recent_Wtr_Year_2023, GD_H...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

## Read in Size Pre-Transplant 

``` r
wl2_pretrans_size1 <- read_csv("../Raw.Data/WL2_DNA_Collection_Size_survey_combined20230703_corrected.csv")
```

```
## Rows: 1427 Columns: 7
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (2): Pop, Notes
## dbl (5): mf, rep, DNA, height (cm), longest leaf (cm)
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
wl2_pretrans_size2 <- read_csv("../Raw.Data/WL2_Extras_DNA_collection_size_survey_combined20230706_corrected.csv") %>% 
  filter(!is.na(`height (cm)`)) #to get rid of genotypes that were measured on the other data sheet (NAs on this sheet)
```

```
## Rows: 152 Columns: 7
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr (2): Pop, Notes
## dbl (5): mf, rep, DNA, height (cm), longest leaf (cm)
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

``` r
wl2_pretrans_size_all <- bind_rows(wl2_pretrans_size1, wl2_pretrans_size2) %>%
  rename(pop=Pop, height.cm = `height (cm)`, long.leaf.cm=`longest leaf (cm)`) %>% 
  unite(Genotype, pop:rep, sep="_", remove = FALSE) %>% 
  select(-DNA)
head(wl2_pretrans_size_all)
```

```
## # A tibble: 6 × 7
##   Genotype pop      mf   rep height.cm long.leaf.cm Notes
##   <chr>    <chr> <dbl> <dbl>     <dbl>        <dbl> <chr>
## 1 CP2_1_1  CP2       1     1       0.5          1.6 KN   
## 2 CP2_1_2  CP2       1     2       0.7          1.8 KN   
## 3 CP2_1_3  CP2       1     3       1.1          1.8 KN   
## 4 CP2_1_4  CP2       1     4       0.8          1.6 KN   
## 5 CP2_1_5  CP2       1     5       0.9          1.8 KN   
## 6 CP2_1_6  CP2       1     6       1            1.9 KN
```

## Scale Predictors, Data Transformations, and Add Pre-transplant size 

``` r
wl2_establishment_scaled <- wl2_establishment %>% 
  left_join(wl2_pretrans_size_all) %>% 
  mutate_at(c("GD_Recent_Wtr_Year_2023", "GD_Historic_Wtr_Year_2023",
              "GD_Recent_GrwSsn_2023", "GD_Historic_GrwSsn_2023",
              "TempDist_Historic_GrwSsn_2023", "TempDist_Historic_Wtr_Year_2023",
              "TempDist_Recent_GrwSsn_2023", "TempDist_Recent_Wtr_Year_2023",
              "PPTDist_Historic_GrwSsn_2023", "PPTDist_Historic_Wtr_Year_2023",
              "PPTDist_Recent_GrwSsn_2023", "PPTDist_Recent_Wtr_Year_2023",
              "Geographic_Dist"), scale)
```

```
## Joining with `by = join_by(Genotype, pop, mf, rep)`
```

``` r
wl2_y1_surv_scaled <- first_yr_surv %>% 
  left_join(wl2_pretrans_size_all) %>% 
  mutate_at(c("GD_Recent_Wtr_Year_2023", "GD_Historic_Wtr_Year_2023",
              "GD_Recent_GrwSsn_2023", "GD_Historic_GrwSsn_2023",
              "TempDist_Historic_GrwSsn_2023", "TempDist_Historic_Wtr_Year_2023",
              "TempDist_Recent_GrwSsn_2023", "TempDist_Recent_Wtr_Year_2023",
              "PPTDist_Historic_GrwSsn_2023", "PPTDist_Historic_Wtr_Year_2023",
              "PPTDist_Recent_GrwSsn_2023", "PPTDist_Recent_Wtr_Year_2023",
              "Geographic_Dist"), scale)
```

```
## Joining with `by = join_by(Genotype, pop, mf, rep)`
```

``` r
wl2_winter_surv_scaled <- winter_surv %>% 
  left_join(wl2_pretrans_size_all) %>% 
  filter(pop!="WR") %>% #remove pops with too low sample size 
  mutate_at(c("GD_Recent_Wtr_Year_2024", "GD_Historic_Wtr_Year_2024",
              "TempDist_Historic_Wtr_Year_2024","TempDist_Recent_Wtr_Year_2024",
              "PPTDist_Historic_Wtr_Year_2024", "PPTDist_Recent_Wtr_Year_2024",
              "Geographic_Dist"), scale)
```

```
## Joining with `by = join_by(Genotype, pop, mf, rep)`
```

``` r
wl2_surv_to_rep_y2_scaled <- wl2_surv_to_bud_y2 %>% 
  left_join(wl2_pretrans_size_all) %>% 
  filter(pop != "LV1", pop != "SQ1", pop != "WR") %>% #remove pops with too low sample size 
  mutate_at(c("GD_Recent_Wtr_Year_2024", "GD_Historic_Wtr_Year_2024",
              "GD_Recent_GrwSsn_2024", "GD_Historic_GrwSsn_2024",
              "TempDist_Historic_GrwSsn_2024", "TempDist_Historic_Wtr_Year_2024",
              "TempDist_Recent_GrwSsn_2024", "TempDist_Recent_Wtr_Year_2024",
              "PPTDist_Historic_GrwSsn_2024", "PPTDist_Historic_Wtr_Year_2024",
              "PPTDist_Recent_GrwSsn_2024", "PPTDist_Recent_Wtr_Year_2024",
              "Geographic_Dist"), scale)
```

```
## Joining with `by = join_by(pop, mf, rep, Genotype)`
```

``` r
wl2_fruits_y2_scaled <- wl2_fruits_y2 %>% 
  left_join(wl2_pretrans_size_all) %>% 
  filter(pop!="LV1", pop!="SQ1", pop!="WR") %>% #remove pops with too low sample size 
  mutate(logFruits=log(y2_fruits)) %>%  #transform fruit number with a log transformation 
  mutate_at(c("GD_Recent_Wtr_Year_2024", "GD_Historic_Wtr_Year_2024",
              "GD_Recent_GrwSsn_2024", "GD_Historic_GrwSsn_2024",
              "TempDist_Historic_GrwSsn_2024", "TempDist_Historic_Wtr_Year_2024",
              "TempDist_Recent_GrwSsn_2024", "TempDist_Recent_Wtr_Year_2024",
              "PPTDist_Historic_GrwSsn_2024", "PPTDist_Historic_Wtr_Year_2024",
              "PPTDist_Recent_GrwSsn_2024", "PPTDist_Recent_Wtr_Year_2024",
              "Geographic_Dist"), scale)
```

```
## Joining with `by = join_by(pop, mf, rep, Genotype)`
```

``` r
wl2_FrFlN_y2_scaled <- wl2_fruits_y2 %>% 
  left_join(wl2_pretrans_size_all) %>% 
  filter(pop!="LV1", pop!="SQ1", pop!="WR") %>% #remove pops with too low sample size 
  drop_na(FrFlN_y2) %>% 
  mutate(logFrFLs=log(FrFlN_y2)) %>% #transform fruit number with a log transformation 
  mutate_at(c("GD_Recent_Wtr_Year_2024", "GD_Historic_Wtr_Year_2024",
              "GD_Recent_GrwSsn_2024", "GD_Historic_GrwSsn_2024",
              "TempDist_Historic_GrwSsn_2024", "TempDist_Historic_Wtr_Year_2024",
              "TempDist_Recent_GrwSsn_2024", "TempDist_Recent_Wtr_Year_2024",
              "PPTDist_Historic_GrwSsn_2024", "PPTDist_Historic_Wtr_Year_2024",
              "PPTDist_Recent_GrwSsn_2024", "PPTDist_Recent_Wtr_Year_2024",
              "Geographic_Dist"), scale)
```

```
## Joining with `by = join_by(pop, mf, rep, Genotype)`
```

``` r
wl2_prob_fitness_scaled <- wl2_prob_rep %>% 
  left_join(wl2_pretrans_size_all) %>% 
  mutate_at(c("AvgGD_Historic_GrwSsn", "AvgGD_Historic_Wtr_Year",
              "AvgGD_Recent_GrwSsn", "AvgGD_Recent_Wtr_Year",
              "AvgTempDist_Historic_GrwSsn", "AvgTempDist_Historic_Wtr_Year",
              "AvgTempDist_Recent_GrwSsn", "AvgTempDist_Recent_Wtr_Year",
              "AvgPPTDist_Historic_GrwSsn", "AvgPPTDist_Historic_Wtr_Year",
              "AvgPPTDist_Recent_GrwSsn", "AvgPPTDist_Recent_Wtr_Year",
              "Geographic_Dist"), scale)
```

```
## Joining with `by = join_by(Genotype, pop, mf, rep)`
```

``` r
wl2_rep_output_scaled <- wlt_tot_fitness %>% 
  left_join(wl2_pretrans_size_all) %>% 
  mutate(logTotalFitness=log(Total_Fitness)) %>% 
  filter(pop!="SQ1", pop!="WR") %>% 
  mutate_at(c("AvgGD_Historic_GrwSsn", "AvgGD_Historic_Wtr_Year",
              "AvgGD_Recent_GrwSsn", "AvgGD_Recent_Wtr_Year",
              "AvgTempDist_Historic_GrwSsn", "AvgTempDist_Historic_Wtr_Year",
              "AvgTempDist_Recent_GrwSsn", "AvgTempDist_Recent_Wtr_Year",
              "AvgPPTDist_Historic_GrwSsn", "AvgPPTDist_Historic_Wtr_Year",
              "AvgPPTDist_Recent_GrwSsn", "AvgPPTDist_Recent_Wtr_Year",
              "Geographic_Dist"), scale)
```

```
## Joining with `by = join_by(Genotype, pop, mf, rep)`
```

## Set Model Engine

``` r
glmer.model_binomial <- 
  linear_reg() %>% 
  set_engine("glmer", family=binomial)
```

## Establishment Models

``` r
#basic model workflow 
surv_wflow <- workflow() %>% 
  add_variables(outcomes = Establishment, predictors = c(pop, mf, block))
surv_fits <- tibble(wflow=list(
  pop.block = {surv_wflow %>% 
      add_model(glmer.model_binomial, formula = Establishment ~ (1|pop) + (1|block))},
  
  pop.mf.block = {surv_wflow %>% 
      add_model(glmer.model_binomial, formula = Establishment ~ (1|pop/mf) + (1|block))}
),
name=names(wflow)
) %>% 
  select(name,wflow)
surv_fits_wl2 <- surv_fits %>%
  mutate(fit = map(wflow, fit, data = wl2_establishment_scaled))
```

```
## boundary (singular) fit: see help('isSingular')
```

``` r
surv_fits_wl2 %>% filter(name=="pop.mf.block") %>% pull(fit) #mf explains very little variation and get singularity warning
```

```
## $pop.mf.block
## ══ Workflow [trained] ══════════════════════════════════════════════════════════
## Preprocessor: Variables
## Model: linear_reg()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## Outcomes: Establishment
## Predictors: c(pop, mf, block)
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: Establishment ~ (1 | pop/mf) + (1 | block)
##    Data: data
##       AIC       BIC    logLik  deviance  df.resid 
##  2028.962  2050.405 -1010.481  2020.962      1569 
## Random effects:
##  Groups Name        Std.Dev. 
##  mf:pop (Intercept) 4.089e-05
##  pop    (Intercept) 5.171e-01
##  block  (Intercept) 6.339e-01
## Number of obs: 1573, groups:  mf:pop, 148; pop, 23; block, 13
## Fixed Effects:
## (Intercept)  
##     -0.2122  
## optimizer (Nelder_Mead) convergence code: 0 (OK) ; 0 optimizer warnings; 1 lme4 warnings
```

``` r
#Add in Geo and tmp/ppt dist
surv_GD_wflow_sub <- workflow() %>%
  add_variables(outcomes = Establishment, predictors = c(pop, block, height.cm, contains("Dist"))) 
surv_GD_fits_sub <- tibble(wflow=list(
  pop.block = {surv_GD_wflow_sub %>% 
      add_model(glmer.model_binomial, formula = Establishment ~ height.cm + (1|pop) + (1|block))},
  
  WY_Recent = {surv_GD_wflow_sub %>% 
      add_model(glmer.model_binomial, formula = Establishment ~ TempDist_Recent_Wtr_Year_2023 + PPTDist_Recent_Wtr_Year_2023 + Geographic_Dist + height.cm + (1|pop) + (1|block))},
  
  WY_Historical = {surv_GD_wflow_sub %>% 
      add_model(glmer.model_binomial, formula = Establishment ~ TempDist_Historic_Wtr_Year_2023 + PPTDist_Historic_Wtr_Year_2023 + Geographic_Dist + height.cm + (1|pop) + (1|block))},
  
  GS_Recent = {surv_GD_wflow_sub %>% 
      add_model(glmer.model_binomial, formula = Establishment ~ TempDist_Recent_GrwSsn_2023 + PPTDist_Recent_GrwSsn_2023 + Geographic_Dist + height.cm + (1|pop) + (1|block))},
  
  GS_Historical = {surv_GD_wflow_sub %>% 
      add_model(glmer.model_binomial, formula = Establishment ~ TempDist_Historic_GrwSsn_2023 + PPTDist_Historic_GrwSsn_2023 + Geographic_Dist + height.cm + (1|pop) + (1|block))}
),
name=names(wflow)
) %>% 
  select(name,wflow) 
surv_GD_fits_wl2_sub <- surv_GD_fits_sub %>%
  mutate(fit = map(wflow, fit, data = wl2_establishment_scaled))

##show results:
surv_GD_fits_wl2_sub %>% mutate(tidy=map(fit, tidy)) %>% unnest(tidy) %>%
  filter(str_detect(term, "Dist") | term=="height.cm") %>%
  drop_na(p.value) %>%
  select(-wflow:-group)
```

```
## # A tibble: 17 × 6
##    name          term                       estimate std.error statistic p.value
##    <chr>         <chr>                         <dbl>     <dbl>     <dbl>   <dbl>
##  1 pop.block     height.cm                    0.415     0.0764     5.43  5.66e-8
##  2 WY_Recent     TempDist_Recent_Wtr_Year_…   0.0708    0.0911     0.777 4.37e-1
##  3 WY_Recent     PPTDist_Recent_Wtr_Year_2…  -0.194     0.0751    -2.58  9.82e-3
##  4 WY_Recent     Geographic_Dist             -0.248     0.0645    -3.84  1.23e-4
##  5 WY_Recent     height.cm                    0.335     0.0789     4.24  2.23e-5
##  6 WY_Historical TempDist_Historic_Wtr_Yea…   0.0563    0.0941     0.598 5.50e-1
##  7 WY_Historical PPTDist_Historic_Wtr_Year…  -0.200     0.0784    -2.55  1.08e-2
##  8 WY_Historical Geographic_Dist             -0.237     0.0648    -3.66  2.54e-4
##  9 WY_Historical height.cm                    0.336     0.0788     4.26  2.05e-5
## 10 GS_Recent     TempDist_Recent_GrwSsn_20…   0.424     0.103      4.11  3.96e-5
## 11 GS_Recent     PPTDist_Recent_GrwSsn_2023  -0.383     0.108     -3.53  4.17e-4
## 12 GS_Recent     Geographic_Dist             -0.243     0.0597    -4.07  4.73e-5
## 13 GS_Recent     height.cm                    0.433     0.0754     5.74  9.51e-9
## 14 GS_Historical TempDist_Historic_GrwSsn_…   0.356     0.104      3.41  6.38e-4
## 15 GS_Historical PPTDist_Historic_GrwSsn_2…  -0.306     0.108     -2.83  4.63e-3
## 16 GS_Historical Geographic_Dist             -0.203     0.0652    -3.11  1.90e-3
## 17 GS_Historical height.cm                    0.428     0.0788     5.43  5.64e-8
```

``` r
surv_GD_fits_wl2_sub %>% mutate(extracted_fit=map(fit, extract_fit_parsnip)) %>% 
  mutate(rsq=map(extracted_fit, r2)) %>% 
  unnest(rsq) %>% 
  unnest(rsq) %>% 
  select(-wflow:-extracted_fit)
```

```
## # A tibble: 10 × 2
##    name             rsq
##    <chr>          <dbl>
##  1 pop.block     0.180 
##  2 pop.block     0.0415
##  3 WY_Recent     0.185 
##  4 WY_Recent     0.0779
##  5 WY_Historical 0.185 
##  6 WY_Historical 0.0776
##  7 GS_Recent     0.184 
##  8 GS_Recent     0.0808
##  9 GS_Historical 0.183 
## 10 GS_Historical 0.0767
```

``` r
#conditional rsq = first row per model - based on fixed and random effects 
#and marginal rsq is the second row per model - based only on fixed effects

#Add in Geo and Gowers clim dist
surv_GD_wflow <- workflow() %>%
  add_variables(outcomes = Establishment, predictors = c(pop, height.cm, block, contains("GD"), Geographic_Dist)) 
surv_GD_fits <- tibble(wflow=list(
  pop.block = {surv_GD_wflow %>% 
      add_model(glmer.model_binomial, formula = Establishment ~ height.cm + (1|pop) + (1|block))},
  
  WY_Recent = {surv_GD_wflow %>% 
      add_model(glmer.model_binomial, formula = Establishment ~ GD_Recent_Wtr_Year_2023 + Geographic_Dist + height.cm + (1|pop) + (1|block))},
  
  WY_Historical = {surv_GD_wflow %>% 
      add_model(glmer.model_binomial, formula = Establishment ~ GD_Historic_Wtr_Year_2023 + Geographic_Dist + height.cm + (1|pop) + (1|block))},
  
  GS_Recent = {surv_GD_wflow %>% 
      add_model(glmer.model_binomial, formula = Establishment ~ GD_Recent_GrwSsn_2023 + Geographic_Dist + height.cm + (1|pop) + (1|block))},
  
  GS_Historical = {surv_GD_wflow %>% 
      add_model(glmer.model_binomial, formula = Establishment ~ GD_Historic_GrwSsn_2023 + Geographic_Dist + height.cm + (1|pop) + (1|block))}
),
name=names(wflow)
) %>% 
  select(name,wflow) 
surv_GD_fits_wl2 <- surv_GD_fits %>%
  mutate(fit = map(wflow, fit, data = wl2_establishment_scaled))

##show results:
surv_GD_fits_wl2 %>% mutate(tidy=map(fit, tidy)) %>% unnest(tidy) %>%
  filter(str_detect(term, "GD") | term=="Geographic_Dist" | term=="height.cm") %>%
  drop_na(p.value) %>%
  select(-wflow:-group)
```

```
## # A tibble: 13 × 6
##    name          term                      estimate std.error statistic  p.value
##    <chr>         <chr>                        <dbl>     <dbl>     <dbl>    <dbl>
##  1 pop.block     height.cm                   0.415     0.0764     5.43  5.66e- 8
##  2 WY_Recent     GD_Recent_Wtr_Year_2023     0.128     0.0871     1.47  1.42e- 1
##  3 WY_Recent     Geographic_Dist            -0.266     0.0755    -3.52  4.27e- 4
##  4 WY_Recent     height.cm                   0.350     0.0777     4.51  6.64e- 6
##  5 WY_Historical GD_Historic_Wtr_Year_2023   0.0611    0.0832     0.734 4.63e- 1
##  6 WY_Historical Geographic_Dist            -0.252     0.0780    -3.23  1.24e- 3
##  7 WY_Historical height.cm                   0.385     0.0737     5.22  1.77e- 7
##  8 GS_Recent     GD_Recent_GrwSsn_2023      -0.255     0.0647    -3.94  8.25e- 5
##  9 GS_Recent     Geographic_Dist            -0.192     0.0637    -3.01  2.57e- 3
## 10 GS_Recent     height.cm                   0.450     0.0650     6.93  4.28e-12
## 11 GS_Historical GD_Historic_GrwSsn_2023    -0.276     0.0639    -4.31  1.60e- 5
## 12 GS_Historical Geographic_Dist            -0.183     0.0611    -2.99  2.75e- 3
## 13 GS_Historical height.cm                   0.392     0.0610     6.43  1.27e-10
```

``` r
#surv_GD_fits_wl2 %>% mutate(glance=map(fit, glance)) %>% unnest(glance) %>% 
 # select(-wflow:-sigma) 

surv_GD_fits_wl2 %>% mutate(extracted_fit=map(fit, extract_fit_parsnip)) %>% 
  mutate(rsq=map(extracted_fit, r2)) %>% 
  unnest(rsq) %>% 
  unnest(rsq) %>% 
  select(-wflow:-extracted_fit)
```

```
## # A tibble: 10 × 2
##    name             rsq
##    <chr>          <dbl>
##  1 pop.block     0.180 
##  2 pop.block     0.0415
##  3 WY_Recent     0.183 
##  4 WY_Recent     0.0666
##  5 WY_Historical 0.181 
##  6 WY_Historical 0.0615
##  7 GS_Recent     0.185 
##  8 GS_Recent     0.0788
##  9 GS_Historical 0.186 
## 10 GS_Historical 0.0828
```

``` r
#conditional rsq = first row per model - based on fixed and random effects 
#and marginal rsq is the second row per model - based only on fixed effects
```

## First Year Surv Models

``` r
#basic model workflow 
surv_wflow <- workflow() %>% 
  add_variables(outcomes = Y1Survival, predictors = c(pop, mf, block))
surv_fits <- tibble(wflow=list(
  pop.block = {surv_wflow %>% 
      add_model(glmer.model_binomial, formula = Y1Survival ~ (1|pop) + (1|block))},
  
  pop.mf.block = {surv_wflow %>% 
      add_model(glmer.model_binomial, formula = Y1Survival ~ (1|pop/mf) + (1|block))}
),
name=names(wflow)
) %>% 
  select(name,wflow)
surv_fits_wl2 <- surv_fits %>%
  mutate(fit = map(wflow, fit, data = wl2_y1_surv_scaled))
surv_fits_wl2 %>% filter(name=="pop.mf.block") %>% pull(fit) #no issues with mf 
```

```
## $pop.mf.block
## ══ Workflow [trained] ══════════════════════════════════════════════════════════
## Preprocessor: Variables
## Model: linear_reg()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## Outcomes: Y1Survival
## Predictors: c(pop, mf, block)
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: Y1Survival ~ (1 | pop/mf) + (1 | block)
##    Data: data
##       AIC       BIC    logLik  deviance  df.resid 
##  747.4865  765.8477 -369.7433  739.4865       724 
## Random effects:
##  Groups Name        Std.Dev.
##  mf:pop (Intercept) 0.07488 
##  pop    (Intercept) 1.46844 
##  block  (Intercept) 0.58592 
## Number of obs: 728, groups:  mf:pop, 137; pop, 22; block, 13
## Fixed Effects:
## (Intercept)  
##      0.3728
```

``` r
#Add in Geo and tmp/ppt dist
surv_GD_wflow_sub <- workflow() %>%
  add_variables(outcomes = Y1Survival, predictors = c(pop, mf, height.cm, block, contains("Dist"))) 
surv_GD_fits_sub <- tibble(wflow=list(
  pop.block = {surv_GD_wflow_sub %>% 
      add_model(glmer.model_binomial, formula = Y1Survival ~ height.cm + (1|pop/mf) + (1|block))},
  
  WY_Recent = {surv_GD_wflow_sub %>% 
      add_model(glmer.model_binomial, formula = Y1Survival ~ TempDist_Recent_Wtr_Year_2023 + PPTDist_Recent_Wtr_Year_2023 + Geographic_Dist + height.cm + (1|pop/mf) + (1|block))},
  
  WY_Historical = {surv_GD_wflow_sub %>% 
      add_model(glmer.model_binomial, formula = Y1Survival ~ TempDist_Historic_Wtr_Year_2023 + PPTDist_Historic_Wtr_Year_2023 + Geographic_Dist + height.cm + (1|pop/mf) + (1|block))},
  
  GS_Recent = {surv_GD_wflow_sub %>% 
      add_model(glmer.model_binomial, formula = Y1Survival ~ TempDist_Recent_GrwSsn_2023 + PPTDist_Recent_GrwSsn_2023 + Geographic_Dist + height.cm + (1|pop/mf) + (1|block))},
  
  GS_Historical = {surv_GD_wflow_sub %>% 
      add_model(glmer.model_binomial, formula = Y1Survival ~ TempDist_Historic_GrwSsn_2023 + PPTDist_Historic_GrwSsn_2023 + Geographic_Dist + height.cm + (1|pop/mf) + (1|block))}
),
name=names(wflow)
) %>% 
  select(name,wflow) 
surv_GD_fits_wl2_sub <- surv_GD_fits_sub %>%
  mutate(fit = map(wflow, fit, data = wl2_y1_surv_scaled))
```

```
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
```

``` r
##show results
surv_GD_fits_wl2_sub %>% mutate(tidy=map(fit, tidy)) %>% unnest(tidy) %>%
  filter(str_detect(term, "Dist") | term=="height.cm") %>%
  drop_na(p.value) %>%
  select(-wflow:-group)
```

```
## # A tibble: 17 × 6
##    name          term                       estimate std.error statistic p.value
##    <chr>         <chr>                         <dbl>     <dbl>     <dbl>   <dbl>
##  1 pop.block     height.cm                   -0.187      0.152   -1.23   2.19e-1
##  2 WY_Recent     TempDist_Recent_Wtr_Year_…   1.22       0.301    4.04   5.38e-5
##  3 WY_Recent     PPTDist_Recent_Wtr_Year_2…  -0.334      0.246   -1.36   1.75e-1
##  4 WY_Recent     Geographic_Dist             -0.0845     0.207   -0.407  6.84e-1
##  5 WY_Recent     height.cm                   -0.302      0.149   -2.02   4.36e-2
##  6 WY_Historical TempDist_Historic_Wtr_Yea…   1.21       0.312    3.87   1.10e-4
##  7 WY_Historical PPTDist_Historic_Wtr_Year…  -0.323      0.258   -1.25   2.11e-1
##  8 WY_Historical Geographic_Dist             -0.0432     0.208   -0.208  8.35e-1
##  9 WY_Historical height.cm                   -0.302      0.149   -2.02   4.29e-2
## 10 GS_Recent     TempDist_Recent_GrwSsn_20…   0.863      0.468    1.84   6.54e-2
## 11 GS_Recent     PPTDist_Recent_GrwSsn_2023   0.123      0.514    0.239  8.11e-1
## 12 GS_Recent     Geographic_Dist             -0.0312     0.271   -0.115  9.08e-1
## 13 GS_Recent     height.cm                   -0.252      0.152   -1.65   9.80e-2
## 14 GS_Historical TempDist_Historic_GrwSsn_…   0.756      0.421    1.79   7.28e-2
## 15 GS_Historical PPTDist_Historic_GrwSsn_2…   0.326      0.463    0.704  4.81e-1
## 16 GS_Historical Geographic_Dist              0.0243     0.267    0.0910 9.27e-1
## 17 GS_Historical height.cm                   -0.267      0.152   -1.75   7.99e-2
```

``` r
surv_GD_fits_wl2_sub %>% mutate(extracted_fit=map(fit, extract_fit_parsnip)) %>% 
  mutate(rsq=map(extracted_fit, r2)) %>% 
  unnest(rsq) %>% 
  unnest(rsq) %>% 
  select(-wflow:-extracted_fit)
```

```
## Random effect variances not available. Returned R2 does not account for random effects.
## Random effect variances not available. Returned R2 does not account for random effects.
## Random effect variances not available. Returned R2 does not account for random effects.
## Random effect variances not available. Returned R2 does not account for random effects.
## Random effect variances not available. Returned R2 does not account for random effects.
```

```
## Warning: There were 5 warnings in `mutate()`.
## The first warning was:
## ℹ In argument: `rsq = map(extracted_fit, r2)`.
## Caused by warning:
## ! Can't compute random effect variances. Some variance components equal
##   zero. Your model may suffer from singularity (see `?lme4::isSingular`
##   and `?performance::check_singularity`).
##   Decrease the `tolerance` level to force the calculation of random effect
##   variances, or impose priors on your random effects parameters (using
##   packages like `brms` or `glmmTMB`).
## ℹ Run `dplyr::last_dplyr_warnings()` to see the 4 remaining warnings.
```

```
## # A tibble: 10 × 2
##    name              rsq
##    <chr>           <dbl>
##  1 pop.block     NA     
##  2 pop.block      0.0115
##  3 WY_Recent     NA     
##  4 WY_Recent      0.319 
##  5 WY_Historical NA     
##  6 WY_Historical  0.319 
##  7 GS_Recent     NA     
##  8 GS_Recent      0.177 
##  9 GS_Historical NA     
## 10 GS_Historical  0.188
```

``` r
#Add in Geo and Gowers clim dist
surv_GD_wflow <- workflow() %>%
  add_variables(outcomes = Y1Survival, predictors = c(pop, mf, height.cm, block, contains("GD"), Geographic_Dist)) 
surv_GD_fits <- tibble(wflow=list(
  pop.block = {surv_GD_wflow %>% 
      add_model(glmer.model_binomial, formula = Y1Survival ~ height.cm + (1|pop/mf) + (1|block))},
  
  WY_Recent = {surv_GD_wflow %>% 
      add_model(glmer.model_binomial, formula = Y1Survival ~ GD_Recent_Wtr_Year_2023 + Geographic_Dist + height.cm + (1|pop/mf) + (1|block))},
  
  WY_Historical = {surv_GD_wflow %>% 
      add_model(glmer.model_binomial, formula = Y1Survival ~ GD_Historic_Wtr_Year_2023 + Geographic_Dist + height.cm + (1|pop/mf) + (1|block))},
  
  GS_Recent = {surv_GD_wflow %>% 
      add_model(glmer.model_binomial, formula = Y1Survival ~ GD_Recent_GrwSsn_2023 + Geographic_Dist + height.cm + (1|pop/mf) + (1|block))},
  
  GS_Historical = {surv_GD_wflow %>% 
      add_model(glmer.model_binomial, formula = Y1Survival ~ GD_Historic_GrwSsn_2023 + Geographic_Dist + height.cm + (1|pop/mf) + (1|block))}
),
name=names(wflow)
) %>% 
  select(name,wflow) 
surv_GD_fits_wl2 <- surv_GD_fits %>%
  mutate(fit = map(wflow, fit, data = wl2_y1_surv_scaled))
```

```
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
```

``` r
##show results 
surv_GD_fits_wl2 %>% mutate(tidy=map(fit, tidy)) %>% unnest(tidy) %>%
  filter(str_detect(term, "GD") | term=="Geographic_Dist" | term=="height.cm") %>%
  drop_na(p.value) %>%
  select(-wflow:-group)
```

```
## # A tibble: 13 × 6
##    name          term                      estimate std.error statistic  p.value
##    <chr>         <chr>                        <dbl>     <dbl>     <dbl>    <dbl>
##  1 pop.block     height.cm                   -0.187     0.152    -1.23  0.219   
##  2 WY_Recent     GD_Recent_Wtr_Year_2023      1.22      0.324     3.78  0.000160
##  3 WY_Recent     Geographic_Dist             -0.311     0.245    -1.27  0.205   
##  4 WY_Recent     height.cm                   -0.263     0.149    -1.76  0.0784  
##  5 WY_Historical GD_Historic_Wtr_Year_2023    0.745     0.347     2.15  0.0317  
##  6 WY_Historical Geographic_Dist             -0.249     0.287    -0.869 0.385   
##  7 WY_Historical height.cm                   -0.213     0.151    -1.41  0.158   
##  8 GS_Recent     GD_Recent_GrwSsn_2023       -0.409     0.326    -1.25  0.210   
##  9 GS_Recent     Geographic_Dist             -0.207     0.305    -0.680 0.496   
## 10 GS_Recent     height.cm                   -0.167     0.154    -1.09  0.277   
## 11 GS_Historical GD_Historic_GrwSsn_2023     -0.677     0.277    -2.44  0.0146  
## 12 GS_Historical Geographic_Dist             -0.110     0.283    -0.388 0.698   
## 13 GS_Historical height.cm                   -0.162     0.152    -1.07  0.286
```

``` r
surv_GD_fits_wl2 %>% mutate(extracted_fit=map(fit, extract_fit_parsnip)) %>% 
  mutate(rsq=map(extracted_fit, r2)) %>% 
  unnest(rsq) %>% 
  unnest(rsq) %>% 
  select(-wflow:-extracted_fit)
```

```
## Random effect variances not available. Returned R2 does not account for random effects.
## Random effect variances not available. Returned R2 does not account for random effects.
## Random effect variances not available. Returned R2 does not account for random effects.
## Random effect variances not available. Returned R2 does not account for random effects.
## Random effect variances not available. Returned R2 does not account for random effects.
```

```
## Warning: There were 5 warnings in `mutate()`.
## The first warning was:
## ℹ In argument: `rsq = map(extracted_fit, r2)`.
## Caused by warning:
## ! Can't compute random effect variances. Some variance components equal
##   zero. Your model may suffer from singularity (see `?lme4::isSingular`
##   and `?performance::check_singularity`).
##   Decrease the `tolerance` level to force the calculation of random effect
##   variances, or impose priors on your random effects parameters (using
##   packages like `brms` or `glmmTMB`).
## ℹ Run `dplyr::last_dplyr_warnings()` to see the 4 remaining warnings.
```

```
## # A tibble: 10 × 2
##    name              rsq
##    <chr>           <dbl>
##  1 pop.block     NA     
##  2 pop.block      0.0115
##  3 WY_Recent     NA     
##  4 WY_Recent      0.261 
##  5 WY_Historical NA     
##  6 WY_Historical  0.132 
##  7 GS_Recent     NA     
##  8 GS_Recent      0.0848
##  9 GS_Historical NA     
## 10 GS_Historical  0.136
```

## Winter Surv Models

``` r
#basic model workflow 
surv_wflow <- workflow() %>% 
  add_variables(outcomes = WinterSurv, predictors = c(pop, mf, block,  height.cm,))
surv_fits <- tibble(wflow=list(
  pop.block = {surv_wflow %>% 
      add_model(glmer.model_binomial, formula = WinterSurv ~ height.cm + (1|pop) + (1|block))},
  
  pop.mf.block = {surv_wflow %>% 
      add_model(glmer.model_binomial, formula = WinterSurv ~ height.cm + (1|pop/mf) + (1|block))}
),
name=names(wflow)
) %>% 
  select(name,wflow)
surv_fits_wl2 <- surv_fits %>%
  mutate(fit = map(wflow, fit, data = wl2_winter_surv_scaled))
surv_fits_wl2 %>% filter(name=="pop.mf.block") %>% pull(fit) #nothing wrong with the full model, so use that 
```

```
## $pop.mf.block
## ══ Workflow [trained] ══════════════════════════════════════════════════════════
## Preprocessor: Variables
## Model: linear_reg()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## Outcomes: WinterSurv
## Predictors: c(pop, mf, block, height.cm, )
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: WinterSurv ~ height.cm + (1 | pop/mf) + (1 | block)
##    Data: data
##       AIC       BIC    logLik  deviance  df.resid 
##  465.7083  486.4185 -227.8541  455.7083       460 
## Random effects:
##  Groups Name        Std.Dev.
##  mf:pop (Intercept) 0.4374  
##  pop    (Intercept) 2.1784  
##  block  (Intercept) 0.2453  
## Number of obs: 465, groups:  mf:pop, 111; pop, 21; block, 13
## Fixed Effects:
## (Intercept)    height.cm  
##     -3.4029       0.2524
```

``` r
#Add in Geo and tmp/ppt dist
surv_GD_wflow_wl2_sub <- workflow() %>%
  add_variables(outcomes = WinterSurv, predictors = c(pop, mf,  height.cm, block, contains("Dist"))) 
surv_GD_fits_wl2_sub <- tibble(wflow=list(
  pop.mf.block = {surv_GD_wflow_wl2_sub %>% 
      add_model(glmer.model_binomial, formula = WinterSurv ~ height.cm + (1|pop/mf) + (1|block))},
  
  WY_Recent = {surv_GD_wflow_wl2_sub %>% 
      add_model(glmer.model_binomial, formula = WinterSurv ~ TempDist_Recent_Wtr_Year_2024 + PPTDist_Recent_Wtr_Year_2024 + Geographic_Dist + height.cm + (1|pop/mf) + (1|block))},
  
  WY_Historical = {surv_GD_wflow_wl2_sub %>% 
      add_model(glmer.model_binomial, formula = WinterSurv ~ TempDist_Historic_Wtr_Year_2024 + PPTDist_Historic_Wtr_Year_2024 + Geographic_Dist + height.cm + (1|pop/mf) + (1|block))}
  
),
name=names(wflow)
) %>% 
  select(name,wflow) %>%
  mutate(fit = map(wflow, fit, data = wl2_winter_surv_scaled))

##show results
surv_GD_fits_wl2_sub %>% mutate(tidy=map(fit, tidy)) %>% unnest(tidy) %>%
  filter(str_detect(term, "Dist") | term=="height.cm") %>%
  drop_na(p.value) %>%
  select(-wflow:-group)
```

```
## # A tibble: 9 × 6
##   name          term                        estimate std.error statistic p.value
##   <chr>         <chr>                          <dbl>     <dbl>     <dbl>   <dbl>
## 1 pop.mf.block  height.cm                    0.252       0.185    1.36     0.173
## 2 WY_Recent     TempDist_Recent_Wtr_Year_2…  0.868       0.649    1.34     0.181
## 3 WY_Recent     PPTDist_Recent_Wtr_Year_20… -0.472       0.605   -0.780    0.436
## 4 WY_Recent     Geographic_Dist             -0.0415      0.449   -0.0924   0.926
## 5 WY_Recent     height.cm                    0.178       0.186    0.958    0.338
## 6 WY_Historical TempDist_Historic_Wtr_Year…  0.835       0.669    1.25     0.212
## 7 WY_Historical PPTDist_Historic_Wtr_Year_… -0.498       0.620   -0.803    0.422
## 8 WY_Historical Geographic_Dist              0.00642     0.438    0.0146   0.988
## 9 WY_Historical height.cm                    0.177       0.186    0.954    0.340
```

``` r
surv_GD_fits_wl2_sub %>% mutate(extracted_fit=map(fit, extract_fit_parsnip)) %>% 
  mutate(rsq=map(extracted_fit, r2)) %>% 
  unnest(rsq) %>% 
  unnest(rsq) %>% 
  select(-wflow:-extracted_fit)
```

```
## # A tibble: 6 × 2
##   name              rsq
##   <chr>           <dbl>
## 1 pop.mf.block  0.606  
## 2 pop.mf.block  0.00842
## 3 WY_Recent     0.600  
## 4 WY_Recent     0.202  
## 5 WY_Historical 0.599  
## 6 WY_Historical 0.208
```

``` r
#Add in Geo and Gowers clim dist
surv_GD_wflow_wl2 <- workflow() %>%
  add_variables(outcomes = WinterSurv, predictors = c(pop, mf, height.cm,  block, contains("GD"), Geographic_Dist)) 
surv_GD_fits_wl2 <- tibble(wflow=list(
  pop.mf.block = {surv_GD_wflow_wl2 %>% 
      add_model(glmer.model_binomial, formula = WinterSurv ~ height.cm + (1|pop/mf) + (1|block))},
  
  WY_Recent = {surv_GD_wflow_wl2 %>% 
      add_model(glmer.model_binomial, formula = WinterSurv ~ GD_Recent_Wtr_Year_2024 + Geographic_Dist + height.cm + (1|pop/mf) + (1|block))},
  
  WY_Historical = {surv_GD_wflow_wl2 %>% 
      add_model(glmer.model_binomial, formula = WinterSurv ~ GD_Historic_Wtr_Year_2024 + Geographic_Dist + height.cm + (1|pop/mf) + (1|block))}
  
),
name=names(wflow)
) %>% 
  select(name,wflow) %>%
  mutate(fit = map(wflow, fit, data = wl2_winter_surv_scaled))

##show results 
surv_GD_fits_wl2 %>% mutate(tidy=map(fit, tidy)) %>% unnest(tidy) %>%
  filter(str_detect(term, "GD") | term=="Geographic_Dist" | term=="height.cm") %>%
  drop_na(p.value) %>%
  select(-wflow:-group)
```

```
## # A tibble: 7 × 6
##   name          term                      estimate std.error statistic p.value
##   <chr>         <chr>                        <dbl>     <dbl>     <dbl>   <dbl>
## 1 pop.mf.block  height.cm                    0.252     0.185     1.36   0.173 
## 2 WY_Recent     GD_Recent_Wtr_Year_2024      1.32      0.545     2.43   0.0151
## 3 WY_Recent     Geographic_Dist             -0.234     0.426    -0.549  0.583 
## 4 WY_Recent     height.cm                    0.181     0.182     0.995  0.320 
## 5 WY_Historical GD_Historic_Wtr_Year_2024    1.06      0.521     2.03   0.0423
## 6 WY_Historical Geographic_Dist             -0.279     0.447    -0.623  0.533 
## 7 WY_Historical height.cm                    0.220     0.181     1.21   0.226
```

``` r
surv_GD_fits_wl2 %>% mutate(extracted_fit=map(fit, extract_fit_parsnip)) %>% 
  mutate(rsq=map(extracted_fit, r2)) %>% 
  unnest(rsq) %>% 
  unnest(rsq) %>% 
  select(-wflow:-extracted_fit)
```

```
## # A tibble: 6 × 2
##   name              rsq
##   <chr>           <dbl>
## 1 pop.mf.block  0.606  
## 2 pop.mf.block  0.00842
## 3 WY_Recent     0.599  
## 4 WY_Recent     0.235  
## 5 WY_Historical 0.583  
## 6 WY_Historical 0.161
```

## Surv to Buds Y2 Models

``` r
#basic model workflow 
surv_wflow <- workflow() %>% 
  add_variables(outcomes = SurvtoRep_y2, predictors = c(pop, mf, block))
surv_fits <- tibble(wflow=list(
  pop.block = {surv_wflow %>% 
      add_model(glmer.model_binomial, formula = SurvtoRep_y2 ~ (1|pop) + (1|block))},
  
  pop.mf.block = {surv_wflow %>% 
      add_model(glmer.model_binomial, formula = SurvtoRep_y2 ~ (1|pop/mf) + (1|block))}
),
name=names(wflow)
) %>% 
  select(name,wflow)
surv_fits_wl2 <- surv_fits %>%
  mutate(fit = map(wflow, fit, data = wl2_surv_to_rep_y2_scaled))
```

```
## Warning: There was 1 warning in `mutate()`.
## ℹ In argument: `fit = map(wflow, fit, data = wl2_surv_to_rep_y2_scaled)`.
## Caused by warning in `checkConv()`:
## ! Model failed to converge with max|grad| = 0.0344541 (tol = 0.002, component 1)
```

``` r
surv_fits_wl2 %>% filter(name=="pop.mf.block") %>% pull(fit) 
```

```
## $pop.mf.block
## ══ Workflow [trained] ══════════════════════════════════════════════════════════
## Preprocessor: Variables
## Model: linear_reg()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## Outcomes: SurvtoRep_y2
## Predictors: c(pop, mf, block)
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: SurvtoRep_y2 ~ (1 | pop/mf) + (1 | block)
##    Data: data
##      AIC      BIC   logLik deviance df.resid 
## 156.8142 168.3454 -74.4071 148.8142      128 
## Random effects:
##  Groups Name        Std.Dev.
##  mf:pop (Intercept) 0.6546  
##  block  (Intercept) 1.0356  
##  pop    (Intercept) 0.4310  
## Number of obs: 132, groups:  mf:pop, 41; block, 13; pop, 7
## Fixed Effects:
## (Intercept)  
##       1.236  
## optimizer (Nelder_Mead) convergence code: 0 (OK) ; 0 optimizer warnings; 1 lme4 warnings
```

``` r
#convergence issues with mf included 

#Add in Geo and tmp/ppt dist
surv_GD_wflow_wl2_sub <- workflow() %>%
  add_variables(outcomes = SurvtoRep_y2, predictors = c(pop, mf, height.cm,  block, contains("Dist"))) 
surv_GD_fits_wl2_sub <- tibble(wflow=list(
  pop.block = {surv_GD_wflow_wl2_sub %>% 
      add_model(glmer.model_binomial, formula = SurvtoRep_y2 ~ height.cm + (1|pop) + (1|block))},
  
  WY_Recent = {surv_GD_wflow_wl2_sub %>% 
      add_model(glmer.model_binomial, formula = SurvtoRep_y2 ~ TempDist_Recent_Wtr_Year_2024 + PPTDist_Recent_Wtr_Year_2024 + Geographic_Dist + height.cm + (1|pop) + (1|block))},
  
  WY_Historical = {surv_GD_wflow_wl2_sub %>% 
      add_model(glmer.model_binomial, formula = SurvtoRep_y2 ~ TempDist_Historic_Wtr_Year_2024 + PPTDist_Historic_Wtr_Year_2024 + Geographic_Dist + height.cm + (1|pop) + (1|block))},
  
  GS_Recent = {surv_GD_wflow_wl2_sub %>% 
      add_model(glmer.model_binomial, formula = SurvtoRep_y2 ~ TempDist_Recent_GrwSsn_2024 + PPTDist_Recent_GrwSsn_2024 + Geographic_Dist + height.cm + (1|pop) + (1|block))},
  
  GS_Historical = {surv_GD_wflow_wl2_sub %>% 
      add_model(glmer.model_binomial, formula = SurvtoRep_y2 ~ TempDist_Historic_GrwSsn_2024 + PPTDist_Historic_GrwSsn_2024 + Geographic_Dist + height.cm + (1|pop) + (1|block))}
),
name=names(wflow)
) %>% 
  select(name,wflow) %>%
  mutate(fit = map(wflow, fit, data = wl2_surv_to_rep_y2_scaled))
```

```
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
```

``` r
##show results 
surv_GD_fits_wl2_sub %>% mutate(tidy=map(fit, tidy)) %>% unnest(tidy) %>%
  filter(str_detect(term, "Dist") | term=="height.cm") %>%
  drop_na(p.value) %>%
  select(-wflow:-group)
```

```
## # A tibble: 17 × 6
##    name          term                       estimate std.error statistic p.value
##    <chr>         <chr>                         <dbl>     <dbl>     <dbl>   <dbl>
##  1 pop.block     height.cm                   -0.626      0.232   -2.70   0.00688
##  2 WY_Recent     TempDist_Recent_Wtr_Year_…   0.0147     0.448    0.0328 0.974  
##  3 WY_Recent     PPTDist_Recent_Wtr_Year_2…   0.516      0.386    1.34   0.181  
##  4 WY_Recent     Geographic_Dist              0.509      0.288    1.77   0.0773 
##  5 WY_Recent     height.cm                   -0.695      0.323   -2.15   0.0316 
##  6 WY_Historical TempDist_Historic_Wtr_Yea…   0.0686     0.470    0.146  0.884  
##  7 WY_Historical PPTDist_Historic_Wtr_Year…   0.539      0.399    1.35   0.176  
##  8 WY_Historical Geographic_Dist              0.487      0.282    1.73   0.0837 
##  9 WY_Historical height.cm                   -0.713      0.327   -2.18   0.0294 
## 10 GS_Recent     TempDist_Recent_GrwSsn_20…  -0.566      0.472   -1.20   0.230  
## 11 GS_Recent     PPTDist_Recent_GrwSsn_2024   0.509      0.473    1.08   0.282  
## 12 GS_Recent     Geographic_Dist              0.322      0.269    1.19   0.232  
## 13 GS_Recent     height.cm                   -0.601      0.341   -1.76   0.0781 
## 14 GS_Historical TempDist_Historic_GrwSsn_…  -0.593      0.483   -1.23   0.220  
## 15 GS_Historical PPTDist_Historic_GrwSsn_2…   0.529      0.477    1.11   0.267  
## 16 GS_Historical Geographic_Dist              0.289      0.262    1.10   0.271  
## 17 GS_Historical height.cm                   -0.621      0.338   -1.83   0.0666
```

``` r
surv_GD_fits_wl2_sub %>% mutate(extracted_fit=map(fit, extract_fit_parsnip)) %>% 
  mutate(rsq=map(extracted_fit, r2)) %>% 
  unnest(rsq) %>% 
  unnest(rsq) %>% 
  select(-wflow:-extracted_fit)
```

```
## Random effect variances not available. Returned R2 does not account for random effects.
## Random effect variances not available. Returned R2 does not account for random effects.
## Random effect variances not available. Returned R2 does not account for random effects.
## Random effect variances not available. Returned R2 does not account for random effects.
## Random effect variances not available. Returned R2 does not account for random effects.
```

```
## Warning: There were 5 warnings in `mutate()`.
## The first warning was:
## ℹ In argument: `rsq = map(extracted_fit, r2)`.
## Caused by warning:
## ! Can't compute random effect variances. Some variance components equal
##   zero. Your model may suffer from singularity (see `?lme4::isSingular`
##   and `?performance::check_singularity`).
##   Decrease the `tolerance` level to force the calculation of random effect
##   variances, or impose priors on your random effects parameters (using
##   packages like `brms` or `glmmTMB`).
## ℹ Run `dplyr::last_dplyr_warnings()` to see the 4 remaining warnings.
```

```
## # A tibble: 10 × 2
##    name             rsq
##    <chr>          <dbl>
##  1 pop.block     NA    
##  2 pop.block      0.112
##  3 WY_Recent     NA    
##  4 WY_Recent      0.187
##  5 WY_Historical NA    
##  6 WY_Historical  0.188
##  7 GS_Recent     NA    
##  8 GS_Recent      0.154
##  9 GS_Historical NA    
## 10 GS_Historical  0.158
```

``` r
#Add in Geo and Gowers clim dist
surv_GD_wflow_wl2 <- workflow() %>%
  add_variables(outcomes = SurvtoRep_y2, predictors = c(pop, mf, height.cm,  block, contains("GD"), Geographic_Dist)) 
surv_GD_fits_wl2 <- tibble(wflow=list(
  pop.block = {surv_GD_wflow_wl2 %>% 
      add_model(glmer.model_binomial, formula = SurvtoRep_y2 ~ height.cm + (1|pop) + (1|block))},
  
  WY_Recent = {surv_GD_wflow_wl2 %>% 
      add_model(glmer.model_binomial, formula = SurvtoRep_y2 ~ GD_Recent_Wtr_Year_2024 + Geographic_Dist + height.cm + (1|pop) + (1|block))},
  
  WY_Historical = {surv_GD_wflow_wl2 %>% 
      add_model(glmer.model_binomial, formula = SurvtoRep_y2 ~ GD_Historic_Wtr_Year_2024 + Geographic_Dist + height.cm + (1|pop) + (1|block))},
  
  GS_Recent = {surv_GD_wflow_wl2 %>% 
      add_model(glmer.model_binomial, formula = SurvtoRep_y2 ~ GD_Recent_GrwSsn_2024 + Geographic_Dist + height.cm + (1|pop) + (1|block))},
  
  GS_Historical = {surv_GD_wflow_wl2 %>% 
      add_model(glmer.model_binomial, formula = SurvtoRep_y2 ~ GD_Historic_GrwSsn_2024 + Geographic_Dist + height.cm + (1|pop) + (1|block))}
),
name=names(wflow)
) %>% 
  select(name,wflow) %>%
  mutate(fit = map(wflow, fit, data = wl2_surv_to_rep_y2_scaled))
```

```
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
```

``` r
##show results:
surv_GD_fits_wl2 %>% mutate(tidy=map(fit, tidy)) %>% unnest(tidy) %>%
  filter(str_detect(term, "GD") | term=="Geographic_Dist" | term=="height.cm") %>%
  drop_na(p.value) %>%
  select(-wflow:-group)
```

```
## # A tibble: 13 × 6
##    name          term                      estimate std.error statistic p.value
##    <chr>         <chr>                        <dbl>     <dbl>     <dbl>   <dbl>
##  1 pop.block     height.cm                   -0.626     0.232    -2.70  0.00688
##  2 WY_Recent     GD_Recent_Wtr_Year_2024     -0.495     0.294    -1.69  0.0919 
##  3 WY_Recent     Geographic_Dist              0.464     0.263     1.76  0.0783 
##  4 WY_Recent     height.cm                   -0.560     0.242    -2.32  0.0205 
##  5 WY_Historical GD_Historic_Wtr_Year_2024   -0.494     0.283    -1.75  0.0810 
##  6 WY_Historical Geographic_Dist              0.472     0.263     1.79  0.0731 
##  7 WY_Historical height.cm                   -0.611     0.241    -2.54  0.0111 
##  8 GS_Recent     GD_Recent_GrwSsn_2024       -0.182     0.283    -0.641 0.521  
##  9 GS_Recent     Geographic_Dist              0.264     0.233     1.13  0.257  
## 10 GS_Recent     height.cm                   -0.552     0.268    -2.06  0.0394 
## 11 GS_Historical GD_Historic_GrwSsn_2024     -0.117     0.245    -0.478 0.633  
## 12 GS_Historical Geographic_Dist              0.278     0.233     1.19  0.233  
## 13 GS_Historical height.cm                   -0.602     0.245    -2.46  0.0140
```

``` r
# recent water year = marginally sig, historical water year = sig, but historical water year had a singular boundary warning 

surv_GD_fits_wl2 %>% mutate(extracted_fit=map(fit, extract_fit_parsnip)) %>% 
  mutate(rsq=map(extracted_fit, r2)) %>% 
  unnest(rsq) %>% 
  unnest(rsq) %>% 
  select(-wflow:-extracted_fit)
```

```
## Random effect variances not available. Returned R2 does not account for random effects.
## Random effect variances not available. Returned R2 does not account for random effects.
## Random effect variances not available. Returned R2 does not account for random effects.
## Random effect variances not available. Returned R2 does not account for random effects.
## Random effect variances not available. Returned R2 does not account for random effects.
```

```
## Warning: There were 5 warnings in `mutate()`.
## The first warning was:
## ℹ In argument: `rsq = map(extracted_fit, r2)`.
## Caused by warning:
## ! Can't compute random effect variances. Some variance components equal
##   zero. Your model may suffer from singularity (see `?lme4::isSingular`
##   and `?performance::check_singularity`).
##   Decrease the `tolerance` level to force the calculation of random effect
##   variances, or impose priors on your random effects parameters (using
##   packages like `brms` or `glmmTMB`).
## ℹ Run `dplyr::last_dplyr_warnings()` to see the 4 remaining warnings.
```

```
## # A tibble: 10 × 2
##    name             rsq
##    <chr>          <dbl>
##  1 pop.block     NA    
##  2 pop.block      0.112
##  3 WY_Recent     NA    
##  4 WY_Recent      0.180
##  5 WY_Historical NA    
##  6 WY_Historical  0.183
##  7 GS_Recent     NA    
##  8 GS_Recent      0.134
##  9 GS_Historical NA    
## 10 GS_Historical  0.130
```

## Fruit N Y2 Models

``` r
#base models:
fruits_modelslog <- tribble(
  ~name,          ~f,
  "1_pop.block",        "logFruits ~ (1|pop) + (1|block)", 
  "2_pop.mf.block",     "logFruits ~  (1|pop/mf) + (1|block)"
)
#run the models 
fruits_modelslog <- fruits_modelslog %>%
  mutate(lmer = map(f, ~ lmer(as.formula(.), 
                            data = wl2_fruits_y2_scaled)), #run the models 
         glance = map(lmer, glance)) #glance at the model results
fruits_modelslog %>% mutate(tidy=map(lmer, tidy, effects = "ran_pars", scales = "vcov")) %>% unnest(tidy) %>% 
  select(name, group, estimate)
```

```
## # A tibble: 7 × 3
##   name           group    estimate
##   <chr>          <chr>       <dbl>
## 1 1_pop.block    block     0.426  
## 2 1_pop.block    pop       0.0215 
## 3 1_pop.block    Residual  0.718  
## 4 2_pop.mf.block mf:pop    0.121  
## 5 2_pop.mf.block block     0.413  
## 6 2_pop.mf.block pop       0.00859
## 7 2_pop.mf.block Residual  0.616
```

``` r
#Add in Geo and tmp/ppt dist
fruits_models_log_CD_GD_sub <- tribble(
  ~name,          ~f,
  "1_pop.block",      "logFruits ~  height.cm + (1|pop/mf) + (1|block)", 
  "2_WY_Recent",      "logFruits ~  TempDist_Recent_Wtr_Year_2024 + PPTDist_Recent_Wtr_Year_2024 + Geographic_Dist + height.cm + (1|pop/mf) + (1|block)",
  "3_WY_Historical",  "logFruits ~  TempDist_Historic_Wtr_Year_2024 + PPTDist_Historic_Wtr_Year_2024 + Geographic_Dist + height.cm + (1|pop/mf) + (1|block)",
  "4_GS_Recent",      "logFruits ~  TempDist_Recent_GrwSsn_2024 + PPTDist_Recent_GrwSsn_2024 + Geographic_Dist + height.cm + (1|pop/mf) + (1|block)", 
  "5_GS_Historical",  "logFruits ~  TempDist_Historic_GrwSsn_2024 + PPTDist_Historic_GrwSsn_2024 + Geographic_Dist + height.cm + (1|pop/mf) + (1|block)"
)
#run the models 
fruits_models_log_CD_GD_sub <- fruits_models_log_CD_GD_sub %>%
  mutate(lmer = map(f, ~ lmer(as.formula(.), 
                            data = wl2_fruits_y2_scaled)), #run the models 
         glance = map(lmer, glance)) #glance at the model results
```

```
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
```

``` r
#boundary (singular) fit: see help('isSingular') warning for all models with climate distance - pop explains 0 variance when you add clim distance 
##show results:
fruits_models_log_CD_GD_sub %>% mutate(tidy=map(lmer, tidy)) %>% unnest(tidy) %>%
  select(name, term:p.value) %>% 
  filter(str_detect(term, "Dist") | term=="height.cm") %>%
  drop_na(p.value)
```

```
## # A tibble: 17 × 7
##    name            term               estimate std.error statistic    df p.value
##    <chr>           <chr>                 <dbl>     <dbl>     <dbl> <dbl>   <dbl>
##  1 1_pop.block     height.cm            0.173      0.125     1.38   22.4  0.180 
##  2 2_WY_Recent     TempDist_Recent_W…  -0.371      0.218    -1.71   41.1  0.0957
##  3 2_WY_Recent     PPTDist_Recent_Wt…  -0.238      0.196    -1.21   32.3  0.234 
##  4 2_WY_Recent     Geographic_Dist      0.0981     0.145     0.679  28.9  0.502 
##  5 2_WY_Recent     height.cm            0.242      0.154     1.58   61.1  0.120 
##  6 3_WY_Historical TempDist_Historic…  -0.413      0.231    -1.79   41.2  0.0813
##  7 3_WY_Historical PPTDist_Historic_…  -0.271      0.205    -1.32   31.7  0.196 
##  8 3_WY_Historical Geographic_Dist      0.0949     0.141     0.675  28.3  0.505 
##  9 3_WY_Historical height.cm            0.253      0.155     1.64   61.4  0.106 
## 10 4_GS_Recent     TempDist_Recent_G…  -0.160      0.228    -0.700  18.2  0.493 
## 11 4_GS_Recent     PPTDist_Recent_Gr…  -0.200      0.226    -0.885  27.5  0.384 
## 12 4_GS_Recent     Geographic_Dist      0.0759     0.132     0.575  27.2  0.570 
## 13 4_GS_Recent     height.cm            0.292      0.158     1.84   61.2  0.0699
## 14 5_GS_Historical TempDist_Historic…  -0.139      0.228    -0.609  18.8  0.550 
## 15 5_GS_Historical PPTDist_Historic_…  -0.220      0.224    -0.979  27.2  0.336 
## 16 5_GS_Historical Geographic_Dist      0.0788     0.130     0.606  24.9  0.550 
## 17 5_GS_Historical height.cm            0.292      0.158     1.85   61.2  0.0685
```

``` r
fruits_models_log_CD_GD_sub %>% mutate(rsq=map(lmer, r2)) %>% 
  unnest(rsq) %>% 
  unnest(rsq) %>% 
  select(-f:-glance)
```

```
## Random effect variances not available. Returned R2 does not account for random effects.
## Random effect variances not available. Returned R2 does not account for random effects.
## Random effect variances not available. Returned R2 does not account for random effects.
## Random effect variances not available. Returned R2 does not account for random effects.
```

```
## Warning: There were 4 warnings in `mutate()`.
## The first warning was:
## ℹ In argument: `rsq = map(lmer, r2)`.
## Caused by warning:
## ! Can't compute random effect variances. Some variance components equal
##   zero. Your model may suffer from singularity (see `?lme4::isSingular`
##   and `?performance::check_singularity`).
##   Decrease the `tolerance` level to force the calculation of random effect
##   variances, or impose priors on your random effects parameters (using
##   packages like `brms` or `glmmTMB`).
## ℹ Run `dplyr::last_dplyr_warnings()` to see the 3 remaining warnings.
```

```
## # A tibble: 10 × 2
##    name                rsq
##    <chr>             <dbl>
##  1 1_pop.block      0.511 
##  2 1_pop.block      0.0254
##  3 2_WY_Recent     NA     
##  4 2_WY_Recent      0.123 
##  5 3_WY_Historical NA     
##  6 3_WY_Historical  0.128 
##  7 4_GS_Recent     NA     
##  8 4_GS_Recent      0.143 
##  9 5_GS_Historical NA     
## 10 5_GS_Historical  0.145
```

``` r
# Can't compute random effect variances. Some variance components equal zero. --> NA conditional rsq
# Your model may suffer from singularity (see `?lme4::isSingular` and `?performance::check_singularity`).
#  Decrease the `tolerance` level to force the calculation of random effect variances, or impose
#  priors on your random effects parameters (using packages like `brms` or `glmmTMB`).

#Add in Geo and Gowers clim dist
fruits_models_log_CD_GD <- tribble(
  ~name,          ~f,
  "1_pop.block",      "logFruits ~  height.cm + (1|pop/mf) + (1|block)", 
  "2_WY_Recent",      "logFruits ~  GD_Recent_Wtr_Year_2024 + Geographic_Dist + height.cm + (1|pop/mf) + (1|block)",
  "3_WY_Historical",  "logFruits ~  GD_Historic_Wtr_Year_2024 + Geographic_Dist + height.cm + (1|pop/mf) + (1|block)",
  "4_GS_Recent",      "logFruits ~  GD_Recent_GrwSsn_2024 + Geographic_Dist + height.cm + (1|pop/mf) + (1|block)", 
  "5_GS_Historical",  "logFruits ~  GD_Historic_GrwSsn_2024 + Geographic_Dist + height.cm + (1|pop/mf) + (1|block)"
)
#run the models 
fruits_models_log_CD_GD <- fruits_models_log_CD_GD %>%
  mutate(lmer = map(f, ~ lmer(as.formula(.), 
                            data = wl2_fruits_y2_scaled)), #run the models 
         glance = map(lmer, glance)) #glance at the model results
```

```
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
```

``` r
##show results:
fruits_models_log_CD_GD %>% mutate(tidy=map(lmer, tidy)) %>% unnest(tidy) %>%
  select(name, term:p.value) %>% 
  filter(str_detect(term, "GD") | term=="Geographic_Dist" | term=="height.cm") %>%
  drop_na(p.value)
```

```
## # A tibble: 13 × 7
##    name            term               estimate std.error statistic    df p.value
##    <chr>           <chr>                 <dbl>     <dbl>     <dbl> <dbl>   <dbl>
##  1 1_pop.block     height.cm            0.173      0.125     1.38  22.4   0.180 
##  2 2_WY_Recent     GD_Recent_Wtr_Yea…  -0.145      0.174    -0.831  1.92  0.497 
##  3 2_WY_Recent     Geographic_Dist      0.229      0.161     1.42   1.74  0.308 
##  4 2_WY_Recent     height.cm            0.152      0.131     1.16   9.49  0.273 
##  5 3_WY_Historical GD_Historic_Wtr_Y…  -0.110      0.175    -0.630  1.74  0.601 
##  6 3_WY_Historical Geographic_Dist      0.213      0.167     1.28   1.67  0.350 
##  7 3_WY_Historical height.cm            0.145      0.130     1.11   8.28  0.297 
##  8 4_GS_Recent     GD_Recent_GrwSsn_…  -0.178      0.137    -1.30  31.8   0.203 
##  9 4_GS_Recent     Geographic_Dist      0.219      0.118     1.85  16.7   0.0826
## 10 4_GS_Recent     height.cm            0.151      0.132     1.14  52.7   0.259 
## 11 5_GS_Historical GD_Historic_GrwSs…  -0.0817     0.122    -0.668 35.4   0.508 
## 12 5_GS_Historical Geographic_Dist      0.218      0.123     1.78  16.4   0.0935
## 13 5_GS_Historical height.cm            0.0870     0.120     0.725 46.1   0.472
```

``` r
fruits_models_log_CD_GD %>% mutate(rsq=map(lmer, r2)) %>% 
  unnest(rsq) %>% 
  unnest(rsq) %>% 
  select(-f:-glance)
```

```
## Random effect variances not available. Returned R2 does not account for random effects.
## Random effect variances not available. Returned R2 does not account for random effects.
```

```
## Warning: There were 2 warnings in `mutate()`.
## The first warning was:
## ℹ In argument: `rsq = map(lmer, r2)`.
## Caused by warning:
## ! Can't compute random effect variances. Some variance components equal
##   zero. Your model may suffer from singularity (see `?lme4::isSingular`
##   and `?performance::check_singularity`).
##   Decrease the `tolerance` level to force the calculation of random effect
##   variances, or impose priors on your random effects parameters (using
##   packages like `brms` or `glmmTMB`).
## ℹ Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.
```

```
## # A tibble: 10 × 2
##    name                rsq
##    <chr>             <dbl>
##  1 1_pop.block      0.511 
##  2 1_pop.block      0.0254
##  3 2_WY_Recent      0.523 
##  4 2_WY_Recent      0.0483
##  5 3_WY_Historical  0.522 
##  6 3_WY_Historical  0.0452
##  7 4_GS_Recent     NA     
##  8 4_GS_Recent      0.103 
##  9 5_GS_Historical NA     
## 10 5_GS_Historical  0.0795
```

``` r
# Can't compute random effect variances. Some variance components equal zero. --> NA conditional rsq
```

## Fruit + Flowers Y2

``` r
#base models:
FrFlN_modelslog <- tribble(
  ~name,          ~f,
  "1_pop.block",        "logFrFLs ~ (1|pop) + (1|block)", 
  "2_pop.mf.block",     "logFrFLs ~  (1|pop/mf) + (1|block)"
)
#run the models 
FrFlN_modelslog <- FrFlN_modelslog %>%
  mutate(lmer = map(f, ~ lmer(as.formula(.), 
                            data = wl2_FrFlN_y2_scaled)), #run the models 
         glance = map(lmer, glance)) #glance at the model results
FrFlN_modelslog %>% mutate(tidy=map(lmer, tidy, effects = "ran_pars", scales = "vcov")) %>% unnest(tidy) %>% 
  select(name, group, estimate)
```

```
## # A tibble: 7 × 3
##   name           group    estimate
##   <chr>          <chr>       <dbl>
## 1 1_pop.block    block      0.464 
## 2 1_pop.block    pop        0.0323
## 3 1_pop.block    Residual   0.646 
## 4 2_pop.mf.block mf:pop     0.0775
## 5 2_pop.mf.block block      0.453 
## 6 2_pop.mf.block pop        0.0237
## 7 2_pop.mf.block Residual   0.580
```

``` r
#Add in Geo and tmp/ppt dist
FrFlN_models_log_CD_GD_sub <- tribble(
  ~name,          ~f,
  "1_pop.block",      "logFrFLs ~  height.cm + (1|pop/mf) + (1|block)", 
  "2_WY_Recent",      "logFrFLs ~  TempDist_Recent_Wtr_Year_2024 + PPTDist_Recent_Wtr_Year_2024 + Geographic_Dist + height.cm + (1|pop/mf) + (1|block)",
  "3_WY_Historical",  "logFrFLs ~  TempDist_Historic_Wtr_Year_2024 + PPTDist_Historic_Wtr_Year_2024 + Geographic_Dist + height.cm + (1|pop/mf) + (1|block)",
  "4_GS_Recent",      "logFrFLs ~  TempDist_Recent_GrwSsn_2024 + PPTDist_Recent_GrwSsn_2024 + Geographic_Dist + height.cm + (1|pop/mf) + (1|block)", 
  "5_GS_Historical",  "logFrFLs ~  TempDist_Historic_GrwSsn_2024 + PPTDist_Historic_GrwSsn_2024 + Geographic_Dist + height.cm + (1|pop/mf) + (1|block)"
)
#run the models 
FrFlN_models_log_CD_GD_sub <- FrFlN_models_log_CD_GD_sub %>%
  mutate(lmer = map(f, ~ lmer(as.formula(.), 
                            data = wl2_FrFlN_y2_scaled)), #run the models 
         glance = map(lmer, glance)) #glance at the model results
```

```
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
```

``` r
#boundary (singular) fit: see help('isSingular') warning for all models with climate distance 
##show results:
FrFlN_models_log_CD_GD_sub %>% mutate(tidy=map(lmer, tidy)) %>% unnest(tidy) %>%
  select(name, term:p.value) %>% 
  filter(str_detect(term, "Dist") | term=="height.cm") %>%
  drop_na(p.value)
```

```
## # A tibble: 17 × 7
##    name            term               estimate std.error statistic    df p.value
##    <chr>           <chr>                 <dbl>     <dbl>     <dbl> <dbl>   <dbl>
##  1 1_pop.block     height.cm            0.157      0.120     1.31  24.3    0.203
##  2 2_WY_Recent     TempDist_Recent_W…  -0.258      0.224    -1.15   7.19   0.287
##  3 2_WY_Recent     PPTDist_Recent_Wt…  -0.108      0.208    -0.518  3.93   0.632
##  4 2_WY_Recent     Geographic_Dist      0.164      0.150     1.10   4.21   0.332
##  5 2_WY_Recent     height.cm            0.195      0.151     1.29  44.3    0.204
##  6 3_WY_Historical TempDist_Historic…  -0.290      0.234    -1.24   7.78   0.252
##  7 3_WY_Historical PPTDist_Historic_…  -0.141      0.212    -0.667  3.91   0.542
##  8 3_WY_Historical Geographic_Dist      0.157      0.142     1.11   4.30   0.327
##  9 3_WY_Historical height.cm            0.200      0.152     1.32  45.3    0.193
## 10 4_GS_Recent     TempDist_Recent_G…  -0.199      0.213    -0.934 18.2    0.363
## 11 4_GS_Recent     PPTDist_Recent_Gr…  -0.0987     0.216    -0.457 28.5    0.651
## 12 4_GS_Recent     Geographic_Dist      0.121      0.126     0.967 26.8    0.342
## 13 4_GS_Recent     height.cm            0.244      0.153     1.60  57.6    0.115
## 14 5_GS_Historical TempDist_Historic…  -0.158      0.213    -0.742 18.8    0.467
## 15 5_GS_Historical PPTDist_Historic_…  -0.139      0.213    -0.652 28.3    0.519
## 16 5_GS_Historical Geographic_Dist      0.117      0.124     0.950 24.3    0.352
## 17 5_GS_Historical height.cm            0.245      0.152     1.61  57.9    0.113
```

``` r
FrFlN_models_log_CD_GD_sub %>% mutate(rsq=map(lmer, r2)) %>% 
  unnest(rsq) %>% 
  unnest(rsq) %>% 
  select(-f:-glance)
```

```
## Random effect variances not available. Returned R2 does not account for random effects.
## Random effect variances not available. Returned R2 does not account for random effects.
```

```
## Warning: There were 2 warnings in `mutate()`.
## The first warning was:
## ℹ In argument: `rsq = map(lmer, r2)`.
## Caused by warning:
## ! Can't compute random effect variances. Some variance components equal
##   zero. Your model may suffer from singularity (see `?lme4::isSingular`
##   and `?performance::check_singularity`).
##   Decrease the `tolerance` level to force the calculation of random effect
##   variances, or impose priors on your random effects parameters (using
##   packages like `brms` or `glmmTMB`).
## ℹ Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.
```

```
## # A tibble: 10 × 2
##    name                rsq
##    <chr>             <dbl>
##  1 1_pop.block      0.523 
##  2 1_pop.block      0.0218
##  3 2_WY_Recent      0.530 
##  4 2_WY_Recent      0.0612
##  5 3_WY_Historical  0.527 
##  6 3_WY_Historical  0.0633
##  7 4_GS_Recent     NA     
##  8 4_GS_Recent      0.137 
##  9 5_GS_Historical NA     
## 10 5_GS_Historical  0.137
```

``` r
# Can't compute random effect variances. Some variance components equal zero. --> NA conditional rsq

#Add in Geo and Gowers clim dist
FrFlN_models_log_CD_GD <- tribble(
  ~name,          ~f,
  "1_pop.block",      "logFrFLs ~  height.cm + (1|pop/mf) + (1|block)", 
  "2_WY_Recent",      "logFrFLs ~  GD_Recent_Wtr_Year_2024 + Geographic_Dist + height.cm + (1|pop/mf) + (1|block)",
  "3_WY_Historical",  "logFrFLs ~  GD_Historic_Wtr_Year_2024 + Geographic_Dist + height.cm + (1|pop/mf) + (1|block)",
  "4_GS_Recent",      "logFrFLs ~  GD_Recent_GrwSsn_2024 + Geographic_Dist + height.cm + (1|pop/mf) + (1|block)", 
  "5_GS_Historical",  "logFrFLs ~  GD_Historic_GrwSsn_2024 + Geographic_Dist + height.cm + (1|pop/mf) + (1|block)"
)
#run the models 
FrFlN_models_log_CD_GD <- FrFlN_models_log_CD_GD %>%
  mutate(lmer = map(f, ~ lmer(as.formula(.), 
                            data = wl2_FrFlN_y2_scaled)), #run the models 
         glance = map(lmer, glance)) #glance at the model results
```

```
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
```

``` r
##show results:
FrFlN_models_log_CD_GD %>% mutate(tidy=map(lmer, tidy)) %>% unnest(tidy) %>%
  select(name, term:p.value) %>% 
  filter(str_detect(term, "GD") | term=="Geographic_Dist" | term=="height.cm") %>%
  drop_na(p.value)
```

```
## # A tibble: 13 × 7
##    name            term               estimate std.error statistic    df p.value
##    <chr>           <chr>                 <dbl>     <dbl>     <dbl> <dbl>   <dbl>
##  1 1_pop.block     height.cm            0.157      0.120     1.31  24.3   0.203 
##  2 2_WY_Recent     GD_Recent_Wtr_Yea…  -0.155      0.135    -1.15   2.18  0.362 
##  3 2_WY_Recent     Geographic_Dist      0.283      0.123     2.29   1.83  0.160 
##  4 2_WY_Recent     height.cm            0.0929     0.112     0.829  5.98  0.439 
##  5 3_WY_Historical GD_Historic_Wtr_Y…  -0.129      0.137    -0.940  2.03  0.445 
##  6 3_WY_Historical Geographic_Dist      0.272      0.129     2.10   1.80  0.185 
##  7 3_WY_Historical height.cm            0.0831     0.111     0.746  5.37  0.487 
##  8 4_GS_Recent     GD_Recent_GrwSsn_…  -0.164      0.129    -1.28  32.7   0.211 
##  9 4_GS_Recent     Geographic_Dist      0.236      0.110     2.14  17.2   0.0466
## 10 4_GS_Recent     height.cm            0.136      0.125     1.08  51.5   0.283 
## 11 5_GS_Historical GD_Historic_GrwSs…  -0.0869     0.116    -0.750 37.3   0.458 
## 12 5_GS_Historical Geographic_Dist      0.240      0.115     2.09  17.4   0.0517
## 13 5_GS_Historical height.cm            0.0808     0.114     0.711 46.6   0.481
```

``` r
FrFlN_models_log_CD_GD %>% mutate(rsq=map(lmer, r2)) %>% 
  unnest(rsq) %>% 
  unnest(rsq) %>% 
  select(-f:-glance)
```

```
## Random effect variances not available. Returned R2 does not account for random effects.
## Random effect variances not available. Returned R2 does not account for random effects.
```

```
## Warning: There were 2 warnings in `mutate()`.
## The first warning was:
## ℹ In argument: `rsq = map(lmer, r2)`.
## Caused by warning:
## ! Can't compute random effect variances. Some variance components equal
##   zero. Your model may suffer from singularity (see `?lme4::isSingular`
##   and `?performance::check_singularity`).
##   Decrease the `tolerance` level to force the calculation of random effect
##   variances, or impose priors on your random effects parameters (using
##   packages like `brms` or `glmmTMB`).
## ℹ Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.
```

```
## # A tibble: 10 × 2
##    name                rsq
##    <chr>             <dbl>
##  1 1_pop.block      0.523 
##  2 1_pop.block      0.0218
##  3 2_WY_Recent      0.532 
##  4 2_WY_Recent      0.0575
##  5 3_WY_Historical  0.530 
##  6 3_WY_Historical  0.0538
##  7 4_GS_Recent     NA     
##  8 4_GS_Recent      0.115 
##  9 5_GS_Historical NA     
## 10 5_GS_Historical  0.0974
```

``` r
# Can't compute random effect variances. Some variance components equal zero. --> NA conditional rsq
```

## Prob Fruits Models

``` r
#basic model workflow:
prob_fitness_wflow <- workflow() %>% 
  add_variables(outcomes = ProbFitness, predictors = c(pop, mf, block))
prob_fitness_fits <- tibble(wflow=list(
  
  pop.block = {prob_fitness_wflow %>% 
      add_model(glmer.model_binomial, formula = ProbFitness ~ (1|pop) + (1|block))},
  
  pop.mf.block = {prob_fitness_wflow %>% 
      add_model(glmer.model_binomial, formula = ProbFitness ~ (1|pop/mf) + (1|block))}
),
name=names(wflow)
) %>% 
  select(name,wflow)
prob_fitness_fits <- prob_fitness_fits %>%
  mutate(fit = map(wflow, fit, data = wl2_prob_fitness_scaled))
```

```
## Warning: There were 2 warnings in `mutate()`.
## The first warning was:
## ℹ In argument: `fit = map(wflow, fit, data = wl2_prob_fitness_scaled)`.
## Caused by warning in `checkConv()`:
## ! Model failed to converge with max|grad| = 0.0855417 (tol = 0.002, component 1)
## ℹ Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.
```

``` r
prob_fitness_fits %>% pull(fit) 
```

```
## $pop.block
## ══ Workflow [trained] ══════════════════════════════════════════════════════════
## Preprocessor: Variables
## Model: linear_reg()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## Outcomes: ProbFitness
## Predictors: c(pop, mf, block)
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: ProbFitness ~ (1 | pop) + (1 | block)
##    Data: data
##       AIC       BIC    logLik  deviance  df.resid 
##  574.0542  590.1364 -284.0271  568.0542      1570 
## Random effects:
##  Groups Name        Std.Dev.
##  pop    (Intercept) 3.1745  
##  block  (Intercept) 0.7552  
## Number of obs: 1573, groups:  pop, 23; block, 13
## Fixed Effects:
## (Intercept)  
##      -5.775  
## optimizer (Nelder_Mead) convergence code: 0 (OK) ; 0 optimizer warnings; 2 lme4 warnings 
## 
## $pop.mf.block
## ══ Workflow [trained] ══════════════════════════════════════════════════════════
## Preprocessor: Variables
## Model: linear_reg()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## Outcomes: ProbFitness
## Predictors: c(pop, mf, block)
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: ProbFitness ~ (1 | pop/mf) + (1 | block)
##    Data: data
##       AIC       BIC    logLik  deviance  df.resid 
##  574.7593  596.2022 -283.3796  566.7593      1569 
## Random effects:
##  Groups Name        Std.Dev.
##  mf:pop (Intercept) 0.4441  
##  pop    (Intercept) 3.1940  
##  block  (Intercept) 0.7705  
## Number of obs: 1573, groups:  mf:pop, 148; pop, 23; block, 13
## Fixed Effects:
## (Intercept)  
##      -5.842
```

``` r
#convergence issue with pop.block model 

#Add in geo and temp/ppt distance
prob_fitness_SUB_wflow_wl2 <- workflow() %>%
  add_variables(outcomes = ProbFitness, predictors = c(pop, mf, height.cm,  block, contains("Dist"))) 
prob_fitness_SUB_fits_wl2 <- tibble(wflow=list(
  pop.block = {prob_fitness_SUB_wflow_wl2 %>% 
      add_model(glmer.model_binomial, formula = ProbFitness ~ height.cm + (1|pop/mf) + (1|block))},
  
  WY_Recent = {prob_fitness_SUB_wflow_wl2 %>% 
      add_model(glmer.model_binomial, formula = ProbFitness ~ AvgTempDist_Recent_Wtr_Year + AvgPPTDist_Recent_Wtr_Year + Geographic_Dist + height.cm + (1|pop/mf) + (1|block))},
  
  WY_Historical = {prob_fitness_SUB_wflow_wl2 %>% 
      add_model(glmer.model_binomial, formula = ProbFitness ~ AvgTempDist_Historic_Wtr_Year + AvgPPTDist_Historic_Wtr_Year + Geographic_Dist + height.cm + (1|pop/mf) + (1|block))},
  
  GS_Recent = {prob_fitness_SUB_wflow_wl2 %>% 
      add_model(glmer.model_binomial, formula = ProbFitness ~ AvgTempDist_Recent_GrwSsn + AvgPPTDist_Recent_GrwSsn+ Geographic_Dist + height.cm + (1|pop/mf) + (1|block))},
  
  GS_Historical = {prob_fitness_SUB_wflow_wl2 %>% 
      add_model(glmer.model_binomial, formula = ProbFitness ~ AvgTempDist_Historic_GrwSsn + AvgPPTDist_Historic_GrwSsn + Geographic_Dist + height.cm + (1|pop/mf) + (1|block))}
),
name=names(wflow)
) %>% 
  select(name,wflow) %>%
  mutate(fit = map(wflow, fit, data = wl2_prob_fitness_scaled))

##show results:
prob_fitness_SUB_fits_wl2 %>% mutate(tidy=map(fit, tidy)) %>% unnest(tidy) %>%
  filter(str_detect(term, "Dist") | term=="height.cm") %>%
  drop_na(p.value) %>%
  select(-wflow:-group)
```

```
## # A tibble: 17 × 6
##    name          term                       estimate std.error statistic p.value
##    <chr>         <chr>                         <dbl>     <dbl>     <dbl>   <dbl>
##  1 pop.block     height.cm                    0.289      0.162     1.79   0.0740
##  2 WY_Recent     AvgTempDist_Recent_Wtr_Ye…   1.61       0.745     2.16   0.0309
##  3 WY_Recent     AvgPPTDist_Recent_Wtr_Year  -0.603      0.750    -0.804  0.421 
##  4 WY_Recent     Geographic_Dist             -0.307      0.583    -0.527  0.598 
##  5 WY_Recent     height.cm                    0.226      0.159     1.42   0.156 
##  6 WY_Historical AvgTempDist_Historic_Wtr_…   1.57       0.767     2.05   0.0401
##  7 WY_Historical AvgPPTDist_Historic_Wtr_Y…  -0.608      0.774    -0.786  0.432 
##  8 WY_Historical Geographic_Dist             -0.239      0.565    -0.423  0.672 
##  9 WY_Historical height.cm                    0.226      0.159     1.42   0.155 
## 10 GS_Recent     AvgTempDist_Recent_GrwSsn    1.28       1.22      1.05   0.293 
## 11 GS_Recent     AvgPPTDist_Recent_GrwSsn     0.257      1.15      0.224  0.823 
## 12 GS_Recent     Geographic_Dist             -0.169      0.649    -0.260  0.795 
## 13 GS_Recent     height.cm                    0.253      0.160     1.58   0.115 
## 14 GS_Historical AvgTempDist_Historic_GrwS…   0.922      1.10      0.841  0.400 
## 15 GS_Historical AvgPPTDist_Historic_GrwSsn   0.731      0.995     0.734  0.463 
## 16 GS_Historical Geographic_Dist             -0.0755     0.627    -0.120  0.904 
## 17 GS_Historical height.cm                    0.245      0.160     1.53   0.127
```

``` r
prob_fitness_SUB_fits_wl2 %>% pull(fit) #convergence issues with GS_Historical model 
```

```
## $pop.block
## ══ Workflow [trained] ══════════════════════════════════════════════════════════
## Preprocessor: Variables
## Model: linear_reg()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## Outcomes: ProbFitness
## Predictors: c(pop, mf, height.cm, block, contains("Dist"))
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: ProbFitness ~ height.cm + (1 | pop/mf) + (1 | block)
##    Data: data
##       AIC       BIC    logLik  deviance  df.resid 
##  572.9313  599.7127 -281.4657  562.9313      1561 
## Random effects:
##  Groups Name        Std.Dev.
##  mf:pop (Intercept) 0.3795  
##  pop    (Intercept) 2.8015  
##  block  (Intercept) 0.7730  
## Number of obs: 1566, groups:  mf:pop, 148; pop, 23; block, 13
## Fixed Effects:
## (Intercept)    height.cm  
##     -5.9119       0.2891  
## 
## $WY_Recent
## ══ Workflow [trained] ══════════════════════════════════════════════════════════
## Preprocessor: Variables
## Model: linear_reg()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## Outcomes: ProbFitness
## Predictors: c(pop, mf, height.cm, block, contains("Dist"))
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## ProbFitness ~ AvgTempDist_Recent_Wtr_Year + AvgPPTDist_Recent_Wtr_Year +  
##     Geographic_Dist + height.cm + (1 | pop/mf) + (1 | block)
##    Data: data
##       AIC       BIC    logLik  deviance  df.resid 
##  568.7175  611.5678 -276.3588  552.7175      1558 
## Random effects:
##  Groups Name        Std.Dev.
##  mf:pop (Intercept) 0.3787  
##  pop    (Intercept) 2.0190  
##  block  (Intercept) 0.7699  
## Number of obs: 1566, groups:  mf:pop, 148; pop, 23; block, 13
## Fixed Effects:
##                 (Intercept)  AvgTempDist_Recent_Wtr_Year  
##                     -5.6022                       1.6093  
##  AvgPPTDist_Recent_Wtr_Year              Geographic_Dist  
##                     -0.6029                      -0.3068  
##                   height.cm  
##                      0.2260  
## 
## $WY_Historical
## ══ Workflow [trained] ══════════════════════════════════════════════════════════
## Preprocessor: Variables
## Model: linear_reg()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## Outcomes: ProbFitness
## Predictors: c(pop, mf, height.cm, block, contains("Dist"))
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## ProbFitness ~ AvgTempDist_Historic_Wtr_Year + AvgPPTDist_Historic_Wtr_Year +  
##     Geographic_Dist + height.cm + (1 | pop/mf) + (1 | block)
##    Data: data
##       AIC       BIC    logLik  deviance  df.resid 
##  568.6611  611.5113 -276.3306  552.6611      1558 
## Random effects:
##  Groups Name        Std.Dev.
##  mf:pop (Intercept) 0.3785  
##  pop    (Intercept) 2.0034  
##  block  (Intercept) 0.7699  
## Number of obs: 1566, groups:  mf:pop, 148; pop, 23; block, 13
## Fixed Effects:
##                   (Intercept)  AvgTempDist_Historic_Wtr_Year  
##                       -5.5858                         1.5738  
##  AvgPPTDist_Historic_Wtr_Year                Geographic_Dist  
##                       -0.6083                        -0.2388  
##                     height.cm  
##                        0.2261  
## 
## $GS_Recent
## ══ Workflow [trained] ══════════════════════════════════════════════════════════
## Preprocessor: Variables
## Model: linear_reg()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## Outcomes: ProbFitness
## Predictors: c(pop, mf, height.cm, block, contains("Dist"))
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: ProbFitness ~ AvgTempDist_Recent_GrwSsn + AvgPPTDist_Recent_GrwSsn +  
##     Geographic_Dist + height.cm + (1 | pop/mf) + (1 | block)
##    Data: data
##       AIC       BIC    logLik  deviance  df.resid 
##  574.1577  617.0079 -279.0788  558.1577      1558 
## Random effects:
##  Groups Name        Std.Dev.
##  mf:pop (Intercept) 0.3864  
##  pop    (Intercept) 2.5511  
##  block  (Intercept) 0.7736  
## Number of obs: 1566, groups:  mf:pop, 148; pop, 23; block, 13
## Fixed Effects:
##               (Intercept)  AvgTempDist_Recent_GrwSsn  
##                   -5.9595                     1.2806  
##  AvgPPTDist_Recent_GrwSsn            Geographic_Dist  
##                    0.2566                    -0.1687  
##                 height.cm  
##                    0.2530  
## 
## $GS_Historical
## ══ Workflow [trained] ══════════════════════════════════════════════════════════
## Preprocessor: Variables
## Model: linear_reg()
## 
## ── Preprocessor ────────────────────────────────────────────────────────────────
## Outcomes: ProbFitness
## Predictors: c(pop, mf, height.cm, block, contains("Dist"))
## 
## ── Model ───────────────────────────────────────────────────────────────────────
## Generalized linear mixed model fit by maximum likelihood (Laplace
##   Approximation) [glmerMod]
##  Family: binomial  ( logit )
## Formula: 
## ProbFitness ~ AvgTempDist_Historic_GrwSsn + AvgPPTDist_Historic_GrwSsn +  
##     Geographic_Dist + height.cm + (1 | pop/mf) + (1 | block)
##    Data: data
##       AIC       BIC    logLik  deviance  df.resid 
##  573.5349  616.3852 -278.7675  557.5349      1558 
## Random effects:
##  Groups Name        Std.Dev.
##  mf:pop (Intercept) 0.3903  
##  pop    (Intercept) 2.4977  
##  block  (Intercept) 0.7735  
## Number of obs: 1566, groups:  mf:pop, 148; pop, 23; block, 13
## Fixed Effects:
##                 (Intercept)  AvgTempDist_Historic_GrwSsn  
##                    -5.89195                      0.92248  
##  AvgPPTDist_Historic_GrwSsn              Geographic_Dist  
##                     0.73067                     -0.07545  
##                   height.cm  
##                     0.24480
```

``` r
prob_fitness_SUB_fits_wl2 %>% mutate(extracted_fit=map(fit, extract_fit_parsnip)) %>% 
  mutate(rsq=map(extracted_fit, r2)) %>% 
  unnest(rsq) %>% 
  unnest(rsq) %>% 
  select(-wflow:-extracted_fit)
```

```
## # A tibble: 10 × 2
##    name              rsq
##    <chr>           <dbl>
##  1 pop.block     0.725  
##  2 pop.block     0.00677
##  3 WY_Recent     0.744  
##  4 WY_Recent     0.370  
##  5 WY_Historical 0.742  
##  6 WY_Historical 0.369  
##  7 GS_Recent     0.755  
##  8 GS_Recent     0.216  
##  9 GS_Historical 0.753  
## 10 GS_Historical 0.227
```

``` r
#Add in geo and Gowers clim distance
prob_fitness_GD_wflow_wl2 <- workflow() %>%
  add_variables(outcomes = ProbFitness, predictors = c(pop, mf, height.cm,  block, contains("GD"), Geographic_Dist)) 
prob_fitness_GD_fits_wl2 <- tibble(wflow=list(
  pop.block = {prob_fitness_GD_wflow_wl2 %>% 
      add_model(glmer.model_binomial, formula = ProbFitness ~ height.cm + (1|pop/mf) + (1|block))},
  
  WY_Recent = {prob_fitness_GD_wflow_wl2 %>% 
      add_model(glmer.model_binomial, formula = ProbFitness ~ AvgGD_Recent_Wtr_Year + Geographic_Dist + height.cm + (1|pop/mf) + (1|block))},
  
  WY_Historical = {prob_fitness_GD_wflow_wl2 %>% 
      add_model(glmer.model_binomial, formula = ProbFitness ~ AvgGD_Historic_Wtr_Year + Geographic_Dist + height.cm + (1|pop/mf) + (1|block))},
  
  GS_Recent = {prob_fitness_GD_wflow_wl2 %>% 
      add_model(glmer.model_binomial, formula = ProbFitness ~ AvgGD_Recent_GrwSsn + Geographic_Dist + height.cm + (1|pop/mf) + (1|block))},
  
  GS_Historical = {prob_fitness_GD_wflow_wl2 %>% 
      add_model(glmer.model_binomial, formula = ProbFitness ~ AvgGD_Historic_GrwSsn + Geographic_Dist + height.cm + (1|pop/mf) + (1|block))}
),
name=names(wflow)
) %>% 
  select(name,wflow) %>%
  mutate(fit = map(wflow, fit, data = wl2_prob_fitness_scaled))

##show results:
prob_fitness_GD_fits_wl2 %>% mutate(tidy=map(fit, tidy)) %>% unnest(tidy) %>%
  filter(str_detect(term, "GD") | term=="Geographic_Dist" | term=="height.cm") %>%
  drop_na(p.value) %>%
  select(-wflow:-group)
```

```
## # A tibble: 13 × 6
##    name          term                    estimate std.error statistic p.value
##    <chr>         <chr>                      <dbl>     <dbl>     <dbl>   <dbl>
##  1 pop.block     height.cm                 0.289      0.162    1.79    0.0740
##  2 WY_Recent     AvgGD_Recent_Wtr_Year     1.47       0.625    2.35    0.0188
##  3 WY_Recent     Geographic_Dist          -0.591      0.608   -0.972   0.331 
##  4 WY_Recent     height.cm                 0.247      0.160    1.54    0.122 
##  5 WY_Historical AvgGD_Historic_Wtr_Year   0.983      0.638    1.54    0.123 
##  6 WY_Historical Geographic_Dist          -0.577      0.649   -0.888   0.374 
##  7 WY_Historical height.cm                 0.272      0.161    1.69    0.0904
##  8 GS_Recent     AvgGD_Recent_GrwSsn       0.0369     0.710    0.0520  0.959 
##  9 GS_Recent     Geographic_Dist          -0.504      0.683   -0.738   0.460 
## 10 GS_Recent     height.cm                 0.285      0.162    1.76    0.0780
## 11 GS_Historical AvgGD_Historic_GrwSsn    -0.808      0.784   -1.03    0.302 
## 12 GS_Historical Geographic_Dist          -0.402      0.671   -0.600   0.549 
## 13 GS_Historical height.cm                 0.290      0.162    1.79    0.0732
```

``` r
prob_fitness_GD_fits_wl2 %>% mutate(extracted_fit=map(fit, extract_fit_parsnip)) %>% 
  mutate(rsq=map(extracted_fit, r2)) %>% 
  unnest(rsq) %>% 
  unnest(rsq) %>% 
  select(-wflow:-extracted_fit)
```

```
## # A tibble: 10 × 2
##    name              rsq
##    <chr>           <dbl>
##  1 pop.block     0.725  
##  2 pop.block     0.00677
##  3 WY_Recent     0.714  
##  4 WY_Recent     0.234  
##  5 WY_Historical 0.712  
##  6 WY_Historical 0.118  
##  7 GS_Recent     0.727  
##  8 GS_Recent     0.0303 
##  9 GS_Historical 0.736  
## 10 GS_Historical 0.0924
```

## Total Fruits Models 

``` r
#base models:
rep_output_modelslog <- tribble(
  ~name,          ~f,
  "1_pop.block",        "logTotalFitness ~ (1|pop) + (1|block)", 
  "2_pop.mf.block",     "logTotalFitness ~  (1|pop/mf) + (1|block)"
)
#run the models 
rep_output_modelslog <- rep_output_modelslog %>%
  mutate(lmer = map(f, ~ lmer(as.formula(.), 
                            data = wl2_rep_output_scaled)), #run the models 
         glance = map(lmer, glance)) #glance at the model results
```

```
## boundary (singular) fit: see help('isSingular')
```

``` r
rep_output_modelslog %>% mutate(tidy=map(lmer, tidy, effects = "ran_pars", scales = "vcov")) %>% unnest(tidy) %>% 
  select(name, group, estimate)
```

```
## # A tibble: 7 × 3
##   name           group    estimate
##   <chr>          <chr>       <dbl>
## 1 1_pop.block    block       0.307
## 2 1_pop.block    pop         0.153
## 3 1_pop.block    Residual    0.753
## 4 2_pop.mf.block mf:pop      0    
## 5 2_pop.mf.block block       0.307
## 6 2_pop.mf.block pop         0.153
## 7 2_pop.mf.block Residual    0.753
```

``` r
#Add in Geo and temp/ppt dist
rep_output_SUB_models_log_CD_GD <- tribble(
  ~name,          ~f,
  "1_pop.block",      "logTotalFitness ~  height.cm + (1|pop) + (1|block)", 
  "2_WY_Recent",      "logTotalFitness ~  AvgTempDist_Recent_Wtr_Year + AvgPPTDist_Recent_Wtr_Year + Geographic_Dist + height.cm + (1|pop) + (1|block)",
  "3_WY_Historical",  "logTotalFitness ~  AvgTempDist_Historic_Wtr_Year + AvgPPTDist_Historic_Wtr_Year + Geographic_Dist + height.cm + (1|pop) + (1|block)",
  "4_GS_Recent",      "logTotalFitness ~  AvgTempDist_Recent_GrwSsn + AvgPPTDist_Recent_GrwSsn + Geographic_Dist + height.cm + (1|pop) + (1|block)", 
  "5a_GS_Historical",  "logTotalFitness ~  AvgTempDist_Historic_GrwSsn + AvgPPTDist_Historic_GrwSsn + Geographic_Dist + height.cm + (1|pop) + (1|block)", 
  #Troubleshoot weird GrwSsn-AvgTemp_Dist results (positive coefficient, but figure indicates it should be neg...):
  "5b_GS_Historical",  "logTotalFitness ~  AvgTempDist_Historic_GrwSsn*AvgPPTDist_Historic_GrwSsn + Geographic_Dist + height.cm + (1|pop) + (1|block)", 
  "5c_GS_Historical",  "logTotalFitness ~  AvgTempDist_Historic_GrwSsn + Geographic_Dist + height.cm + (1|pop) + (1|block)", 
  "5d_GS_Historical",  "logTotalFitness ~  AvgPPTDist_Historic_GrwSsn + Geographic_Dist + height.cm + (1|pop) + (1|block)"
)
#run the models 
rep_output_SUB_models_log_CD_GD <- rep_output_SUB_models_log_CD_GD %>%
  mutate(lmer = map(f, ~ lmer(as.formula(.), 
                            data = wl2_rep_output_scaled)), #run the models 
         glance = map(lmer, glance)) #glance at the model results
```

```
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
## boundary (singular) fit: see help('isSingular')
```

``` r
#some singularity warnings b/c pop explains 0 var in some models 
rep_output_SUB_models_log_CD_GD %>% mutate(tidy=map(lmer, tidy, effects = "ran_pars", scales = "vcov")) %>% unnest(tidy) %>% 
  select(name, group, estimate)
```

```
## # A tibble: 24 × 3
##    name            group    estimate
##    <chr>           <chr>       <dbl>
##  1 1_pop.block     block     0.290  
##  2 1_pop.block     pop       0.233  
##  3 1_pop.block     Residual  0.740  
##  4 2_WY_Recent     block     0.250  
##  5 2_WY_Recent     pop       0.00141
##  6 2_WY_Recent     Residual  0.749  
##  7 3_WY_Historical block     0.252  
##  8 3_WY_Historical pop       0.00639
##  9 3_WY_Historical Residual  0.748  
## 10 4_GS_Recent     block     0.255  
## # ℹ 14 more rows
```

``` r
##show results:
rep_output_SUB_models_log_CD_GD %>% mutate(tidy=map(lmer, tidy)) %>% unnest(tidy) %>%
  select(name, term, estimate:p.value) %>% 
  filter(str_detect(term, "Dist") | term=="height.cm") %>%
  drop_na(p.value)
```

```
## # A tibble: 28 × 7
##    name            term               estimate std.error statistic    df p.value
##    <chr>           <chr>                 <dbl>     <dbl>     <dbl> <dbl>   <dbl>
##  1 1_pop.block     height.cm             0.137     0.113     1.21  61.6  0.230  
##  2 2_WY_Recent     AvgTempDist_Recen…   -0.622     0.148    -4.22   4.15 0.0125 
##  3 2_WY_Recent     AvgPPTDist_Recent…   -0.585     0.126    -4.64   1.84 0.0510 
##  4 2_WY_Recent     Geographic_Dist      -0.120     0.106    -1.13   3.22 0.336  
##  5 2_WY_Recent     height.cm             0.271     0.123     2.20  75.7  0.0308 
##  6 3_WY_Historical AvgTempDist_Histo…   -0.679     0.160    -4.23   4.79 0.00905
##  7 3_WY_Historical AvgPPTDist_Histor…   -0.607     0.138    -4.40   2.32 0.0364 
##  8 3_WY_Historical Geographic_Dist      -0.105     0.108    -0.972  3.95 0.387  
##  9 3_WY_Historical height.cm             0.275     0.124     2.22  82.0  0.0293 
## 10 4_GS_Recent     AvgTempDist_Recen…    0.259     0.146     1.78  88.7  0.0791 
## # ℹ 18 more rows
```

``` r
rep_output_SUB_models_log_CD_GD %>% mutate(rsq=map(lmer, r2)) %>% 
  unnest(rsq) %>% 
  unnest(rsq) %>% 
  select(-f:-glance)
```

```
## Random effect variances not available. Returned R2 does not account for random effects.
## Random effect variances not available. Returned R2 does not account for random effects.
## Random effect variances not available. Returned R2 does not account for random effects.
```

```
## Warning: There were 3 warnings in `mutate()`.
## The first warning was:
## ℹ In argument: `rsq = map(lmer, r2)`.
## Caused by warning:
## ! Can't compute random effect variances. Some variance components equal
##   zero. Your model may suffer from singularity (see `?lme4::isSingular`
##   and `?performance::check_singularity`).
##   Decrease the `tolerance` level to force the calculation of random effect
##   variances, or impose priors on your random effects parameters (using
##   packages like `brms` or `glmmTMB`).
## ℹ Run `dplyr::last_dplyr_warnings()` to see the 2 remaining warnings.
```

```
## # A tibble: 16 × 2
##    name                 rsq
##    <chr>              <dbl>
##  1 1_pop.block       0.425 
##  2 1_pop.block       0.0185
##  3 2_WY_Recent       0.391 
##  4 2_WY_Recent       0.186 
##  5 3_WY_Historical   0.392 
##  6 3_WY_Historical   0.182 
##  7 4_GS_Recent      NA     
##  8 4_GS_Recent       0.238 
##  9 5a_GS_Historical NA     
## 10 5a_GS_Historical  0.239 
## 11 5b_GS_Historical NA     
## 12 5b_GS_Historical  0.248 
## 13 5c_GS_Historical  0.459 
## 14 5c_GS_Historical  0.0370
## 15 5d_GS_Historical  0.399 
## 16 5d_GS_Historical  0.140
```

``` r
# Can't compute random effect variances. Some variance components equal zero. --> NA conditional rsq

#Add in Geo and Gowers clim dist
rep_output_models_log_CD_GD <- tribble(
  ~name,          ~f,
  "1_pop.block",      "logTotalFitness ~  height.cm + (1|pop) + (1|block)", 
  "2_WY_Recent",      "logTotalFitness ~  AvgGD_Recent_Wtr_Year + Geographic_Dist + height.cm + (1|pop) + (1|block)",
  "3_WY_Historical",  "logTotalFitness ~  AvgGD_Historic_Wtr_Year + Geographic_Dist + height.cm + (1|pop) + (1|block)",
  "4_GS_Recent",      "logTotalFitness ~  AvgGD_Recent_GrwSsn + Geographic_Dist + height.cm + (1|pop) + (1|block)", 
  "5_GS_Historical",  "logTotalFitness ~  AvgGD_Historic_GrwSsn + Geographic_Dist + height.cm + (1|pop) + (1|block)"
)
#run the models 
rep_output_models_log_CD_GD <- rep_output_models_log_CD_GD %>%
  mutate(lmer = map(f, ~ lmer(as.formula(.), 
                            data = wl2_rep_output_scaled)), #run the models 
         glance = map(lmer, glance)) #glance at the model results
##show results:
rep_output_models_log_CD_GD %>% mutate(tidy=map(lmer, tidy)) %>% unnest(tidy) %>%
  select(name, term:p.value) %>% 
  filter(str_detect(term, "GD") | term=="Geographic_Dist" | term=="height.cm") %>%
  drop_na(p.value)
```

```
## # A tibble: 13 × 7
##    name            term               estimate std.error statistic    df p.value
##    <chr>           <chr>                 <dbl>     <dbl>     <dbl> <dbl>   <dbl>
##  1 1_pop.block     height.cm           1.37e-1     0.113   1.21    61.6   0.230 
##  2 2_WY_Recent     AvgGD_Recent_Wtr_… -4.78e-2     0.269  -0.178    3.98  0.868 
##  3 2_WY_Recent     Geographic_Dist    -6.84e-4     0.261  -0.00262  4.02  0.998 
##  4 2_WY_Recent     height.cm           1.76e-1     0.121   1.45    79.0   0.151 
##  5 3_WY_Historical AvgGD_Historic_Wt…  1.99e-2     0.257   0.0773   3.87  0.942 
##  6 3_WY_Historical Geographic_Dist    -3.44e-2     0.254  -0.135    4.04  0.899 
##  7 3_WY_Historical height.cm           1.72e-1     0.121   1.43    75.9   0.157 
##  8 4_GS_Recent     AvgGD_Recent_GrwS… -4.96e-1     0.163  -3.03     5.86  0.0237
##  9 4_GS_Recent     Geographic_Dist     1.30e-1     0.136   0.954    4.70  0.386 
## 10 4_GS_Recent     height.cm           2.04e-1     0.119   1.72    73.1   0.0903
## 11 5_GS_Historical AvgGD_Historic_Gr… -4.63e-1     0.167  -2.78     3.71  0.0543
## 12 5_GS_Historical Geographic_Dist     1.20e-1     0.142   0.844    4.11  0.445 
## 13 5_GS_Historical height.cm           1.60e-1     0.114   1.41    47.3   0.166
```

``` r
rep_output_models_log_CD_GD %>% mutate(rsq=map(lmer, r2)) %>% 
  unnest(rsq) %>% 
  unnest(rsq) %>% 
  select(-f:-glance)
```

```
## # A tibble: 10 × 2
##    name               rsq
##    <chr>            <dbl>
##  1 1_pop.block     0.425 
##  2 1_pop.block     0.0185
##  3 2_WY_Recent     0.487 
##  4 2_WY_Recent     0.0244
##  5 3_WY_Historical 0.486 
##  6 3_WY_Historical 0.0252
##  7 4_GS_Recent     0.403 
##  8 4_GS_Recent     0.123 
##  9 5_GS_Historical 0.413 
## 10 5_GS_Historical 0.127
```
