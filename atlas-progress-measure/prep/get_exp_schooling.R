

###################################
### EXPECTED YEARS OF SCHOOLING ###
###################################

### USES RAW INPUT FILE ###

## 1. Calls expschool data
## 2. Calculates progress measures using trackr::track_progress()
## 3. Saves calculated measures to "intermediate" file


# Import packages & metadata
rm(list=ls())
library(tidyverse)
library(collapse)
library(readxl)
library(dplyr)
library(haven)
library(wbstats)

## Make sure you have the latest version of [trackr] installed ###
#devtools::install_github("RossanaTat/trackr")
library(trackr)


## Setting file paths ##
# setwd(".../atlas-progress-measure")
input_dir <- "input"
output_dir <- "intermediate"


### Set Indicator: Take this from "indicator" column ###
selected_indicator <- "HCI_EYRS"


### Loading in metasheet ###
meta <- read.csv(file.path(input_dir, "meta_sheet_new.csv")) |>
  collapse::fmutate(best = ifelse(more_is_better == 1, "high", "low"))

meta_indicator <- meta |>
  filter(indicator == selected_indicator)
target         <- meta_indicator$target_value
best           <- meta_indicator$best
indicatorname  <- meta_indicator$indicatorname
indicator_sdg  <- meta_indicator$indicator_sdg
startyear_data <- 1950
endyear_data   <- as.numeric(meta_indicator$end_prog_eval)


### Getting the data ###

data <- wbstats::wb_data(indicator = c(meta_indicator$col_name = meta_indicator$indicator),
                         country = "countries_only") |>
  rename("code" = "iso3c")

# save(data, file = file.path(output_dir, meta_indicator$raw_file_name))
load(file.path(input_dir, meta_indicator$raw_file_name))



############################
### Calculating Progress ###
############################

# Track progress
progress_results <- trackr::track_progress(
  data           = data,
  indicator      = meta_indicator$col_name,
  code_col       = "code",
  year_col       = "year",
  startyear_data = startyear_data,
  endyear_data   = endyear_data,
  eval_from      = meta_indicator$start_prog_eval,
  eval_to        = meta_indicator$end_prog_eval,
  future         = TRUE,
  target_year    = 2030,
  speed          = TRUE,
  percentiles    = TRUE,
  sequence_pctl  = seq(20, 80, 20),
  sequence_speed = c(0.25, 0.5, 1, 2, 4),
  best           = best,
  #  min            = meta_indicator$min,
  #  max            = meta_indicator$max,
  support        = meta_indicator$support,
  granularity    = meta_indicator$granularity
)






























