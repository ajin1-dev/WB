### Full indicator list:
## From WDI (Atlas stories)
# 1 Gender: SL.TLF.ACTI.FE.ZS
# 2 Prosperity: SI.SPR.PGAP
# 3 Electricity: EG.ELC.ACCS.ZS
# 4 Internet: IT.NET.USER.ZS
# 5 SPI: IQ.SPI.OVRL

## Direct downloads (Atlas stories)
# 6 Education: HCI_EYRS
# 7 Water: wat_imp_prem_t
# 8 Water: wat_imp_qual_t
# 9 Water: wat_imp_av_t

## Direct downloads (Progress paper indicators)
# 10 Poverty: SI.POV.DDAY
# 11 Electricity: ELEC_SUP_PC
# 12 Gender: WOMEN.INDEX
# 13 Health: lifeexpectancy
# 14 Climate: carbon_intensity

## No progress indicators
# Climate
# Urban development
# Overall progress
# Artificial intelligence

### test <- wbstats::wb_data(indicator = "HCI_EYRS", lang = "en", country = "countries_only")


### Indicators in the dashboard
# 1. Exp years of schooling: education.Rda
# 2. FLFP: SL.TLF.ACTI.FE.ZS
# 3. Life expectancy: lifeexpectancy.Rda
# 4. Women pol. empowerment index: gender.Rda
# 5. Extreme pov rate: povert.Rda
# 6. Prosperity Gap: SI.SPR.PGAP
# 7. Carbon intensity: ghggdp.Rda
# 8. Access to water on premises: JMP_2025_WLD.xlsx "wat_imp_prem_t"
# 9. Access to water contamination free: JMP_2025_WLD.xlsx "wat_imp_qual_t"
# 10. Access to readily available water services: JMP_2025_WLD.xlsx "wat_imp_av_t"
# 11. Access to electricity: EG.ELC.ACCS.ZS
# 12. Electricity Supply: electricity.Rda
# 13. Individuals using the internet: IT.NET.USER.ZS
# 14. SPI: IQ.SPI.OVRL



rm(list = ls())


## set wd to the folder that you download this folder that Indicator_Progress_Paths folder is downloaded into
setwd("C:/Users/wb661549/OneDrive - WBG/Desktop/Internship/WB")
input_dir <- "Indicator_Progress_Paths/input"
output_dir <- "Indicator_Progress_Paths/Outputs"

library(tidyverse)
library(trackr)
library(readxl)
library(wbstats)
library(ggrepel)

## Getting parameters for the indicators
meta <- read.csv(file.path(input_dir, "meta_sheet.csv")) |>
  collapse::fmutate(best = ifelse(more_is_better == 1, "high", "low"))
meta_new <- read.csv(file.path(input_dir, "meta_sheet_new.csv")) |>
  collapse::fmutate(best = ifelse(more_is_better == 1, "high", "low"))


#########################################################
### Calculations for data downloaded from from WB API ###
#########################################################


## Setting up list of indicators with data accessible through WB API
indicator_list <- meta_new |>
  filter(wbstats_access == 1,
         in_dashboard == 1) |>
  pull(dataname)


indicator_data <- wb_data(indicator = indicator_list,
                          country = "countries only")

## Creates empty dataframes to put paths into
indicator_paths <- data.frame(time = numeric())
future_paths <- data.frame(code = factor(), year = numeric(), speed = numeric())

## Getting Data, Creating typical path, then plotting and saving plot
for (tracked_indicator in indicator_list) {
  data <- indicator_data |>
    select(iso3c, date, tracked_indicator)
  metadata <- meta_new |> 
    filter(dataname == tracked_indicator)
  print(tracked_indicator)
  progress <- track_progress(
    data = data,
    indicator = tracked_indicator,
    code_col = "iso3c",
    year_col = "date",
    startyear_data = min(data$date),
    endyear_data = max(data$date),
    eval_from = metadata$start_prog_eval,
    eval_to = metadata$end_prog_eval,
    speed = TRUE,
    percentiles = FALSE,
    future = TRUE, ## TRUE,
    target_year = 2030,
    sequence_pctl = seq(20, 80, 20),
    sequence_speed = c(0.25, 0.5, 1, 2, 4),
    best = metadata$best,
    support = metadata$support,
    granularity = metadata$granularity
  )
  
  typical_path <- progress$predicted_changes$path_speed |>
    rename(!!tracked_indicator := y)
  country_future_path <- progress$path_future$speed |>
    select(-speed_source) |>
    rename(!!tracked_indicator := y_fut)
  
  ### Creating Typical path plot
  typical_path_plot <- typical_path |>
    ggplot(aes(x = time, y = !!sym(tracked_indicator))) +
    geom_line() +
    labs(title = metadata$indicatorname)
  print(typical_path_plot)
  ggsave(paste0(output_dir, "/", tracked_indicator, "_plot.png"),
         plot = typical_path_plot)
  
  ### Combining with indicator_paths dataframe
  indicator_paths <- indicator_paths |> full_join(typical_path, by = "time")
  future_paths <- future_paths |> full_join(country_future_path, by = c("code", "year", "speed"))
}




#################################################
### Getting water data from JMP_2025_WLD.xlsx ###
#################################################


water <- read_excel(file.path(input_dir, "JMP_2025_WLD.xlsx"), sheet = "wat") |>
  select(iso3,
         year,
         wat_sm_t,
         wat_imp_prem_t,
         wat_imp_qual_t,
         wat_imp_av_t) |>
  rename("iso3c" = iso3, "date" = year)

## Getting Data, Creating typical path, then plotting and saving plot
for (tracked_indicator in c("wat_sm_t",
                            "wat_imp_prem_t",
                            "wat_imp_qual_t",
                            "wat_imp_av_t")) {
  data <- water |> select(iso3c, date, !!tracked_indicator)
  metadata <- meta |> filter(indicator == tracked_indicator)
  progress <- track_progress(
    data = data,
    indicator = tracked_indicator,
    code_col = "iso3c",
    year_col = "date",
    startyear_data = min(data$date),
    endyear_data = max(data$date),
    eval_from = (max(data$date) - 5),
    eval_to = max(data$date),
    speed = TRUE,
    percentiles = FALSE,
    future = TRUE,
    ## TRUE,
    target_year = 2030,
    sequence_pctl = seq(20, 80, 20),
    sequence_speed = c(0.25, 0.5, 1, 2, 4),
    best = "high",
    support = 1,
    granularity = 0.1
  )
  print(tracked_indicator)
  
  typical_path <- progress$predicted_changes$path_speed |>
    rename(!!tracked_indicator := y)
  country_future_paths <- progress$path_future$speed |>
    select(-speed_source) |>
    rename(!!tracked_indicator := y_fut)
  
  ### Creating Typical path plot
  typical_path_plot <- typical_path |>
    ggplot(aes(x = time, y = !!sym(tracked_indicator))) +
    geom_line() +
    labs(title = tracked_indicator)
  print(typical_path_plot)
  ggsave(paste0(output_dir, "/", tracked_indicator, "_plot.png"),
         plot = typical_path_plot)
  
  ### Combining with indicator_paths dataframe
  indicator_paths <- indicator_paths |> full_join(typical_path, by = "time")
  future_paths <- future_paths |> full_join(country_future_paths, by = c("code", "year", "speed"))
}





#######################################################
### List for indicators with data in the input file ###
#######################################################

## Creating list of all the files
data_list <- c("electricity",
               "ghggdp",
               "lifeexpectancy",
               "gender",
               "education")

## Loading parameters
load(file.path(input_dir, "parameters.Rda"))


for (tracked_indicator in data_list) {
  
  load(paste0(input_dir, "/", tracked_indicator, ".Rda"))
  ## Removes any duplicates from multiple sources
  data <- get(tracked_indicator) |>
    rename("value" = all_of(tracked_indicator)) |>
    filter(!is.na(value)) |>
    pivot_wider(names_from = source,
              values_from = value) |>
    mutate(value = ifelse(is.na(`1`), `2`, `1`))
  
  print(tracked_indicator)

  ## Calculates the future paths
  metadata <- meta_new |> filter(dataname == tracked_indicator)
  progress <- track_progress(
    data = data,
    indicator = "value",
    code_col = "code",
    year_col = "year",
    startyear_data = min(data$year),
    endyear_data = max(data$year),
    eval_from = 2015,
    eval_to = 2023,
    speed = TRUE,
    percentiles = FALSE,
    future = TRUE,
    target_year = 2030,
    sequence_pctl = seq(20, 80, 20),
    sequence_speed = c(0.25, 0.5, 1, 2, 4),
    best = metadata$best,
    support = 1,
    granularity = 0.1,
    verbose = TRUE)
  
  typical_path <- progress$predicted_changes$path_speed |>
    rename(!!tracked_indicator := y)
  country_future_paths <- progress$path_future$speed |>
    select(-speed_source) |>
    rename(!!tracked_indicator := y_fut)
  
  ### Creating Typical path plot
  typical_path_plot <- typical_path |>
    ggplot(aes(x = time, y = !!sym(tracked_indicator))) +
    geom_line() +
    labs(title = tracked_indicator)
  print(typical_path_plot)
  ggsave(paste0(output_dir, "/", tracked_indicator, "_plot.png"),
         plot = typical_path_plot)
  
  ### Combining with indicator_paths dataframe
  indicator_paths <- indicator_paths |> full_join(typical_path, by = "time")
  future_paths <- future_paths |> full_join(country_future_paths, by = c("code", "year", "speed"))
}



##################################################################
### Converting to long format and saving the combined datasets ###
##################################################################

indicator_paths_long <- indicator_paths |>
  pivot_longer(cols = -time,
               names_to = "indicator",
               values_to = "value") |>
  filter(!is.na(value))
future_paths_long <- future_paths |>
  pivot_longer(cols = -c(code, year, speed),
               names_to = "indicator",
               values_to = "value") |>
  filter(!is.na(value))

write.csv(indicator_paths_long, file.path(output_dir, "indicator_typical_paths.csv"))
write.csv(future_paths_long, file.path(output_dir, "country_future_paths.csv"))

##################################################################
### Creating Line Graph with all the indicators' typical paths ###
##################################################################

indicator_paths_plot <- indicator_paths_long |>
  group_by(indicator) |>
  mutate(label = if_else(time == max(time), as.character(indicator), NA_character_)) |>
  ungroup() |>
  ggplot(aes(x = time, y = value, color = indicator)) + 
  geom_line() + 
  geom_label_repel(aes(label = label),
                   nudge_x = max(indicator_paths_long$time) * 0.05,
                   direction = "both",
                   hjust = 0,
                   segment.color = "black",
                   na.rm = FALSE) +
  theme_minimal() +
  theme(legend.position = "none")
  

ggsave(plot = indicator_paths_plot, file.path(output_dir, "indicator_paths.png"), width = 10, height = 5)




