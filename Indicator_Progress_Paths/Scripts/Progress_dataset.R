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
library(tidyverse)
library(trackr)
library(readxl)
library(wbstats)

## set wd to the folder that you download this folder that Indicator_Progress_Paths folder is downloaded into
setwd("C:/Users/wb661549/OneDrive - WBG/Desktop/Internship/WB/Indicator_Progress_Paths")
input_dir <- "input"
output_dir <- "Outputs"


## Getting parameters
meta <- read.csv(file.path(input_dir, "meta_sheet.csv")) |>
  collapse::fmutate(best = ifelse(more_is_better == 1, "high", "low"))
meta_new <- read.csv(file.path(input_dir, "meta_sheet_new.csv")) |>
  collapse::fmutate(best = ifelse(more_is_better == 1, "high", "low"))


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
    rename(!!paste0(tracked_indicator, "_fut") := y_fut)
  
  ### Creating Typical path plot
  typical_path_plot <- typical_path |>
    ggplot(aes(x = time, y = !!sym(tracked_indicator))) +
    geom_line() +
    labs(title = metadata$indicatorname)
  print(typical_path_plot)
  ggsave(paste0(output_dir, tracked_indicator, "_plot.png"),
         plot = typical_path_plot)
  
  ### Combining with indicator_paths dataframe
  indicator_paths <- indicator_paths |> full_join(typical_path, by = "time")
  future_paths <- future_paths |> full_join(country_future_path, by = c("code", "year", "speed"))
}







## Getting water data from JMP_2025_WLD.xlsx
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
    rename(!!paste0(tracked_indicator, "_fut") := y_fut)
  
  ### Creating Typical path plot
  typical_path_plot <- typical_path |>
    ggplot(aes(x = time, y = !!sym(tracked_indicator))) +
    geom_line() +
    labs(title = tracked_indicator)
  print(typical_path_plot)
  ggsave(paste0(output_dir, tracked_indicator, "_plot.png"),
         plot = typical_path_plot)
  
  ### Combining with indicator_paths dataframe
  indicator_paths <- indicator_paths |> full_join(typical_path, by = "time")
  future_paths <- future_paths |> full_join(country_future_paths, by = c("code", "year", "speed"))
}


### Saving the Datasets ###
write.csv(indicator_paths, file.path(output_dir, "indicator_typical_paths.csv"))
write.csv(future_paths, file.path(output_dir, "country_future_paths.csv"))










### Data for Direct Downloads is not working ###
stop("this part isn't working yet")

## List for indicators with data in the input file
data_list <- c("electricity",
               "education",
               "ghggdp",
               "lifeexpectancy",
               "gender")



## Loading parameters
load(file.path(input_dir, "parameters.Rda"))



load(file.path(input_dir, "education.Rda"))
data <- get("education") |>
  filter(!is.na(education))
metadata <- meta_new |> filter(dataname == "education")
progress <- track_progress(
  data = data,
  indicator = "education",
  code_col = "code",
  year_col = "year",
  startyear_data = 1950,
  endyear_data = 2023,
  eval_from = 2015,
  eval_to = 2023,
  speed = TRUE,
  percentiles = FALSE,
  future = FALSE, ## TRUE,
  target_year = 2030,
  sequence_pctl = seq(20, 80, 20),
  sequence_speed = c(0.25, 0.5, 1, 2, 4),
  best = "high",
  support = 1,
  granularity = 0.1,
  verbose = TRUE)









#### Testing ####




## Getting Data, Creating typical path, then plotting and saving plot
for (tracked_indicator in data_list) {
  load(paste0(
    "Indicator_Progress_Paths/input/", tracked_indicator, ".Rda"))
  data <- get(tracked_indicator)
  metadata <- parameters |> filter(indicator == tracked_indicator)
  progress <- track_progress(
    data = data,
    indicator = tracked_indicator,
    code_col = "code",
    year_col = "year",
    startyear_data = min(data$year),
    endyear_data = max(data$year),
    eval_from = (max(data$year) - 5),
    eval_to = max(data$year),
    speed = TRUE,
    percentiles = FALSE,
    future = TRUE,
    target_year = 2030,
    sequence_pctl = seq(20, 80, 20),
    sequence_speed = c(0.25, 0.5, 1, 2, 4),
    best = metadata$best,
    support = metadata$support,
    granularity = metadata$granularity)
  print(metadata$label)
  typical_path <- progress$predicted_changes$path_speed |>
    rename(!!tracked_indicator := y)
  
  country_future_paths <- progress$path_future$speed |>
    select(-speed_source) |>
    rename(!!paste0(tracked_indicator, "_fut") := y_fut)
  
  ### Creating Typical path plot
  typical_path_plot <- typical_path |>
    ggplot(aes(x = time, y = !!sym(tracked_indicator))) +
    geom_line() +
    labs(title = metadata$label)
  print(typical_path_plot)
  ggsave(
    paste0(
      "Indicator_Progress_Paths/Outputs/",
      tracked_indicator,
      "_plot.png"
    ),
    plot = typical_path_plot
  )
  
  ### Combining with indicator_paths dataframe
  indicator_paths <- indicator_paths |> full_join(typical_path, by = "time")
  future_paths <- future_paths |> full_join(country_future_paths, by = c("code", "year", "speed"))
  
}









#### TESTING ####
stop("testing")
test <- wbstats::wb_data(indicator = "SL.TLF.ACTI.FE.ZS",
                         lang = "en",
                         country = "countries_only")
result <- track_progress(
  data = test,
  indicator = "SL.TLF.ACTI.FE.ZS",
  code_col = "iso3c",
  year_col = "date",
  startyear_data = 1990,
  endyear_data = 2025,
  eval_from = 2015,
  eval_to = 2025,
  speed = TRUE,
  percentiles = FALSE,
  future = TRUE,
  target_year = 2030,
  sequence_pctl = seq(20, 80, 20),
  sequence_speed = c(0.25, 0.5, 1, 2, 4),
  best = "high",
  support = 1,
  granularity = 0.1
)

typical_path <- result$predicted_changes$path_speed

### Creating Typical path plot
typical_path_plot <- typical_path |>
  ggplot(aes(x = time, y = y)) +
  geom_line() + labs(title = "FLFP Outside of the loop, inputted values")
ggsave(
  "Indicator_Progress_Paths/Outputs/FLFP Outside of the loop, inputted values.png"
)



metadata <- meta |> filter(indicator == "SL.TLF.ACTI.FE.ZS")

test <- wbstats::wb_data(indicator = "SL.TLF.ACTI.FE.ZS",
                         lang = "en",
                         country = "countries_only")

result <- track_progress(
  data = test,
  indicator = "SL.TLF.ACTI.FE.ZS",
  code_col = "iso3c",
  year_col = "date",
  startyear_data = 1990,
  endyear_data = 2025,
  eval_from = 2015,
  eval_to = 2025,
  speed = TRUE,
  percentiles = TRUE,
  future = FALSE,
  target_year = 2030,
  sequence_pctl = seq(20, 80, 20),
  sequence_speed = c(0.25, 0.5, 1, 2, 4),
  best = "high",
  support = 1,
  granularity = 0.1,
  verbose = TRUE
)

typical_path <- result$predicted_changes$path_speed

### Creating Typical path plot
typical_path_plot <- typical_path |>
  ggplot(aes(x = time, y = y)) +
  geom_line() + labs(title = "FLFP")



ggsave(
  "Indicator_Progress_Paths/Outputs/FLFP"
)
