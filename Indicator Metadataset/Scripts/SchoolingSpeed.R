library(tidyverse)
library(readxl)

country_codes <- c("BGD", "IDN", "KEN", "MAR", "NPL", "PAK", "PHL", "TZA", "WLD")
schooling <- read.csv("C:/Users/wb661549/OneDrive - WBG/Desktop/R/Data/48327fbd-9372-496a-b5b6-bb1666ef6d69_Data.csv", na.strings = "..") |>
  filter(Country.Code %in% country_codes) |>
  select(-Series.Code) |>
  mutate(Series.Name = case_when(Series.Name == "UIS: Mean years of schooling (ISCED 1 or higher), population 25+ years, female" ~ "female",
                                 Series.Name == "UIS: Mean years of schooling (ISCED 1 or higher), population 25+ years, male" ~ "male",
                                 Series.Name == "UIS: Mean years of schooling (ISCED 1 or higher), population 25+ years, both sexes" ~ "total",
                                 Series.Name == "Adult literacy rate, population 15+ years, both sexes (%)" ~ "adult_literacy",
                                 TRUE ~ Series.Name))

colnames(schooling) <- gsub(".*([0-9]{4}).*", "\\1", colnames(schooling))


schooling_rates <- schooling |> 
  pivot_longer(cols = 4:38,
               names_to = "year",
               values_to = "value") |>
  mutate(year = as.integer(year))

rates <- schooling_rates |>
  filter(!is.na(value)) |>
  group_by(Country.Name, Series.Name) |>
  arrange(year, .by_group = TRUE) |>
  mutate(
    rate = (lead(value) - lag(value)) / (lead(year) - lag(year))
  ) |>
  ungroup() |>
  select(Country.Name, Series.Name, year, rate)

schooling_rates <- schooling_rates %>%
  left_join(rates, by = c("Country.Name", "Series.Name", "year"))

for (disagg in unique(schooling_rates$Series.Name)) {
  plot <- schooling_rates |>
    filter(Series.Name == disagg, !is.na(rate)) |>
    ggplot(aes(x = year, y = rate, color = Country.Name)) +
    geom_line() +
    labs(
      title = disagg
    )
  print(plot)
}

country_codes <- c("BGD", "IDN", "KEN", "MAR", "NPL", "PAK", "PHL", "TZA", "WLD")
schooling <- read.csv("C:/Users/wb661549/OneDrive - WBG/Desktop/R/Data/WB_HCP_UISCR1_WIDEF.csv", na.strings = "..") |>
  rename(Country.Code = REF_AREA, Country.Name = REF_AREA_LABEL, Series.Name = SEX_LABEL) |>
  filter(Country.Code %in% country_codes) |>
  select(-c(FREQ,
            FREQ_LABEL,
            INDICATOR, 
            INDICATOR_LABEL,
            SEX, 
            UNIT_MEASURE,
            UNIT_MEASURE_LABEL, 
            DECIMALS, 
            DECIMALS_LABEL, 
            DATABASE_ID, 
            DATABASE_ID_LABEL,
            UNIT_MULT,
            UNIT_MULT_LABEL,
            OBS_STATUS,
            OBS_STATUS_LABEL,
            OBS_CONF, 
            OBS_CONF_LABEL))

colnames(schooling) <- gsub(".*([0-9]{4}).*", "\\1", colnames(schooling))


schooling_rates <- schooling |> 
  pivot_longer(cols = 4:38,
               names_to = "year",
               values_to = "value") |>
  mutate(year = as.integer(year))

rates <- schooling_rates |>
  filter(!is.na(value)) |>
  group_by(Country.Name, Series.Name) |>
  arrange(year, .by_group = TRUE) |>
  mutate(
    rate = (lead(value) - lag(value)) / (lead(year) - lag(year))
  ) |>
  ungroup() |>
  select(Country.Name, Series.Name, year, rate)

schooling_rates <- schooling_rates %>%
  left_join(rates, by = c("Country.Name", "Series.Name", "year"))

for (disagg in unique(schooling_rates$Series.Name)) {
  plot <- schooling_rates |>
    filter(Series.Name == disagg, !is.na(rate)) |>
    ggplot(aes(x = year, y = rate, color = Country.Name)) +
    geom_line() +
    labs(
      title = disagg
    )
  print(plot)
}

