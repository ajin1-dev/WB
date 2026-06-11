library(tidyverse)
library(readxl)
library(lubridate)
library(trackr)


## vectors for benchmarks and makeshift benchmarks
benchmarks = c("SOM", "SSF", "LIC", "WLD", "FCS")
classifications <- read_excel("Somalia_Hunger/Data/country_classifications.xlsx")
low_inc_codes <- classifications |>
  filter(`Income group` == "Low income") |>
  pull(Code)
ssa_codes <- classifications |>
  filter(Region == "Sub-Saharan Africa") |>
  pull(Code)

# Load in PoU dataset
## Prevalence of undernourishment (% of population); ind = SN.ITK.DEFC.ZS
pou <- read.csv("Somalia_Hunger/Data/API_SN.ITK.DEFC.ZS_DS2_en_csv_v2_331187.csv", skip = 4) |>
  select(c(Country.Name, Country.Code, "X2001":last_col(offset = 3))) |>
  filter(Country.Code %in% benchmarks)
colnames(pou) <- gsub("X", "", colnames(pou))
pou <- pou |>
  pivot_longer(cols = "2001":last_col(),
               names_to = "year",
               values_to = "value")

# Create PoU visualization
pou_plot <- pou |> ggplot(aes(x = year, y = value, group = Country.Name, color = Country.Name)) +
  geom_line() +
  scale_x_discrete(breaks = seq(2001, 2023, by = 5)) +
  labs(title = "Prevalence of Undernourishment (% of population)",
       x = "Year",
       y = "% Undernourishment",
       color = "Country/Grouping")
ggsave("Somalia_Hunger/Visualizations/Pct_Undernourishment.png")



# Load in FIES Dataset
## Prevalence of moderate or severe food insecurity in the population (%); ind = SN.ITK.MSFI.ZS
insecurity_ms<- read.csv("Somalia_Hunger/Data/API_SN.ITK.MSFI.ZS_DS2_en_csv_v2_7955.csv", skip = 4) |>
  select(c(Country.Name, Country.Code, "X2021")) |>
  rename(pct = X2021)

## Creating Makeshift Benchmarks
### world average
wld_avg_ms <- insecurity_ms |>
  summarize(pct = mean(pct, na.rm = TRUE)) |>
  mutate(Country.Name = "World", Country.Code = "WLD")
### ssa average
ssa_avg_ms <- insecurity_ms |>
  filter(Country.Code %in% ssa_codes) |>
  summarize(pct = mean(pct, na.rm = TRUE)) |>
  mutate(Country.Name = "Sub-Saharan Africa", Country.Code = "SSF")
### low income average
lic_avg_ms <- insecurity_ms |>
  filter(Country.Code %in% low_inc_codes) |>
  summarize(pct = mean(pct, na.rm = TRUE)) |>
  mutate(Country.Name = "Low income", Country.Code = "LIC")
### Joining to main dataset and filtering
insecurity_ms <- insecurity_ms |>
  filter(Country.Code == "SOM") |>
  bind_rows( wld_avg_ms, ssa_avg_ms, lic_avg_ms)
  

## Prevalence of severe food insecurity in the population (%); ind = SN.ITK.SVFI.ZS
insecurity_s <- read.csv("Somalia_Hunger/Data/API_SN.ITK.SVFI.ZS_DS2_en_csv_v2_331188.csv", skip = 4) |>
  select(c(Country.Name, Country.Code, Indicator.Name, "X2021")) |>
  rename(pct = X2021)

## Creating Makeshift Benchmarks
### world average
wld_avg_s <- insecurity_s |>
  summarize(pct = mean(pct, na.rm = TRUE)) |>
  mutate(Country.Name = "World", Country.Code = "WLD")
### ssa average
ssa_avg_s <- insecurity_s |>
  filter(Country.Code %in% ssa_codes) |>
  summarize(pct = mean(pct, na.rm = TRUE)) |>
  mutate(Country.Name = "Sub-Saharan Africa", Country.Code = "SSF")
### low income average
lic_avg_s <- insecurity_s |>
  filter(Country.Code %in% low_inc_codes) |>
  summarize(pct = mean(pct, na.rm = TRUE)) |>
  mutate(Country.Name = "Low income", Country.Code = "LIC")
### Joining to main dataset and filtering
insecurity_s <- insecurity_s |>
  filter(Country.Code == "SOM") |>
  bind_rows( wld_avg_s, ssa_avg_s, lic_avg_s)


# Create FIES visualizations
modsev_plot <- insecurity_ms |>
  ggplot(aes(x = Country.Name, y = pct, fill = Country.Name)) +
  geom_col() +
  labs(
    title = "2021: Prevalence of moderate and severe food insecurity in the population (%)",
    x = "Country/Grouping",
    y = "% of population insecure",
    fill = "Country/Grouping"
  )
ggsave("Somalia_Hunger/Visualizations/Mod_Sev_Insecurity.png")

sev_plot <- insecurity_s |>
  ggplot(aes(x = Country.Name, y = pct, fill = Country.Name)) +
  geom_col() +
  labs(
    title = "2021: Prevalence of severe food insecurity in the population (%)",
    x = "Country/Grouping",
    y = "% of population insecure",
    fill = "Country/Grouping"
  )
ggsave("Somalia_Hunger/Visualizations/Sev_Insecurity.png")


# Load in IPC data
ipc0 <- read_excel("Somalia_Hunger/Data/IPC_Population_Analysis_Styled_2017,2027_1781118839874.xlsx") |>
  select(1:"Current - Phase 3+ %") |>
  filter(!is.na(`Country Population`) & !is.na(`Current - Analysis Period`)) |>
  filter(`Country/Analysis Name/Group Name` != "SO: Acute Food Insecurity Analysis January 2026")
ipc1 <- read_excel("Somalia_Hunger/Data/IPC_Population_Analysis_Styled_2017,2027_1781119874050.xlsx") |>
  select(1:"Current - Phase 3+ %") |>
  filter(!is.na(`Country Population`))
ipc <- bind_rows(ipc0, ipc1)
ipc <- ipc |> 
  rename(population = `Country Population`, date = `Date of Analysis`) |>
  mutate(date = ymd_hms(date))


## Data with population
ipc_pop <- ipc |>
  select(population, date, ends_with("Pop")) |>
  rename_with(~gsub("Current - ", "", .x)) |>
  rename_with(~gsub(" Pop", "", .x))
ipc_pop <- ipc_pop |>
  pivot_longer(cols = `Phase 1`:`Phase 3+`,
               names_to = "phase",
               values_to = "pop") |>
  mutate(pop_scaled = pop/1000000)

## Data with percents
ipc_pct <- ipc |>
  select(population, date, ends_with("%")) |>
  rename_with(~gsub("Current - ", "", .x)) |>
  rename_with(~gsub(" %", "", .x))
ipc_pct <- ipc_pct |>
  pivot_longer(cols = `Phase 1`:`Phase 3+`,
               names_to = "phase",
               values_to = "pct")


# IPC Visualizations
ipc_pop_plot <- ipc_pop |> filter(phase != "Phase 3+") |>
  ggplot(aes(x = date, y = pop_scaled, fill = phase)) +
  geom_area() +
  labs(
    title = "IPC Food Insecurity Classifications, by Population",
    subtitle = "Somalia 2017-2026",
    x = "Date",
    y = "Population (Millions)",
    fill = ""
  )
ggsave("Somalia_Hunger/Visualizations/IPC_Phases_Pop.png")

ipc_pct_plot <- ipc_pct |> filter(phase != "Phase 3+") |>
  ggplot(aes(x = date, y = pct, fill = phase)) +
    geom_area() +
  labs(
    title = "IPC Food Insecurity Classifications, (% of Population)",
    subtitle = "Somalia 2017-2026",
    x = "Date",
    y = "Percent",
    fill = ""
  )
ggsave("Somalia_Hunger/Visualizations/IPC_Phases_Pct.png")

ipc_p3_plot <- ipc_pct |> filter(phase == "Phase 3+") |>
  ggplot(aes(x = date, y = pct)) + geom_line() +
  labs(
    title = ""
  )

#Stunting
stunting_modeled <- wbstats::wb_data(indicator = "SH.STA.STNT.ME.ZS",
                                     start_date = 2000,
                                     end_date = 2026,
                                     lang = "en",
                                     country = benchmarks)

stunting_modeled_plot <- stunting_modeled |>
  ggplot(aes(x=date, y = SH.STA.STNT.ME.ZS, color = country)) +
  geom_line(linewidth = 0.75) +
  labs(
    title = "Prevalence of Stunting, (% of Population",
    x = "Year",
    y = "Percent",
    color = "Country/Grouping"
  )
ggsave("Somalia_Hunger/Visualizations/Modeled_Stunting.png")
