library(tidyverse)
library(readxl)
library(haven)
wb_countries <- read_dta("Data/WBcountrylist.dta")


# Population
pop_tot <- read.csv("Data/country_populations.csv") |>
  rename(code = REF_AREA, country = REF_AREA_LABEL) |>
  filter(code %in% wb_countries$country_code) |>
  select(
    c(
      country,
      code,
      X1960:last_col()
      )
  )
colnames(pop_tot) <- gsub("X", "", colnames(pop_tot))

pop_tot <- pop_tot |>
  pivot_longer(cols = "1960":last_col(),
               names_to = "year",
               values_to = "population"
               ) |>
  mutate(year = as.numeric(year), population = as.numeric(population))

master <- pop_tot


# Governance

## WGI
### estimate is std dev of score
### score is normalize value on scale from 0-100
file_path <- "Data/WGI_Governance.xlsx"
indicator_names <- excel_sheets(file_path)
indicator_list <- lapply(indicator_names, function(sheet) {
  read_excel(file_path, sheet = sheet) |>
    select(
           country = "Economy (name)",
           code = "Economy (code)", 
           year = "Year",
           !!paste0(sheet, "_estimate") := "Governance estimate (approx. -2.5 to +2.5)",
           !!paste0(sheet, "_score") := "Governance score (0-100)"
           )
})
governance <- reduce(indicator_list, full_join, by = c("country",
                                                       "code",
                                                       "year"
                                                       )
                     )

master <- master |> full_join(governance, by = c("code", "country", "year"))




# Debt

## General Government Debt Stock (Millions of Current USD), % of GDP
debt <- read.csv("Data/Debt_Stock.csv") |>
  rename(code = REF_AREA, country = REF_AREA_LABEL) |>
  select(-c(FREQ,
            FREQ_LABEL,
            INDICATOR, 
            INDICATOR_LABEL,
            UNIT_MEASURE,
            UNIT_MEASURE_LABEL, 
            VINTAGE, 
            VINTAGE_LABEL, 
            DATABASE_ID, 
            DATABASE_ID_LABEL,
            UNIT_MULT,
            UNIT_MULT_LABEL,
            OBS_STATUS,
            OBS_STATUS_LABEL,
            OBS_CONF, 
            OBS_CONF_LABEL
            )
         )
colnames(debt) <- gsub("X", "", colnames(debt))

debt <- debt |> 
  pivot_longer(
    cols = 3:51,
    names_to = "year",
    values_to = "debt_stock_gdp") |>
  mutate(year = as.numeric(year))

master <- master |> full_join(debt, by = c("country", "code", "year"))



# Conflict and Fragility

pol_violence <- read_excel("Data/number_of_political_violence_events_by_country-year_as-of-29May2026.xlsx") |>
  rename(country = COUNTRY, year = YEAR, pol_violence_events = EVENTS)

master <- master |> full_join(pol_violence, by = c("country", "year"))

fatalities <- read_excel("Data/number_of_reported_fatalities_by_country-year_as-of-29May2026.xlsx") |>
  rename(country = COUNTRY, year = YEAR, fatalities_pc = FATALITIES)

master <- master |> full_join(fatalities, by = c("country", "year")) |>
  mutate(fatalities_pc = fatalities_pc / population)

# ODA

## Net official development assistance and official aid received (constant 2023 US$)
oda <- read.csv("Data/ODA.csv") |>
  rename(code = REF_AREA, country = REF_AREA_LABEL) |>
  select(c(code,
           country,
           40:103))
colnames(oda) <- gsub("X", "", colnames(oda))

oda <- oda |> 
  pivot_longer(
    cols = 3:66,
    names_to = "year",
    values_to = "oda_pc") |>
  mutate(year = as.numeric(year))

master <- master |> full_join(oda, by = c("country", "code", "year")) |>
  mutate(oda_pc = oda_pc /population)

me <- read.csv("Data/indicator_metadataset_metadata.csv")




# Youth Labor Market
## Labor force participation rate for ages 15-24, total (%) (ILO Est.)
lfp <- read.csv("Data/Youth_LFP.csv") |>
  rename(code = REF_AREA, country = REF_AREA_LABEL) |>
  select(c(code,
           country,
           40:75))
colnames(lfp) <- gsub("X", "", colnames(lfp))

lfp <- lfp |> 
  pivot_longer(
    cols = 3:38,
    names_to = "year",
    values_to = "youth_lfp") |>
  mutate(year = as.numeric(year))

master <- master |> full_join(lfp, by = c("country", "code", "year"))


## Share of youth not in education, employment or training, total (% of youth population) (ILO Est.)
neet <- read.csv("Data/Youth_NEET.csv") |>
  rename(code = REF_AREA, country = REF_AREA_LABEL) |>
  select(c(code,
           country,
           40:60))
colnames(neet) <- gsub("X", "", colnames(neet))

neet <- neet |> 
  pivot_longer(
    cols = 3:23,
    names_to = "year",
    values_to = "neet") |>
  mutate(year = as.numeric(year))

master <- master |> full_join(neet, by = c("country", "code", "year"))





# Social Assistance

## Government expenditure on social protection
soc_assist <- read.csv("Data/Gov_SocExpend.csv") |>
  rename(code = REF_AREA, country = REF_AREA_LABEL) |>
  filter(UNIT_MEASURE == "PT_GDP") |>
  select(c(code,
           country,
           SECTOR_LABEL,
           "X1990":last_col()))

colnames(soc_assist) <- gsub("X", "", colnames(soc_assist))


soc_assist_clean <- soc_assist |>
  filter(SECTOR_LABEL == "Sector: Budgetary central government")
soc_assist_clean <- soc_assist_clean |>
  bind_rows(filter(soc_assist, SECTOR_LABEL == "Sector: Central government (excl. social security)" & !(country %in% soc_assist_clean$country) ))



  

soc_assist_clean <- soc_assist_clean |>
  pivot_longer(
    cols = "1990":last_col(),
    names_to = "year",
    values_to = "expend_gdp"
  )|>
  mutate(year = as.numeric(year)) |>
  select(-c(SECTOR_LABEL))


master <- master |> full_join(soc_assist_clean, by = c("country", "code", "year"))
# master <- master |> full_join(soc_assist_trial, by = c("country", "code", "year"))



#Trade
trade <- read.csv("Data/Trade.csv") |>
  rename(code = REF_AREA, country = REF_AREA_LABEL) |>
  select(c(code,
           country,
           "X1960":last_col()))
colnames(trade) <- gsub("X", "", colnames(trade))

trade <- trade |>
  pivot_longer(cols = "1960":last_col(),
               names_to = "year",
               values_to = "trade_gdp"
               ) |>
  mutate(year = as.numeric(year))
master <- master |> full_join(trade, by = c("country", "code", "year"))



#Business Climate
fdi <- read.csv("Data/FDI.csv", skip = 4) |>
  rename(code = Country.Code, country = Country.Name) |>
  select(c(code,
           country,
           "X1970":last_col(offset = 2)))
colnames(fdi) <- gsub("X", "", colnames(fdi))

fdi <- fdi |>
  pivot_longer(cols = "1970":last_col(),
               names_to = "year",
               values_to = "fdi_gdp"
  ) |>
  mutate(year = as.numeric(year))
master <- master |> full_join(fdi, by = c("country", "code", "year"))



#Private Sector
credit <- read.csv("Data/Domestic_Credit.csv") |>
  rename(code = REF_AREA, country = REF_AREA_LABEL) |>
  select(c(code,
           country,
           "X1960":last_col()))
colnames(credit) <- gsub("X", "", colnames(credit))

credit <- credit |>
  pivot_longer(cols = "1960":last_col(),
               names_to = "year",
               values_to = "credit_gdp"
  ) |>
  mutate(year = as.numeric(year))
master <- master |> full_join(credit, by = c("country", "code", "year"))




# Female LFP
female <- read.csv("Data/Female_LFP.csv", skip = 4) |>
  rename(code = Country.Code, country = Country.Name) |>
  select(c(code,
           country,
           "X1990":last_col(offset = 1)))
colnames(female) <- gsub("X", "", colnames(female))
female <- female |>
  pivot_longer(cols = "1990":last_col(),
               names_to = "year",
               values_to = "female_lfp"
  ) |>
  mutate(year = as.numeric(year))
master <- master |> full_join(female, by = c("country", "code", "year"))





# final
master <- master |> filter(code %in% wb_countries$country_code) |>
  mutate(across(-c(country, code), as.numeric)) |>
  select(where(~ mean(is.na(.)) < 0.9))

indicator_labels <- read.csv("Data/indicator_metadataset_metadata.csv")




for (i in seq_len(nrow(indicator_labels))) {
  var <- indicator_labels$Variable[i]
  lab <- indicator_labels$Description[i]
  
  master[[var]] <- labelled(master[[var]], label = lab)
}


write_dta(master, "indicator_metadataset.dta")





## STOP HERE
## STOP HERE
## STOP HERE
## STOP HERE
## STOP HERE
## STOP HERE
## STOP HERE
## STOP HERE
## STOP HERE
## STOP HERE
stop("Notes and Experiments")
soc_assist_trial <- soc_assist
soc_assist_trial <- soc_assist_trial |> 
  filter(country %in% (soc_assist_trial |>
                         group_by(country) |>
                         summarise(has_sector = any(SECTOR_LABEL == "Sector: Budgetary central government")) |>
                         filter(!has_sector) |>
                         pull(country)
  )
  )

aspire_sp <- read_dta("Data/ASPIRE performance indicators.dta") |>
  filter(Country_Code %in% wb_countries$country_code & Sub_Topic4 == "Total" & Sub_Topic1 == "Indicators estimated using pre-transfer welfare")|>
  arrange(Country_Code, Countries, Year, indicator_name, Indicator_Code)


soc_assist_trial <- soc_assist
soc_assist_trial <- soc_assist_trial %>%
  pivot_longer(cols = 6:55, names_to = "year", values_to = "value") %>%
  group_by(country, code, year) %>%
  summarise(
    total = {
      gg     <- value[SECTOR_LABEL == "Sector: General government"]
      local  <- value[SECTOR_LABEL == "Sector: Local government"]
      state  <- value[SECTOR_LABEL == "Sector: State governments"]
      cen_inc <- value[SECTOR_LABEL == "Sector: Central government (incl. social security funds)"]
      cen_exc <- value[SECTOR_LABEL == "Sector: Central government (excl. social security)"]
      ssf    <- value[SECTOR_LABEL == "Sector: Social security funds"]
      
      # collapse to single value or NA
      gg      <- if (length(gg) > 0) gg[1] else NA
      local   <- if (length(local) > 0) local[1] else NA
      state   <- if (length(state) > 0) state[1] else NA
      cen_inc <- if (length(cen_inc) > 0) cen_inc[1] else NA
      cen_exc <- if (length(cen_exc) > 0) cen_exc[1] else NA
      ssf     <- if (length(ssf) > 0) ssf[1] else NA
      
      if (!is.na(gg)) {
        gg
      } else {
        central <- if (!is.na(cen_inc)) cen_inc else sum(c(cen_exc, ssf), na.rm = TRUE)
        result <- sum(c(central, local, state), na.rm = TRUE)
        if (all(is.na(c(cen_inc, cen_exc, ssf, local, state)))) NA else result
      }
    },
    .groups = "drop"
  ) |>
  mutate(year = as.numeric(year))


gender <- read.csv("Data/genderlaws.csv") |>
  filter(REF_AREA_LABEL %in% wb_countries$country_name) |>
  pivot_longer(cols = "X1970":last_col(),
               names_to = "year",
               values_to = "gender_score")



for (indicator in indicator_names) {
  assign(paste0("governance_", indicator), indicator_list[[indicator]] |>
           select(
             c(
               "ID variable (economy code/ gov. dimension/ year)",
               "Economy (name)",
               "Economy (code)",
               "Year",
               "Governance estimate (approx. -2.5 to +2.5)",
               "Governance score (0-100)",
               )
             ) |>
           rename(ID = "ID variable (economy code/ gov. dimension/ year)",
                  name = "Economy (name)",
                  code = "Economy (code)", 
                  year = "Year",
                  !!paste0(indicator, "_estimate") := "Governance estimate (approx. -2.5 to +2.5)",
                  !!paste0(indicator, "_score") := "Governance score (0-100)")
           )
}



governance <- governance_va |> right_join(governance_cc, by )


for (indicator in indicator_names[2:6]) {
  governance_va <- governance_va |> left_join(paste0())
    
}

for (i in unique(soc_assist$country)) {
  socclean <- socclean |> bind_rows(soc_assist$country == i & SECTOR == "IMF_SEC_GG")
  if (!any(soc_assist$country == i & soc_assist$SECTOR == "IMF_SEC_GG")) {
    
  }
}
  


