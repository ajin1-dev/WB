library(tidyverse)
library(readxl)


country_codes <- c("BGD", "IDN", "KEN", "MAR", "NPL", "PAK", "PHL", "TZA", "WLD")
electrify <- read.csv("~/Desktop/R Practice/Data/API_EG.ELC.ACCS.ZS_DS2_en_csv_v2_262855.csv")
electrifyclean <- electrify |> filter(Country.Code %in% country_codes)
colnames(electrifyclean) <- gsub("X", "", colnames(electrifyclean))

electrifyclean_long <- electrifyclean |>
  pivot_longer(
    cols = matches("^[0-9]{4}$"),  # grabs year columns
    names_to = "Year",
    values_to = "Electrification"
  ) |>
  mutate(Year = as.integer(Year)) |>
  filter(Year >= 1990) |>
  rename(Country = Country.Name)


Electricity_Plot <- ggplot(electrifyclean_long, aes(x = Year, y = Electrification, color = Country)) +
  geom_line() +
  ggtitle("Access to Electrification (%)")

ggsave(Electricity_Plot, 
       filename = "Electricity_Plot.pdf",
       device = "pdf")

schooling <- read.csv("~/Desktop/R Practice/Data/Expected years of schooling.csv")
schoolingclean <- schooling |>
  filter(Country.Code %in% country_codes) |>
  filter(Year >= 1990) |>
  rename(Exp_Schooling = Value) |>
  rename(Country = Country.Name)

schooling_female <- schoolingclean |> filter(Disaggregation == "female")
schooling_male <- schoolingclean |> filter(Disaggregation == "male")
schooling_total <- schoolingclean |> filter(Disaggregation == "total")

Schooling_Plot_Female <- ggplot(schooling_female, aes(x = Year, y = Exp_Schooling, color = Country)) +
  geom_line() +
  ggtitle("Expected Schooling - Female")

Schooling_Plot_Male <- ggplot(schooling_male, aes(x = Year, y = Exp_Schooling, color = Country)) +
  geom_line() +
  ggtitle("Expected Schooling - Male")

Schooling_Plot_Total <- ggplot(schooling_total, aes(x = Year, y = Exp_Schooling, color = Country)) +
  geom_line() +
  ggtitle("Expected Schooling - Total")

Schooling_Plot_Female
Schooling_Plot_Male
Schooling_Plot_Total

world_vals <- schoolingclean |>
  filter(Country == "World") |>
  select(Year, Disaggregation, ExS_World = Exp_Schooling)

schooling_diff <- schoolingclean |>
  filter(Country != "World") |>
  left_join(world_vals, by = c("Year", "Disaggregation")) |>
  mutate(Diff = Exp_Schooling - ExS_World) |>
  select(Year, Disaggregation, Country, Diff) |>
  filter(Disaggregation != "total")

for (country in unique(schooling_diff$Country)) {
  
  p <- schooling_diff |>
    filter(Country == country) |>
    ggplot(aes(x = Year, y = Diff, color = Disaggregation)) +
    geom_hline(yintercept = 0, linewidth = 1, color = "black") +
    geom_line() +
    labs(
      title = paste("Gap to World Average —", country),
      x = "Year",
      y = "Years Relative to World Average"
    )
  
  print(p)
  ggsave(paste0("~/Desktop/R Practice/Plots/Schoolingdiff_", country, ".png"), plot = p)
}

