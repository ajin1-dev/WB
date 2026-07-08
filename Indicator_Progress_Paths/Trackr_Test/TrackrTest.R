
### change wd
setwd("C:/Users/wb661549/OneDrive - WBG/Desktop/Internship/WB/Indicator_Progress_Paths/Trackr_Test")

library(tidyverse)
library(trackr)


load("education.Rda")
data <- get("education") |>
  rename("educ" = education)
progress <- track_progress(
  data = data,
  indicator = "educ",
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
