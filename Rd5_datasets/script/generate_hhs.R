
library(dplyr)

df0 <- read.csv("https://us-dhhs-aa.s3.us-east-2.amazonaws.com/g62h-syeh_2024-06-28T15-57-21.csv")
df <- 
  dplyr::select(df0, state, date, 
                d_adm = previous_day_admission_influenza_confirmed,
                d_adm_cov = previous_day_admission_influenza_confirmed_coverage,
                d_death = previous_day_deaths_influenza,
                d_death_cov = previous_day_deaths_influenza_coverage) %>% 
  dplyr::mutate(date = as.Date(date, "%Y/%m/%d") - 1,
                epiweek = paste0(MMWRweek::MMWRweek(date)$MMWRyear, "w",
                                 MMWRweek::MMWRweek(date)$MMWRweek)) %>% 
  dplyr::summarise(dplyr::across(dplyr::starts_with("d_"),
                                 list(function(x) sum(x, na.rm = TRUE)), 
                                 .names = "w_{.col}"), 
                   .by = c("state", "epiweek")) %>% 
  tidyr::separate(epiweek, into = c("year", "week"), sep = "w") %>% 
  dplyr::mutate(date = MMWRweek::MMWRweek2Date(as.numeric(year), 
                                               as.numeric(week), 7)) %>% 
  dplyr::arrange(date, state) %>% 
  dplyr::select(state, 
                date.wk_end = date,
                weekly.flu.admissions.HHS = w_d_adm,
                weekly.flu.admissions.HHS.coverage = w_d_adm_cov,
                weekly.flu.deaths.HHS = w_d_death,
                weekly.flu.deaths.HHS.coverage = w_d_death_cov)

write.csv(df, "Rd5_datasets/HHS_flu_2020_2024_dataset.csv", row.names = FALSE)
