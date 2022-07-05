library(tidyr)
library(dplyr)
library(ggplot2); theme_set(theme_bw(base_family = "Times", base_size = 14))
library(lubridate)
library(data.table)
library(purrr)
library(MMWRweek)
library(cdcfluview)

# download hospital capacity dataset 

setwd("C:/My Documents/Cecile/CoV/ScenarioHub/flu-scenario-modeling-hub_resources/HospitalizationsAnalysis")


## download HHS dataset from API, 2020-present
download.file("https://healthdata.gov/api/views/anag-cw7u/rows.csv?accessType=DOWNLOAD","US_HHS_flu_COVID.csv")

hhs=read.csv("US_HHS_flu_COVID.csv") %>%
  dplyr::select(hospital_pk, 
                collection_week,
                state,
                total_patients_hospitalized_confirmed_influenza_7_day_avg,
                icu_patients_confirmed_influenza_7_day_avg,
                total_patients_hospitalized_confirmed_influenza_7_day_sum,
                icu_patients_confirmed_influenza_7_day_sum,
                total_patients_hospitalized_confirmed_influenza_7_day_coverage,
                total_patients_hospitalized_confirmed_influenza_and_covid_7_day_coverage,
                previous_day_admission_influenza_confirmed_7_day_sum) %>%
  mutate(date=as.Date(as.character(collection_week), "%Y/%m/%d"),
         week=epiweek(date),
         year=epiyear(date),
         datew=MMWRweek2Date(year, week, 7)) %>%
  mutate(across(c(total_patients_hospitalized_confirmed_influenza_7_day_avg:previous_day_admission_influenza_confirmed_7_day_sum), 
                ~ ifelse(.x == -999999, NA, .x)))  %>%
  arrange(state, date) %>%
  group_by(state, datew) %>%
  summarise(across(c(total_patients_hospitalized_confirmed_influenza_7_day_avg:previous_day_admission_influenza_confirmed_7_day_sum),
              ~sum(.x, na.rm = TRUE))) %>%
 arrange(state,datew)
  
hhs_natl=hhs %>%
    group_by(datew) %>%
  summarise(across(c(total_patients_hospitalized_confirmed_influenza_7_day_avg:previous_day_admission_influenza_confirmed_7_day_sum),
                   ~sum(.x, na.rm = TRUE))) %>%
  ungroup() 

colors <- c("Patients hosp. flu" = "blue", "New admissions flu" = "red")

g0 <- ggplot(hhs_natl) +
  geom_line(aes(datew, total_patients_hospitalized_confirmed_influenza_7_day_avg*7, color="Patients hosp. flu"), lwd=0.8) +
  geom_line(aes(datew, previous_day_admission_influenza_confirmed_7_day_sum, color="New admissions flu"), lwd=0.8 ) +
 # geom_line(aes(datew, total_patients_hospitalized_confirmed_influenza_7_day_coverage), lwd=0.8, color="blue" ) +
  geom_vline(xintercept = c(as.Date("2021-01-01"),as.Date("2022-01-01")), lty=2)+
  labs(x = "Date",
       y = "Hospitalizations",
       color = "Legend")+
xlim(c(as.Date("2020-01-01"),as.Date("2022-08-01"))) +
  scale_color_manual(values = colors) +
  theme(legend.position = c(0.3, 0.8)) +ggtitle("HHS dataset")

g0

ggsave("HHS_flu_national.png", g0, width=12, height=9)

############################
# Getting FluSurv-NET data from cdcfluview package
###############################


## list of locations; note there are two contributig systems EIP and IHSP 
flusurvnet.geography=surveillance_areas() 
flusurvnet.eip= flusurvnet.geography %>% filter(surveillance_area=="eip")
flusurvnet.ihsp= flusurvnet.geography %>% filter(surveillance_area=="ihsp")

#we can only get data for one location at a time, so building a list where each location is an element
eiplist <- vector('list', length(flusurvnet.eip$region))
ihsplist <- vector('list', length(flusurvnet.ihsp$region))

hosp_fs <- hospitalizations("flusurv", years=2015)  
hosp_eip <- hospitalizations("eip") 
hosp_ihsp <- hospitalizations("ihsp")

## EIP sites dowload
for (i in flusurvnet.eip$region){eiplist[[i]] <- hospitalizations(surveillance_area="eip", region=i)}
eiplist=eiplist %>% bind_rows


## IHSP sites dowload
for (i in flusurvnet.ihsp$region){ihsplist[[i]] <- hospitalizations(surveillance_area="ihsp", region=i)}
ihsplist=ihsplist %>% bind_rows

## Combined sites download
flusurvtot= hospitalizations(surveillance_area="flusurv")

## combining all 3 types of sites, 2003-2021
historic=eiplist %>% bind_rows(ihsplist) %>% bind_rows(flusurvtot) %>%
  arrange(surveillance_area,region,age, wk_start) %>% dplyr::select (-mmwrid) %>%
  filter (age < 7) # age categories above 7 not labelled

## Adding the most recent seasons 2020-21 and 2021-22

## 2020-21 and 2021-22 data not yet loaded on package so have to download from https://gis.cdc.gov/grasp/FluView/FluHospRates.html
    
flusurv202022=read.csv("FluSurveillance_Custom_Download_Data_2020_22.csv") %>%
  mutate(CUMULATIVE.RATE2=case_when(CUMULATIVE.RATE=="null" ~ 0, 
                                    CUMULATIVE.RATE!="null" ~ as.numeric(CUMULATIVE.RATE))) %>%
  dplyr::select(-CUMULATIVE.RATE) %>%
  dplyr::rename(CUMULATIVE.RATE=CUMULATIVE.RATE2) %>%
  mutate(yr_labels=case_when(YEAR=="2020-21" ~ list(c(2020,60,"Season 2020-21")),
                             YEAR=="2021-22" ~ list(c(2021,61,"Season 2021-22")))) %>%
  mutate(year= as.numeric(yr_labels  %>% map_chr(~ .x[1] )),
         season= as.numeric(yr_labels  %>% map_chr(~ .x[2] )),
         sea_description= yr_labels  %>% map_chr(~ .x[3] )) %>%
  dplyr::select(-yr_labels) %>%
  filter(AGE.CATEGORY %in% c("0-4 yr", "5-17 yr", "18-49 yr", "50-64 yr", "65+ yr", "Overall") &
         SEX.CATEGORY =="Overall" & RACE.CATEGORY=="Overall") %>%
  rename(sea_label=YEAR,
         surveillance_area=NETWORK,
         region=CATCHMENT,
         year_wk_num=MMWR.WEEK,
         age_label=AGE.CATEGORY,
         rate=CUMULATIVE.RATE,
          weeklyrate= WEEKLY.RATE) %>%
  mutate(age=case_when(age_label=="0-4 yr" ~ 1,
                       age_label=="5-17 yr" ~ 2,
                       age_label=="18-49 yr" ~ 3,
                       age_label=="50-64 yr" ~ 4,
                       age_label=="65+ yr" ~ 5,
                       age_label=="Overall" ~ 6),
         wk_start=MMWRweek2Date(MMWR.YEAR, year_wk_num, 1),
         wk_end=MMWRweek2Date(MMWR.YEAR, year_wk_num, 7)) %>%
  dplyr::select(-SEX.CATEGORY,-RACE.CATEGORY,-MMWR.YEAR)  %>%
  arrange(surveillance_area, region, age, age_label,
          season, sea_description, sea_label,
          wk_start, wk_end, year_wk_num)  %>%
  group_by(surveillance_area, region, age, age_label,
           season, sea_description, sea_label) %>%
  mutate(weeklyrate2=rate-lag(rate)) %>% dplyr::select(-weeklyrate2)
## Here I was trying to reconstruct the missing weekly rates in 2020-21 but it does not seem to work


## Combining all flusurv datasets, 2003-2022

allflusurv=historic %>% bind_rows(flusurv202022) %>%
  arrange(surveillance_area,region,age, wk_start) 

## plot flusurv data by week and age

## Weekly hospitalizations
g1 <- ggplot(subset(allflusurv, (surveillance_area =="FluSurv-NET" & 
                                   region=="Entire Network")), group=age_label) +
  geom_line(aes(wk_start, weeklyrate, color=age_label), lwd=0.9) +
  facet_wrap(~sea_description, scale="free", ncol=4)+
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank() )+
  ylab("Hospitalization rate per 100,000") + xlab("Date") + ggtitle("FluSurvNet")
g1


ggsave("FluSurvNetBySeasonAndAge.png", g1, width=13, height=9)

## Cumulative hospitalizations
g2 <- ggplot(subset(allflusurv, (surveillance_area =="FluSurv-NET" & 
                                   region=="Entire Network")), group=age_label) +
  geom_line(aes(wk_start, rate, color=age_label), lwd=0.9) +
  facet_wrap(~sea_description, scale="free", ncol=4)+
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank() )+
  ylab("Hospitalization rate per 100,000") + xlab("Date") + ggtitle("FluSurvNet")
g2
## Note that cum rates in 2021-22 look wrong

