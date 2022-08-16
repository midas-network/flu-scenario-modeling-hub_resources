library(tidyr)
library(dplyr)
library(ggplot2); theme_set(theme_bw(base_family = "Times", base_size = 14))
library(lubridate)
library(data.table)
library(purrr)
library(MMWRweek)
library(cdcfluview)



###############################
# download hospital capacity dataset from HHS 
###############################


## download HHS dataset from API, 2020-present. Use the state-level time series version, rather than facilities,
## since it's what's used for Flusight and has better flu variables

download.file("https://healthdata.gov/api/views/g62h-syeh/rows.csv?accessType=DOWNLOAD","HospitalizationsAnalysis/US_HHS_flu_COVID.csv")

 

hhs=read.csv("HospitalizationsAnalysis/US_HHS_flu_COVID.csv") %>%
  dplyr::select(state, date,    
                previous_day_admission_influenza_confirmed,
                previous_day_admission_influenza_confirmed_coverage,
                previous_day_deaths_covid_and_influenza,
                previous_day_deaths_covid_and_influenza_coverage,
                previous_day_deaths_influenza,
                previous_day_deaths_influenza_coverage,
                icu_patients_confirmed_influenza,
                icu_patients_confirmed_influenza_coverage,
                total_patients_hospitalized_confirmed_influenza,
                total_patients_hospitalized_confirmed_influenza_and_covid,
                total_patients_hospitalized_confirmed_influenza_and_covid_coverage,
                total_patients_hospitalized_confirmed_influenza_coverage,
                inpatient_beds_used_covid,
                inpatient_beds_used_covid_coverage,
                percent_of_inpatients_with_covid,
                adult_icu_bed_utilization,
                adult_icu_bed_utilization_coverage) %>%
    mutate(date=as.Date(as.character(date), "%Y/%m/%d")-1, ## data is for previous day flu admission
           week=epiweek(date),
           year=epiyear(date),
           datew=MMWRweek2Date(year, week, 7)) %>%
    arrange(state, date) %>%
    group_by(state, datew) %>%  ##summarize daily data into weekly counts
  summarise(across(c(previous_day_admission_influenza_confirmed:previous_day_deaths_influenza_coverage),
                   ~sum(.x, na.rm = TRUE)),
            across(c(icu_patients_confirmed_influenza:adult_icu_bed_utilization_coverage),
                   ~mean(.x, na.rm = TRUE))) %>%
    ungroup()

  
hhs_natl=hhs %>%
    group_by(datew) %>%
  summarise(across(c(previous_day_admission_influenza_confirmed:adult_icu_bed_utilization_coverage),
                   ~sum(.x, na.rm = TRUE))) %>%
  ungroup() %>% mutate(state="US")


pop=read.csv("HospitalizationsAnalysis/State Population till 2021.csv") %>%
  left_join(read.csv("HospitalizationsAnalysis/state abbreviations.csv"), by="region") %>%
  filter(year==2021) %>% dplyr::select(region,abbreviation, population)


hhs= hhs %>% 
  bind_rows(hhs_natl) %>% filter(state !="AS") %>%
  mutate(state=factor(state, levels=c("US", state.abb))) %>%
  left_join(pop, by=c("state"="abbreviation")) %>%
  mutate(flu_hosp_rate= previous_day_admission_influenza_confirmed/population*100000,
         flu_death_rate= previous_day_deaths_influenza/population*100000)


colors <- c("Hospitalization per 100,000" = "blue", 
            "No hospitals reporting flu"="green")

hhs_natl2=  hhs %>% filter(state=="US")
r=max(hhs_natl2$flu_hosp_rate,na.rm=T)/max(hhs_natl2$previous_day_admission_influenza_confirmed_coverage) # ratio of max flu admissions to max no reporting hospitals for plot

g0 <- ggplot(subset(hhs, state=="US" & datew<max(datew)-7)) +  #excluding most recent week as typically incomplete
  geom_line(aes(datew, flu_hosp_rate, color="Hospitalization per 100,000"), lwd=0.8) +
#  geom_line(aes(datew, flu_death_rate*10, color="Deaths per 1,000,000"), lwd=0.8 ) +
  geom_line(aes(datew, previous_day_admission_influenza_confirmed_coverage*r, 
                color="No hospitals reporting flu"), lwd=0.8 ) +
  
    # geom_line(aes(datew, total_patients_hospitalized_confirmed_influenza_7_day_coverage), lwd=0.8, color="blue" ) +
  geom_vline(xintercept = c(as.Date("2021-01-01"),as.Date("2022-01-01")), lty=2)+
  geom_vline(xintercept = c(as.Date("2022-02-02")), lty=2, color="purple")+
  labs(x = "Date",
       y = "Weekly influenza admission rates",
       color = "Legend")+
  xlim(c(as.Date("2020-09-01"),as.Date("2022-08-01"))) +
  scale_y_continuous( sec.axis = sec_axis(~ ./r, name = "No reporting hospitals"))+
  scale_color_manual(values = colors) +
  theme(legend.position = c(0.4, 0.5)) +ggtitle("HHS dataset")

g0

ggsave("HospitalizationsAnalysis/HHS_flu_national.png", g0, width=12, height=9)


colors <- c("Hospitalization" = "blue", "Deaths" = "red")

g00 <- ggplot(subset(hhs, state!="AS")) + 
  geom_line(aes(datew, flu_hosp_rate, color="Hospitalization"), lwd=0.8) +
  geom_line(aes(datew, flu_death_rate, color="Deaths"), lwd=0.8 ) +
  # geom_line(aes(datew, total_patients_hospitalized_confirmed_influenza_7_day_coverage), lwd=0.8, color="blue" ) +
  geom_vline(xintercept = c(as.Date("2021-01-01"),as.Date("2022-01-01")), lty=2)+
  labs(x = "Date",
       y = "Weekly hospitalizations or deaths per 100,000",
       color = "Legend")+
  xlim(c(as.Date("2021-09-01"),as.Date("2022-08-01"))) +
  scale_color_manual(values = colors) +
  facet_wrap(~state, scale="free", ncol=6)+
    theme(legend.position = c(0.8, 0.05)) +ggtitle("HHS dataset, flu 2021-22 season")

g00

ggsave("HospitalizationsAnalysis/HHS_flu_state.png", g00, width=15, height=12)


g00 <- ggplot(subset(hhs, state!="AS")) + 
  geom_line(aes(datew, flu_death_rate, color="Deaths"), lwd=0.8 ) +
  # geom_line(aes(datew, total_patients_hospitalized_confirmed_influenza_7_day_coverage), lwd=0.8, color="blue" ) +
  geom_vline(xintercept = c(as.Date("2021-01-01"),as.Date("2022-01-01")), lty=2)+
  labs(x = "Date",
       y = "Weekly hospitalizations",
       color = "Legend")+
  xlim(c(as.Date("2021-09-01"),as.Date("2022-08-01"))) +
  scale_color_manual(values = colors) +
  facet_wrap(~state, scale="free", ncol=6)+
  theme(legend.position = c(0.8, 0.05)) +ggtitle("HHS dataset")

g00

ggsave("HospitalizationsAnalysis/HHS_flu_deaths_state.png", g00, width=15, height=12)



colors <- c("Hospitals reporting" = "red")
g00 <- ggplot(subset(hhs, state!="AS")) + 
  geom_line(aes(datew, previous_day_admission_influenza_confirmed_coverage, color="Hospitals reporting"), lwd=0.8 ) +
  # geom_line(aes(datew, total_patients_hospitalized_confirmed_influenza_7_day_coverage), lwd=0.8, color="blue" ) +
  geom_vline(xintercept = c(as.Date("2021-01-01"),as.Date("2022-01-01")), lty=2)+
  labs(x = "Date",
       y = "Weekly no. hospitals reporting flu admissions",
       color = "Legend")+
  xlim(c(as.Date("2021-09-01"),as.Date("2022-08-01"))) +
  scale_color_manual(values = colors) +
  facet_wrap(~state, scale="free", ncol=6)+
  theme(legend.position = c(0.8, 0.05)) +ggtitle("HHS dataset -- flu reporting")

g00

ggsave("HospitalizationsAnalysis/HHS_flu_coverage_state.png", g00, width=15, height=12)




############################
# FluSurv-NET is an independant dataset that provides extra years for calibration nationally and for select states
# Here we get FluSurv-NET data from cdcfluview R package and most recent years from the CDC dashboard
###############################


## list of locations; note there are two contributig systems EIP and IHSP 
flusurvnet.geography=surveillance_areas() 
flusurvnet.eip= flusurvnet.geography %>% filter(surveillance_area=="eip")
flusurvnet.ihsp= flusurvnet.geography %>% filter(surveillance_area=="ihsp")

#we can only get data for one location at a time, so building a list where each location is an element
eiplist <- vector('list', length(flusurvnet.eip$region))
ihsplist <- vector('list', length(flusurvnet.ihsp$region))


## EIP sites dowload
for (i in flusurvnet.eip$region){eiplist[[i]] <- 
  hospitalizations(surveillance_area="eip", region=i)}
eiplist=eiplist %>% bind_rows


## IHSP sites dowload
for (i in flusurvnet.ihsp$region){ihsplist[[i]] <- hospitalizations(surveillance_area="ihsp", region=i)}
ihsplist=ihsplist %>% bind_rows

## Combined sites download
flusurvtot= hospitalizations(surveillance_area="flusurv")

## combining all 3 types of sites, 2003-2020
historic=eiplist %>% bind_rows(ihsplist) %>% bind_rows(flusurvtot) %>%
  arrange(surveillance_area,region,age, wk_start) %>% dplyr::select (-mmwrid) %>%
  filter (age < 7) %>% # age categories above 7 not labelled
  #filling in missing season labels when CA was the only reporting state
 mutate(sea_label=case_when(is.na(sea_label) & wk_start>as.Date("2003-08-31") &
                                               wk_start<as.Date("2004-09-01") ~ "2003-04",
                            is.na(sea_label) & wk_start>as.Date("2004-08-31") &
                              wk_start<as.Date("2005-09-01") ~ "2004-05",
                            is.na(sea_label) & wk_start>as.Date("2005-08-31") &
                              wk_start<as.Date("2006-09-01") ~ "2005-06",
                            is.na(sea_label) & wk_start>as.Date("2006-08-31") &
                              wk_start<as.Date("2007-09-01") ~ "2006-07",
                            is.na(sea_label) & wk_start>as.Date("2007-08-31") &
                              wk_start<as.Date("2008-09-01") ~ "2007-08",
                            is.na(sea_label) & wk_start>as.Date("2008-08-31") &
                              wk_start<as.Date("2009-09-01") ~ "2008-09",
                            !is.na(sea_label) ~ sea_label)) %>%
  mutate(sea_description=paste("Season", sea_label))
  
## Adding the most recent seasons 2020-21 and 2021-22

## 2020-21 and 2021-22 data not yet loaded on package so have to download from https://gis.cdc.gov/grasp/FluView/FluHospRates.html
    
flusurv202022=read.csv("HospitalizationsAnalysis/FluSurveillance_Custom_Download_Data_2020_2022_updated.csv", skip=2) %>%
  mutate(CUMULATIVE.RATE2=case_when(CUMULATIVE.RATE=="null" ~ 0, 
                                    CUMULATIVE.RATE!="null" ~ as.numeric(as.character(CUMULATIVE.RATE)))) %>%
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
          wk_start, wk_end, year_wk_num) 

## Note that most weekly incident rates are missing in 2020-21 given very low counts
## 2020-21 has so few flu hospitalizations, probably best to ignore


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


ggsave("HospitalizationsAnalysis/FluSurvNetBySeasonAndAge.png", g1, width=13, height=9)

## Cumulative hospitalizations


g2 <- ggplot(subset(allflusurv, (surveillance_area =="IHSP" & 
                                   region=="Utah")), group=age_label) +
  geom_line(aes(wk_start, rate, color=age_label), lwd=0.9) +
  facet_wrap(~sea_description, scale="free", ncol=4)+
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank() )+
  ylab("Hospitalization rate per 100,000") + xlab("Date") + ggtitle("FluSurvNet")
g2

ggsave("HospitalizationsAnalysis/FluSurvNetCumRatesBySeasonAndAge.png", g2, width=13, height=9)


g2 <- ggplot(subset(allflusurv, (surveillance_area =="EIP" & 
                                   region=="California")), group=age_label) +
  geom_line(aes(wk_start, weeklyrate, color=age_label), lwd=0.9) +
  facet_wrap(~sea_description, scale="free", ncol=4)+
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank() )+
  ylab("Hospitalization rate per 100,000") + xlab("Date") + ggtitle("FluSurvNet")
g2

ggsave("HospitalizationsAnalysis/CABySeasonAndAge.png", g2, width=13, height=9)



g2 <- ggplot(subset(allflusurv, (surveillance_area =="IHSP" & 
                                   region=="Utah")), group=age_label) +
  geom_line(aes(wk_start, rate, color=age_label), lwd=0.9) +
  facet_wrap(~sea_description, scale="free", ncol=4)+
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank() )+
  ylab("Hospitalization rate per 100,000") + xlab("Date") + ggtitle("FluSurvNet")
g2

flusurv202122=flusurv202022 %>% 
  filter(age_label=="Overall" & year==2021) %>%
  filter(!(region=="Entire Network" & surveillance_area=="EIP")) %>%
  filter(!(region=="Entire Network" & surveillance_area=="IHSP")) %>%
    rename(region2=region) %>%
  mutate(region=case_when(region2=="Entire Network" ~ "United States",
                          region2!="Entire Network" ~ as.character(region2))) %>%
  mutate(region=case_when(region %in% c("New York - Albany","New York - Rochester")
                          ~"New York",
                          !(region %in% c("New York - Albany","New York - Rochester"))~region)) %>%
group_by(region,sea_label, sea_description, year_wk_num, year, 
         age, age_label, wk_start, wk_end )  %>%
  summarize(rate=mean(rate,na.rm=T),
            weeklyrate=mean(weeklyrate,na.rm=T)) %>%
   mutate(region=factor(region, levels=c("United States", state.name)))
                          
both_hosp=hhs %>% mutate(region=as.character(region)) %>%
  left_join(flusurv202122, by=c("region","datew"="wk_end")) %>%
  dplyr::rename(flusurv.hosp.rate=weeklyrate,
                flusurv.hosp.cum.rate=rate,
                hhs.hosp.rate=flu_hosp_rate, 
                hhs.death.rate=flu_death_rate)

colors <- c("HHS" = "blue", "Flusurvnet" = "red")


g2 <- ggplot(subset(both_hosp,region=="United States")) +
  geom_line(aes(datew, flusurv.hosp.rate, color="Flusurvnet"), lwd=0.9) +
  geom_line(aes(datew, hhs.hosp.rate, color="HHS"), lwd=0.9) +
  xlim(c(as.Date("2021-09-01"),as.Date("2022-08-01"))) +
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank() )+
  ylab("Hospitalization rate per 100,000") + xlab("Date") + ggtitle("FluSurvNet")
g2



g22 <- ggplot(subset(both_hosp,
                     region %in% c("California","Colorado","Connecticut",
                                   "Georgia", "Maryland","Minnesota",    
              "New Mexico","New York", "Oregon","Tennessee","United States", 
              "Iowa", "Michigan","Ohio","Utah" ))) +
  geom_line(aes(datew, flusurv.hosp.rate, color="Flusurvnet"), lwd=0.9) +
  geom_line(aes(datew, hhs.hosp.rate, color="HHS"), lwd=0.9) +
  xlim(c(as.Date("2021-09-01"),as.Date("2022-08-01"))) +
  labs(x = "Date",
       y = "Hospitalizations per 100,000",
       color = "Legend")+
  facet_wrap(~state, scale="free", ncol=4)+
  theme(
    panel.grid = element_blank(),
    strip.background = element_blank() )+
  theme(legend.position = c(0.8, 0.1))+
 ggtitle("FluSurvNet vs HHS")
g22


ggsave("HospitalizationsAnalysis/HHS_flusurv_flu_state.png", g22, width=15, height=15)

corrs= both_hosp %>% filter(region %in% c("California","Colorado","Connecticut",
                                      "Georgia", "Maryland","Minnesota",    
                                      "New Mexico","New York", "Oregon","Tennessee","United States", 
                                      "Iowa", "Michigan","Ohio","Utah" )) %>%
                              group_by(region) %>% summarize(corr=cor.test(hhs.hosp.rate, flusurv.hosp.rate,na.rm=T)$estimate)

both_hosp=both_hosp %>% dplyr::select(-previous_day_deaths_covid_and_influenza,                           
-previous_day_deaths_covid_and_influenza_coverage,
-(icu_patients_confirmed_influenza:adult_icu_bed_utilization_coverage)) %>%
  rename(date.wk_end=datew,
         date.wk_start=wk_start,
         weekly.flu.admissions.HHS=previous_day_admission_influenza_confirmed,
         weekly.flu.admissions.HHS.coverage=previous_day_admission_influenza_confirmed_coverage,
         weekly.flu.deaths.HHS=previous_day_deaths_influenza,
         weekly.flu.deaths.HHS.coverage=previous_day_deaths_influenza_coverage) %>%
  filter(!is.na(sea_description))

save('both_hosp', file="HospitalizationsAnalysis/Combined_HHS_Flusurvnet_datasets.rda")
save('allflusurv', file="HospitalizationsAnalysis/Flusurvnet_2003_2022_dataset.rda")
save('hhs', file="HospitalizationsAnalysis/HHS_flu_2020_2022_dataset.rda")

write.csv(hhs, file="HospitalizationsAnalysis/HHS_flu_2020_2022_dataset.csv",row.names = F)
write.csv(allflusurv, file="HospitalizationsAnalysis/Flusurvnet_2003_2022_dataset.csv",row.names = F)
write.csv(both_hosp, file="HospitalizationsAnalysis/Combined_HHS_Flusurvnet_datasets_2021_2022.csv",row.names = F)
