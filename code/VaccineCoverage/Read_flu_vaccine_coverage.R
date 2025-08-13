library(lubridate)
library(dplyr)
library(tidyr)
library(data.table)
library(rlist)
library(ggplot2)
library(mgcv)
library(splines)
library(MASS)
library(stats)
library(zoo)
library(stringr)
library(signal)
library(pracma)


rm(list = ls())

## Adult flu vax (monthly data, 1 season 2021-2022)
## will assign date to midpoint of the month in which each survey was taken
download.file("https://data.cdc.gov/api/views/3myw-4j4q/rows.csv?accessType=DOWNLOAD","FluCoverage.csv")
#https://data.cdc.gov/Vaccinations/Cumulative-Influenza-Coverage-by-Race-Ethnicity-an/3myw-4j4q

flu2021=read.csv('FluCoverage.csv') %>%
  dplyr::select(-Current_Season_Week_Ending_Label) %>%
  mutate(Date=as.Date(as.character(Week_Ending), format='%m/%d/%Y')+days(-15),
         Age_Group=factor(Age_Group,levels=c("All Adults (18+)","18-49 yrs",
                                             "50-64 yrs","65+ yrs")))   %>%
  dplyr::filter(Geographic_Level !="City" & Race=="Overall")  %>%
  arrange(Geography, Age_Group, Race, Date)

dates_sun=seq(as.Date("2021-07-15"),as.Date("2022-03-13"), by="week" )

flu2021b <- flu2021 %>% 
  complete(nesting(Geography, Age_Group), Date = dates_sun) %>%
  mutate(Month=month(Date),
         Estimate = case_when(Date< as.Date("2021-08-15") ~ 0,
                            Date== as.Date("2021-08-15") ~ 1.1,
                           Date > as.Date("2021-08-15") ~ Point_Estimate)) %>% 
                          arrange(Geography, Age_Group,Date) %>%
  group_by(Geography, Age_Group) %>%
    mutate(Estimate.int=na.spline(Estimate, na.rm = FALSE),
           Estimate.roll=rollmeanr(Estimate.int, 3, align = "right", fill=NA),
           Estimate.spl=fitted(smooth.spline(Date, Estimate.int, df=3)))


p <- ggplot(subset(flu2021b, Age_Group %in% c("All Adults (18+)",
                                              "18-49 yrs",  "50-64 yrs", "65+ yrs" ) &
                     Geography != "Virgin Islands"), 
            aes(group=Age_Group)) +
  geom_line(aes(x=Date, y=Estimate.int, color = Age_Group, linetype = Age_Group, size=.8)) + 
  geom_line(aes(x=Date, y=Estimate.spl, color = Age_Group,  size=1.2)) + 
  geom_point(aes(x=Date, y=Estimate, color = Age_Group, size=1.5)) + 
    facet_wrap(vars(Geography)) +
  scale_size_identity()+
  ylab("Coverage")+ xlab("Week in 2021-22")+ ylim(0,100)+
  scale_x_date(date_labels = "%b")+
ggtitle("Monthly flu vaccine ramp up, 2021-2022, by age")
print(p)


ggsave("AdultsFluCoverageInterpolatedWeekly.png", p, width=13, height=13)

## kids flu vax (weekly data, 3 seasons)
download.file("https://data.cdc.gov/api/views/eudc-n39h/rows.csv?accessType=DOWNLOAD","FluCoverageKids.csv")
#https://data.cdc.gov/Vaccinations/Weekly-Cumulative-Influenza-Vaccination-Coverage-C/eudc-n39h

fluk=read.csv('FluCoverageKids.csv') %>%
  dplyr::select(-Current_Season_Week_Ending_Label) %>%
  mutate(Date=as.Date(as.character(Week_Ending), format='%m/%d/%Y'),
         Day=day(Date),
         Month=month(Date),
         Add=case_when(
  Date < as.Date("2020-09-01") ~ 730,
  Date < as.Date("2021-09-01") ~ 365,
  Date < as.Date("2022-09-01") ~0),
         Date2=Date + days(Add))   %>%
  dplyr::filter(Geographic_Level !="City" & Race_Ethnicity=="Overall" )  %>%
  arrange(Geography_Name,Race_Ethnicity, Date)

library(stringr)
#https://data.cdc.gov/Flu-Vaccinations/Influenza-Vaccination-Coverage-for-All-Ages-6-Mont/vh55-3he6
download.file("https://data.cdc.gov/api/views/vh55-3he6/rows.csv?accessType=DOWNLOAD","FluCoverageAll.csv")

Sys.setlocale("LC_ALL", "pt_PT.UTF-8")

flu=read.csv('FluCoverageAll.csv', encoding="UTF-8") %>%  
  dplyr::rename(Estimate=Estimate...., CI95= X95..CI....) %>%
  mutate(Month=as.numeric(Month),
        year1 = as.numeric(substr(Season.Survey.Year,1,4)),
         year2 = as.numeric(paste(substr(Season.Survey.Year,1,2),substr(Season.Survey.Year,6,7), sep="")),
         date=case_when(Month>7 ~ lubridate::ceiling_date(make_date(year1, Month, 1),"month")-1,
                        Month<8 ~ lubridate::ceiling_date(make_date(year2, Month, 1),"month")-1),
        FIPS=as.numeric(FIPS), Estimate=as.numeric(as.character(Estimate)),
        Estimate=case_when((is.na(Estimate) & Month==8) ~ 1.1,
                       !is.na(Estimate) ~ Estimate),
        Dimension=as.character(Dimension))%>%
        mutate(Dimension=case_when(Dimension=="\u226565 Years" ~ "65+ Years",
                            Dimension=="\u22656 Months" ~ "6+ Months",
                            Dimension=="\u226518 Years" ~ "18+ Years",
                            !(Dimension %in% c("\u2265,65 Years",
                                                "\u22656 Months",
                                                "\u226518 Years")) ~ Dimension))%>%
  dplyr::filter(Dimension.Type=="Age" & Geography.Type == "States/Local Areas" &FIPS<100
         & Geography!="U.S. Virgin Islands" & 
           Vaccine=="Seasonal Influenza" & !(Month %in% c(6,7))) %>%
  mutate(Dimension=factor(Dimension, levels=c("6 Months - 17 Years" , "18-49 Years" , 
                                              "50-64 Years" ,  "65+ Years" ,
                                              "6+ Months","18+ Years", "18-49 Years at High Risk" ,
                                                "18-49 Years not at High Risk", 
                                              "18-64 Years", "18-64 Years at High Risk",
                                                    "18-64 Years not at High Risk",
                                              "6 Months - 4 Years",  "5-12 Years" ,"13-17 Years"))) %>%
  dplyr::rename(Age=Dimension)%>% 
  dplyr::filter(Age %in% c("6 Months - 4 Years",  "5-12 Years" ,"13-17 Years", 
                           "6 Months - 17 Years" , "18-49 Years" , 
             "50-64 Years" ,  "65+ Years")) %>% 
  arrange(Season.Survey.Year, Geography, Age, date) %>%
  group_by(Season.Survey.Year, Geography, Age) %>%
  mutate(Estimate.int=na.spline(Estimate, na.rm = FALSE),
         Estimate.roll=rollmeanr(Estimate, 3, align = "right", fill=NA),
         Estimate.spl=fitted(smooth.spline(date, Estimate.int, df=5)))




dates_sun=seq(as.Date("2020-08-16"),as.Date("2021-06-05"), by="week" )

try= flu %>%  dplyr::filter(Age %in% c("6 Months - 4 Years",  "5-12 Years" ,"13-17 Years",
                                       "6 Months - 17 Years", "18-49 Years" ,  "50-64 Years" ,
                                       "65+ Years") & 
                        Season.Survey.Year =="2020-21" ) %>%
  mutate(Estimate.trunc=case_when(Month %in% c(7) ~ 0,
                                  Month %in% c(8, 9, 10, 11,12,1,2,3,4,5) ~ Estimate.int)) %>%
    arrange(Season.Survey.Year, Geography, Age, date) 


## Interpolation from monthly to weekly data using pchip functions

big_mat= try %>%  dplyr::select(Season.Survey.Year, Geography, Age) %>%
  ungroup() %>% 
  tidyr::expand(nesting(Season.Survey.Year, Geography, Age), dates_sun) %>%   
  dplyr::rename(date=dates_sun) %>%
  arrange(Season.Survey.Year, Geography, Age, date) %>%
  mutate(Estimate.pchip=0)
  
for (state in unique(big_mat$Geography)){
    for (age in unique(big_mat$Age)){ 
      idx1=which(big_mat$Age==age & big_mat$Geography==state)
      idx2=which(try$Age==age & try$Geography==state)
      big_mat$Estimate.pchip[idx1]=pchip(as.numeric(try$date[idx2]), try$Estimate.trunc[idx2], as.numeric(dates_sun))}}
 
  big_mat2=big_mat %>% bind_rows(try) %>% 
  arrange(Season.Survey.Year, Geography, Age, date)  %>% 
    group_by(Season.Survey.Year, Geography, Age)  %>% 
      mutate(Month=month(date),
    forced_zero_extr=case_when(Month %in% c(7) ~ "YES",
                        Month %in% c(8, 9, 10, 11,12,1,2,3,4,5) ~ "NO"),
    Estimate.pchip2=na.approx(Estimate.pchip, na.rm=F, rule=2))
  
  p <- ggplot(subset(big_mat2, Age %in% c("6 Months - 4 Years",  "5-12 Years" ,"13-17 Years",
                                          "6 Months - 17 Years", "18-49 Years",
                                     "50-64 Years",  "65+ Years") & 
                       Season.Survey.Year =="2020-21" &
                       Geography %in% c("California", "New York", "Florida","Louisiana",
                                        "Vermont","Michigan") &
                       !(Month %in% c(6,7))), 
              aes(x=date,group=Age)) + 
    geom_line(aes(y=na.approx(Estimate.pchip*.9,na.rm=F, rule=2), color = Age),  linetype= "solid", size=1) + 
    geom_point(aes(y=na.approx(Estimate.pchip2*.9,na.rm=F, rule=2), color = Age),  shape=19) + 
        #geom_point(aes(y=Estimate.pchip*.9, color = Age),  size=1, shape=19) + 
    geom_point(aes(y=Estimate.int*.9, color = Age),  size=2, shape=19) + 
    #geom_point(aes(y=Estimate.int, color = Age,  size=1.2)) + 
    geom_point(aes(y=jitter(Estimate.trunc*.9, amount=1), color = Age,  shape=forced_zero_extr, size=2)) + 
    facet_wrap(vars(Geography), nrow=2) +
    scale_size_identity()+
    ylab("Coverage (%)")+ xlab("Month in 2022-23")+
    scale_x_date(date_labels = "%b")+
    geom_hline(yintercept = 68, linetype=2) +
    geom_hline(yintercept = 68*.85, linetype=2)
  #+    ggtitle("Projection of COVID vaccine ramp up, 2022-2023, by age")
  print(p)
  
  
  
  
  
  ggsave("AdultsFluCoverageAllstatesPCHIP0.9.png", p, width=13, height=8)
  
## Reading population files  

states=read.csv("state abbreviations.csv")

state_pop_data <- read.csv("state_pop_data.csv")

pop.k0_4=state_pop_data %>% dplyr::filter(AGE<5 & SEX == 0 & NAME != "United States") %>% 
  group_by(NAME) %>% 
  mutate(w_pop=case_when(AGE==0 ~ POPEST2020_CIV*0.5, #0-6mo do not get vaccinated
                         AGE>0 ~ as.numeric(POPEST2020_CIV))) %>%
  dplyr::summarise(pop=round(sum(w_pop))) %>%
  left_join(states, by = c("NAME" = "region"))  %>% 
  mutate(Age="6 Months - 4 Years")  %>% 
  dplyr::select(NAME, pop, Age)

pop.k5_12=state_pop_data %>% dplyr::filter(AGE>4 & AGE<13 & SEX == 0 & NAME != "United States") %>% 
  group_by(NAME) %>% 
  mutate(w_pop=POPEST2020_CIV) %>% 
  dplyr::summarise(pop=round(sum(w_pop))) %>%
  left_join(states, by = c("NAME" = "region"))  %>% 
  mutate(Age="5-12 Years")  %>% 
  dplyr::select(NAME, pop, Age)  

pop.k13_17=state_pop_data %>% dplyr::filter(AGE>12 & AGE<18 & SEX == 0 & NAME != "United States") %>% 
  group_by(NAME) %>% 
  mutate(w_pop=POPEST2020_CIV) %>% 
  dplyr::summarise(pop=round(sum(w_pop))) %>%
  left_join(states, by = c("NAME" = "region"))  %>% 
  mutate(Age="13-17 Years")  %>% 
  dplyr::select(NAME, pop, Age) 

pop.k0_17=state_pop_data %>% dplyr::filter(AGE<18 & SEX == 0 & NAME != "United States") %>% 
  group_by(NAME) %>% 
  mutate(w_pop=case_when(AGE==0 ~ POPEST2020_CIV*.5,  #0-6mo do not get vaccinated
                         AGE>0 ~ as.numeric(POPEST2020_CIV))) %>% 
  dplyr::summarise(pop=round(sum(w_pop))) %>%
  left_join(states, by = c("NAME" = "region"))  %>% 
  mutate(Age="6 Months - 17 Years")  %>% 
  dplyr::select(NAME, pop, Age) 

pop.a18_49=state_pop_data %>% dplyr::filter(AGE>17 & AGE<50 & SEX == 0 & NAME != "United States") %>% 
  group_by(NAME) %>% 
  mutate(w_pop=POPEST2020_CIV) %>% 
  dplyr::summarise(pop=round(sum(w_pop))) %>%
  left_join(states, by = c("NAME" = "region"))  %>% 
  mutate(Age="18-49 Years")  %>% 
  dplyr::select(NAME, pop, Age) 

pop.a50_64=state_pop_data %>% dplyr::filter(AGE>49 & AGE<65 & SEX == 0 & NAME != "United States") %>% 
  group_by(NAME) %>% 
  mutate(w_pop=POPEST2020_CIV) %>% 
  dplyr::summarise(pop=round(sum(w_pop))) %>%
  left_join(states, by = c("NAME" = "region"))  %>% 
  mutate(Age="50-64 Years")  %>% 
  dplyr::select(NAME, pop, Age) 

pop.a65=state_pop_data %>% dplyr::filter(AGE>64 & AGE<999 & SEX == 0 & NAME != "United States") %>% 
  group_by(NAME) %>% 
  mutate(w_pop=POPEST2020_CIV) %>% 
  dplyr::summarise(pop=round(sum(w_pop))) %>%
  left_join(states, by = c("NAME" = "region"))  %>% 
  mutate(Age="65+ Years")  %>% 
  dplyr::select(NAME, pop, Age)

pops= pop.k0_4 %>% bind_rows(pop.k5_12) %>% bind_rows(pop.k13_17) %>% bind_rows(pop.k0_17) %>% bind_rows(pop.a18_49) %>%
  bind_rows(pop.a50_64) %>% bind_rows(pop.a65) %>%
  arrange(NAME, Age)

dates_sat=seq(as.Date("2020-06-07"),as.Date("2021-05-30"), by="week" )

big_mat3=big_mat2 %>% dplyr::filter(is.na(Vaccine) & Geography!="Guam" & Geography !="Puerto Rico" &
                                       Age %in% c("6 Months - 4 Years",  "5-12 Years" ,"13-17 Years", "6 Months - 17 Years", "18-49 Years","50-64 Years","65+ Years")) %>%
  ungroup() %>%
  complete(nesting(Geography, Age), date = dates_sat) %>% 
  left_join(pops, by=c("Geography"="NAME", "Age")) %>% 
  mutate(boost.coverage.rd1.sc_A_B=case_when(is.na(Estimate.pchip) ~0,
                                  !is.na(Estimate.pchip) ~Estimate.pchip*1.1),
         boost.coverage.rd1.sc_C_D=case_when(is.na(Estimate.pchip) ~0,
                                  !is.na(Estimate.pchip) ~Estimate.pchip*0.9),
         date_sat=date+days(365*2+4),
         Age=factor(Age, levels=c("6 Months - 4 Years",  "5-12 Years" ,"13-17 Years","6 Months - 17 Years", "18-49 Years","50-64 Years","65+ Years"))) %>%
  ungroup() %>%
  arrange(Geography, Age, date_sat) %>%
    dplyr::select(Geography, Age, pop, date_sat, boost.coverage.rd1.sc_A_B, boost.coverage.rd1.sc_C_D) %>%
  rename(Population=pop,Week_Ending_Sat=date_sat,
         Boost.coverage.rd1.sc_A_B=boost.coverage.rd1.sc_A_B,
         Boost.coverage.rd1.sc_C_D=boost.coverage.rd1.sc_C_D)

## Estimate national coverage as a weighted mean of each state's coverage
big_mat_us=big_mat3 %>%
  group_by(Age, Week_Ending_Sat) %>%
  dplyr::summarise(Population2=sum(Population),
            Boost.coverage.rd1.sc_A_B=weighted.mean(Boost.coverage.rd1.sc_A_B, w=Population),
            Boost.coverage.rd1.sc_C_D=weighted.mean(Boost.coverage.rd1.sc_C_D, w=Population)) %>%
  rename(Population=Population2) %>%
  mutate(Geography="United States") %>% dplyr::filter(Geography != "NA")


big_mat4= big_mat3 %>% bind_rows(big_mat_us) %>% 
  mutate(Geography=factor(Geography, levels=c("United States",state.name, "District of Columbia"))) %>%
  dplyr::filter(Week_Ending_Sat > as.Date("2022-08-01")) %>%
  group_by(Geography,Age, Population, Week_Ending_Sat) %>%
  mutate(Boost.coverage.rd1.sc_C_D=max(0,Boost.coverage.rd1.sc_C_D,na.rm=T),
        Boost.coverage.rd1.sc_A_B=max(0,Boost.coverage.rd1.sc_A_B,na.rm=T))

p <- ggplot(subset(big_mat4, Geography != "NA"), 
            aes(x=Week_Ending_Sat,group=Age)) + 
  geom_line(aes(y=Boost.coverage.rd1.sc_C_D, color = Age),  linetype= "solid", size=1) + 
  geom_line(aes(y=Boost.coverage.rd1.sc_A_B, color = Age),  linetype= "dashed", size=1) + 
    facet_wrap(vars(Geography), nrow=7) +
  scale_size_identity()+
  ylab("Coverage (%)")+ xlab("Month in 2022-23")+
  scale_x_date(date_labels = "%b")+
  geom_hline(yintercept = 68, linetype=2) +
  geom_hline(yintercept = 68*.85, linetype=2)
#+    ggtitle("Projection of COVID vaccine ramp up, 2022-2023, by age")
print(p)

ggsave("FluCoverageAllstates_fluRd1_Sc_A_D.png", p, width=14, height=12)


ggplot(subset(big_mat4, Geography == "United States"), 
       aes(x=Week_Ending_Sat,group=Age)) + 
  geom_line(aes(y=Boost.coverage.rd1.sc_C_D, color = Age),  linetype= "solid", size=1) + 
  geom_line(aes(y=Boost.coverage.rd1.sc_A_B, color = Age),  linetype= "dashed", size=1) + 
  facet_wrap(vars(Geography), nrow=7) +
  scale_size_identity()+
  ylab("Coverage (%)")+ xlab("Month in 2022-23")+
  scale_x_date(date_labels = "%b")+
  geom_hline(yintercept = 68, linetype=2) +
  geom_hline(yintercept = 68*.85, linetype=2)
#+    
write.csv(big_mat4,  row.names =F, "Age_Specific_Coverage_Flu_RD1_2022_23_Sc_A_B_C_D.csv")

