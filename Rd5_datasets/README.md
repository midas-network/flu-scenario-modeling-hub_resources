#### Round 1 - 2024-2025 Resources

* `Age_Specific_Coverage_Flu_RD1_2024_25_Sc_A_B_C_D_E_F.csv` simulates three 
levels of vaccine coverage for the 2024-2025 season to be used in round 1 - 
2024/2025 (also called round 5). The data in this file provides weekly 
cumulative coverages by state and adult and child age groups to apply to 
scenario A and B (20%  higher than the 2022-23 flu season), 
scenario C and D (same as in 2022-23 flu season), and scenario E and F 
(20%  lower than the 2022-23 flu season). Estimates are based on the 
reported coverage of the flu vaccine in the 2022-2023 flu season. 
The data in this file can be used as is (no adjustment to coverage should 
be needed). Age groups can be collapsed based on provided pop sizes.


* `HHS_flu_2020_2024_dataset` is the national and state-specific data collected 
by the 
[HHS Protect Public Data Hub](https://healthdata.gov/Hospital/COVID-19-Reported-Patient-Impact-and-Hospital-Capa/g62h-syeh/about_data) 
showing influenza lab-confirmed admissions in hospitals in 2020-2024. 
The data in this file provides estimates for weekly flu admissions (derived from
`"previous_day_admission_influenza_confirmed"`), and in-patient flu deaths 
(`"previous_day_deaths_influenza"`), as well as hospital coverages (no 
hospitals reporting each week) for each outcome. Note that the dataset was 
paused in May 2024 so there will be no recent data for calibration.


* `CumulativeDeaths_2022_2024.csv` has weekly cumulative death estimates from 
the CDC model for the 2022-23 and 2023-24 seasons. Estimates are provided as 
2,5 and 97.5% quantiles. This is a model based on reported hospitalizations 
to Flusurvnet, factoring in underreporting and age distribution of 
hospitalizations, and applying an hospitalization fatality rate to obtain deaths. 
This should be considered our death  target.
**Notes:** data for earlier seasons are available in the Rd1_datasets, in the 
[In-season-National-Burden.csv](https://github.com/midas-network/flu-scenario-modeling-hub_resources/blob/main/Rd1_datasets/In-season-National-Burden.csv) 
file
