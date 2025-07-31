#### Round 1 - 2025-2026 Resources

* `CumulativeDeaths_2022_2025.csv` has weekly cumulative death estimates from 
the CDC model for the 2022-23, 2023-24, and 2024-35 seasons. Estimates are provided as 
2,5 and 97.5% quantiles. This is a model based on reported hospitalizations 
to Flusurvnet, factoring in underreporting and age distribution of 
hospitalizations, and applying an hospitalization fatality rate to obtain deaths. 
This should be considered our death  target.
**Notes:** data for earlier seasons are available in the Rd1_datasets, in the 
[In-season-National-Burden.csv](https://github.com/midas-network/flu-scenario-modeling-hub_resources/blob/main/Rd1_datasets/In-season-National-Burden.csv) 
file.

* `Age_Specific_Coverage_Flu_RD1_2025_26_Sc_A_B.csv` simulates two levels of vaccine coverage
for the 2025-2026 season to be used in round 1 - 2025/2026 (also called round 6). The data in 
this file provides weekly cumulative coverages by state and adult and child age groups to 
apply to scenario A (same coverage as in the 2023-24 season) and B (35% lower than the 2023-24 
flu season in individuals under 65 yrs). Weekly estimates are interpolated based on the reported 
coverage of the flu vaccine in the 2023-2024 flu season using Piecewise Cubic Hermite Interpolating 
Polynomial. The data in this file can be used as is (no adjustment to coverage should be needed). 
Age groups can be collapsed based on provided pop sizes. Week dates (Week_Ending_Sat) are provided as 
the last day of the week, which is the Saturday at the end of an MMWR week. Cumulative coverage is 
provided per 100 population (percent); eg, if flu.coverage.rd2526.sc_A=46.3 it means that 46.3% of a 
given population group is vaccinated. No data is provided for scenario C. Teams should assume 0% 
coverage in all age groups for scenario C.