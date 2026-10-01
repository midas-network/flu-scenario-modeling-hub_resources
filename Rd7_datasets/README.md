#### Round 1 - 2026-2027 Resources

* `CumulativeDeaths_2022_2026` has weekly cumulative death estimates from 
the CDC model for the 2022-23, 2023-24, 2024-25, and 2025-26 season. Estimates 
are provided as 2,5 and 97.5% quantiles. This is a model based on reported 
hospitalizations to Flusurvnet, factoring in underreporting and age distribution of 
hospitalizations, and applying an hospitalization fatality rate to obtain deaths. 
This should be considered our death  target.
**Notes:** data for earlier seasons are available in the Rd1_datasets, in the 
[In-season-National-Burden.csv](https://github.com/midas-network/flu-scenario-modeling-hub_resources/blob/main/Rd1_datasets/In-season-National-Burden.csv) 
file.

* `flu_RD7_Vaccination_curves.csv` simulates two levels of vaccine coverage
for the 2024-2025 season and 2019-2020 season to be used in round 1 - 2026/2027 (also called round 7). 
The data in this file provides weekly cumulative coverages by state and adult and child age groups to 
apply to scenario A (same coverage as in the 2019-20 season), B (same coverage as in the 2024-25 season)
and C (same coverage as in the 2024-25 season, but no children vaccination). Weekly estimates are 
interpolated based on the reported coverage of the flu vaccine in the 2019-2020 and 2024-2025 flu seasons 
using Piecewise Cubic Hermite Interpolating Polynomial. The data in this file can be used as is 
(no adjustment to coverage should be needed). 
Age groups can be collapsed based on provided pop sizes. Week dates (Week_Ending_Sat) are provided as 
the last day of the week, which is the Saturday at the end of an MMWR week. Cumulative coverage is 
provided per 100 population (percent); eg, if flu.coverage.sc_A=46.3 it means that 46.3% of a 
given population group is vaccinated. No data is provided for scenario D. Teams should assume 0% 
coverage in all age groups for scenario D.