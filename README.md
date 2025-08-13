# Flu Scenario Modeling Hub - Resources

Repository containing code to process target flu data and auxiliary calibration 
datasets for Scenario Modeling Hub flu rounds.

## Data Description

The repository contains additional data used for 
[SMH Flu round projections](https://github.com/midas-network/flu-scenario-modeling-hub)

- [Archive Data](./archive_data)
    - [CumulativeDeath](./archive_data/CumulativeDeath): Weekly estimates of 
    cumulative influenza-related deaths based on CDC multiplier model for the 
    2022-2023 season.
    - [HospitalizationsAnalysis](./archive_data/HospitalizationsAnalysis): HHS, 
    FluSurv-NET data for the 2003 to 2022 seasons. Including R code to re-run 
    the analysis.
  
- [Code](./code)
    - [VaccineCoverage](./code/VaccineCoverage): code to generate vaccine 
    coverage data
    
- [FluView Data](./fluview)
    - [ILINet and ICL/NREVSS](./fluview/ilinet): Data from 
    [National, Regional, and State Level Outpatient Illness and Viral Surveillance](https://gis.cdc.gov/grasp/fluview/fluportaldashboard.html)
    at National and State level for all available seasons (1997 to 2025),
    downloaded on 2025-08-13.
    - [FluSurv-NET](./fluview/flusurnet): Data from 
    [Laboratory-Confirmed Influenza Hospitalizations](https://gis.cdc.gov/GRASP/Fluview/FluHospRates.html),
    downloaded on 2025-08-13.

- Round Associated Data:
    - [Round 1](./Rd1_datasets)
    - [Round 2](./Rd2_datasets)
    - [Round 4](./Rd4_datasets)
    - [Round 5](./Rd5_datasets)
    - [Round 6](./Rd4_datasets)

## License

All source code specific to the overall project is available under an 
open-source MIT license. Some items might be available under different terms 
and licenses. Please consult these licenses before using it to ensure that you 
follow the terms under which they were released.

## Contributing

Please feel free to open an issue if you identify any issue or would like to 
suggest an idea/improvement.

## Funding

Scenario modeling groups are supported through grants to the contributing 
investigators.

The Scenario Modeling Hub site is supported by the 
[MIDAS Coordination Center](https://midasnetwork.us/), 
NIGMS Grant U24GM132013 to the University of Pittsburgh.
