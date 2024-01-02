# SurvStat MultiDim

R functions and examples for querying the Robert Koch Institute's (RKI) SurvStat dataset. 
This dataset contains aggregated confirmed case numbers reported to RKI in Germany for a number of notifiable infectious diseases. They are stratified along a number of dimensions such as sex, age group, district (Landkreis).

See [https://survstat.rki.de](https://survstat.rki.de). 
While one can click a query together on the web page and then download the resulting table, this can be unstable, and tiresome when one wants multidimensional data, e.g., counts along sex and age group.

Queries can be long to execute.

Please see explanations and details in the scripts.

These scripts were originally written in 2020 by a developer who wishes to remain anonymous.

N.B. 1: Matching RKI's districts (Landkreise) with other datasets, such as official population data, can be tricky. Population can be recovered by dividing case counts by relative incidence and multiplying by 100,000.

N.B. 2: Some districts have been redrawn or have been added during the time covered by the dataset. When a district has been split, the RKI constructs time series for the two resulting districts over the whole time range by randomly assigning cases reported before the split with 50% probability to either district as if it already existed.  

Code made available with a [CC0](https://creativecommons.org/public-domain/cc0/) license. 
