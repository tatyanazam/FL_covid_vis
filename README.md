

# Visualizing total and daily case trends of COVID-19 cases in Florida
Author: Tatyana Zamkovaya

## Download data
To visualize these trends, data was first downloaded or manually curated.
The case trends were created by using the Florida Department of Health (FDOH) dataset which includes the state's COVID-19 cases and deaths (which includes the gender, age, age group of each case) per county from March 10 to July 15.
[accessed here](https://open-fdoh.hub.arcgis.com/datasets/florida-covid19-case-line-data)

The dataframe of these trends is called `flcovid_mask_df` and can be downloaded here in .RData format.

To visualize florida counties as a map, county coordinates were retrieved and stored as a separate dataframe (`counties2.RData`) using the following code:
```r 
library(sf)
counties <- sf::st_as_sf(map("county", plot = FALSE, fill = TRUE))
counties <- subset(counties, grepl("florida", counties$ID))
counties$area <- as.numeric(st_area(counties))
counties2 <- counties %>% separate(ID, c("state", "county"), sep=",")

#All counties were then capitalized and counties like Miami-Dade, DeSoto, and Palm Beach were then manually changed so that they fit the fl_covid_data format. 
```

A dataframe `fl_county_mask_date_df` of counties and whether or not they currently have mandatory mask ordinances enforced was also created and can be downloaded here in .RData format. 
A dataframe `fl_covid_data_f` of just the gender, age, and age group trends per day per County (removing all other information) and adding a new column (num_case) with 1 added to each entry was then used to visualize daily trends per County and again is available here in .RData format.


## Import necessary libraries/data
```r 
library(data.table)
library(dplyr)
library(tidyverse)
library(ggplot2) 
library(tmap)
library(sf)
library(raster)
library(spData)
library(usmap) #to get back initial data on florida county coordinates
devtools::install_github("UrbanInstitute/urbnmapr")
library(urbnmapr) #for easier plotting of florida map
library(tigris) #for easier plotting of florida map
library(ggsn)
library(RColorBrewer)
library(viridis)
``` 
## Map of the Total Cases per County, including mask ordinance information
I wanted to see which counties had the highest number of total COVID-19 cases and wanted to also visualize which counties currently had mask orders enforced.

![florida_covid_map](https://github.com/tatyanazam/FL_covid_vis/blob/master/total_case_map.png)

Although Miami clearly stands out as being heavily hit by COVID-19 cases, even the lighter shaded counties all have a high total case count (10000 and above). 
Additionally, though the most heavily hit regions currently enforce masks, most counties (46 out of 67) currently do not have mandatory mask orders. Some counties, like Hillsborough and Pinellas only started enforcing masks beginning on June 24th. These counties, though not quite as high as Miami, are still quite heavily hit and potentially could've had reduced total cases if masks were enforced sooner. 

While masks are encouraged by all counties, **mandatory mask orders are needed** to reduce risk of COVID-19 throughout Florida. 


## Daily Case Numbers Per County from March to July
For each day, for each county that did not record any cases, I first added an entry of 0 to the column of case counts so that case counts could be accurately compared. Then, I used `geom_tile` from `ggplot2` to visualize changes in daily case numbers for all Florida counties.

![county_daily_case_change](https://github.com/tatyanazam/FL_covid_vis/blob/master/case_count_per_day.png)  

The y-axis shows Florida counties (listed alphabetically) and x-axis shows from left to right the change over time in days from March 10 to July 15. 

Visualizing changes in daily cases numbers shows that Miami, Hillsborough, and Broward have particularly seen a rise in case numbers and that overall, most counties are experiencing a *dramatic rise* in case numbers, with up to **1000-2000 cases recorded per day** since July. 

## Change in Age Group Distribution in Cases per County from March to July

Focusing on the top 10 counties with the highest number of COVID-19 cases, I wanted to see whether or not a change in age group distribution occurred in daily cases. 
Barplots, colored by age group, would help indicate whether cases were evenly distributed among age groups or not and evaluating the age group distribution over time would help determine which age groups are most affected, and at which time points, for each Florida county.

![age_group_change_top_10_counties](https://github.com/tatyanazam/FL_covid_vis/blob/master/Change_in_age_group_dist_of_cases_for_top10covidcounties.png)

As can be seen from the plot, as time has gone by, a higher proportion of cases is being recorded among younger age groups, **particularly among 15-44 year olds**.
Though the elderly population were most heavily affected by coronavirus in countries like Italy and China, _this does not appear to be the case (in terms of positive case distributions) in Florida_, despite the high retiree population and high number of nursing homes. Instead, residents aged 15-54 are testing positive the most, particularly in June/July. 

## Change in Age Group Distribution per County just for dates in July

To more clearly see the change in age group distribution for these top 10 COVID-19 Florida counties, I focused on the last 15 days (July 1-July 15)

![age_group_change_july](https://github.com/tatyanazam/FL_covid_vis/blob/master/just_july_dates_age_dist_changes.png)

In July, the highest proportion of cases has been consistently skewed towards younger age groups


To recreate these plots or to further explore Florida coronavirus data, please download the flcovid_code_for_plots.R file and .RData files found here.



 
