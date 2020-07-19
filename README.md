

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

load("fl_county_mask_date_df.RData")
load("flcovid_mask_df.RData")
load("counties2.RData")
load("fl_covid_data_f.RData")

````
## Map of the Total Cases per County, including mask ordinance information
I wanted to see which counties had the highest number of total COVID-19 cases and wanted to also visualize which counties currently had mask orders enforced.

![florida_covid_map](https://github.com/tatyanazam/FL_covid_vis/blob/master/total_case_map.png)

Although Miami clearly stands out as being heavily hit by COVID-19 cases, even the lighter shaded counties all have a high total case count (10000 and above). 
Additionally, though the most heavily hit regions currently enforce masks, most counties (46 out of 67) currently do not have mandatory mask orders. Some counties, like Hillsborough and Pinellas only started enforcing masks beginning on June 24th. These counties, though not quite as high as Miami, are still quite heavily hit and potentially could've had reduced total cases if masks were enforced sooner. 

While masks are encouraged by all counties, **mandatory mask orders are needed** to reduce risk of COVID-19 throughout Florida. 

```r 
flcovid_mask_df <- merge(fl_covid_data, fl_county_mask_date_df, by.x = "County", by.y="CountyName", all.x=TRUE)

total_case_count_per_fl_df <- flcovid_mask_df %>% group_by(County) %>% count() %>% rename(total_cases = n, county_name = County)
total_case_count_per_fl_w_dates_df <- flcovid_mask_df %>% group_by(County, date) %>% count() %>% rename(total_cases = n, county_name = County)

counties2_v2 <- tigris::geo_join(counties2, total_case_count_per_fl_df, "CountName", "county_name")
counties2_w_dates_v2 <- tigris::geo_join(counties2, total_case_count_per_fl_w_dates_df, "CountName", "county_name")
counties2_v2_w_masks <- tigris::geo_join(counties2_v2, fl_county_mask_date_df, "CountName", "county_name")

#map with counties filled in by # total cases and outlined by whether or not masks are enforced by mandatory order
ggplot() + geom_sf(data = counties2_v2_w_masks,
 aes(fill=total_cases, color=masks)) + 
  ggtitle("Total Cases per Florida County - 07/15/2020") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_viridis_c(option ="magma", 
                       direction=-1,
                       breaks = c(0,10000,20000, 30000,40000, 50000,60000)) + 
  blank() + 
  scale_color_manual(values= c("lightblue", "black")) +
  scalebar(counties2_v2, dist=50,dist_unit="mi",
           transform=TRUE, model="WGS84", 
           location="bottomleft", st.size=3) + 
           guides(fill=guide_legend(title="Total Cases"), 
         color=guide_legend(title="Mandatory Mask Order"))
         
```
## Daily Case Numbers Per County from March to July
For each day, for each county that did not record any cases, I first added an entry of 0 to the column of case counts so that case counts could be accurately compared. Then, I used `geom_tile` from `ggplot2` to visualize changes in daily case numbers for all Florida counties.

![county_daily_case_change](https://github.com/tatyanazam/FL_covid_vis/blob/master/case_count_per_day.png)  

The y-axis shows Florida counties (listed alphabetically) and x-axis shows from left to right the change over time in days from March 10 to July 15. 

Visualizing changes in daily cases numbers shows that Miami, Hillsborough, and Broward have particularly seen a rise in case numbers and that overall, most counties are experiencing a *dramatic rise* in case numbers, with up to **1000-2000 cases recorded per day** since July. 

``` r
full_counts_per_date_l <- lapply(unique(fl_covid_data_f$date), function(date_c){
newdf <- fl_covid_data_f[fl_covid_data_f$date == date_c,] %>% group_by(County) %>% count()
newdf$date = date_c
#print(date_c)
if(length(setdiff(unique(fl_county_mask_date_df$county_name), newdf$County)) > 0){
  add_l <- NULL
  for(county in setdiff(unique(fl_county_mask_date_df$county_name), newdf$County)){
    print(county)
    add_df <- data.frame(County = county, 
                         n = as.numeric(0),
                         date = date_c)
    print(add_df)
    add_l[[county]] = add_df
  }
  add_l_df <- do.call(rbind, add_l)
  add_l_df$County <- as.character(add_l_df$County)
  #add_l_df$n <- as.integer(add_l_df$n)
  add_l_df$date <- as.character(add_l_df$date)
  newdf <- rbind(as.data.frame(newdf), add_l_df)
}
#print(setdiff(unique(fl_county_mask_date_df$county_name), newdf$County))
else{
  newdf = newdf
}
return(newdf)
})

full_case_counts_df <- dplyr::bind_rows(full_counts_per_date_l, .id="column_label")
full_case_counts_df <- full_case_counts_df %>% 
  separate(date, into=c("year", "month", "day"), 
           sep= "-") %>% 
  unite("newdate", c(month, day))
  
#heatmap like plot of rise in daily case # across counties
ggplot(full_case_counts_df[full_case_counts_df$County != "Unknown County",], 
       aes(as.factor(newdate), reorder(County, desc(County)), fill = n)) + 
  geom_tile(size=1, stat="identity") +
  scale_fill_distiller(palette="Spectral") + 
  ggtitle("Daily cases per County") + 
  theme(axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank()) + 
 labs(fill="Total Daily Cases")
```

## Change in Age Group Distribution in Cases per County from March to July

Focusing on the top 10 counties with the highest number of COVID-19 cases, I wanted to see whether or not a change in age group distribution occurred in daily cases. 
Barplots, colored by age group, would help indicate whether cases were evenly distributed among age groups or not and evaluating the age group distribution over time would help determine which age groups are most affected, and at which time points, for each Florida county.

![age_group_change_top_10_counties](https://github.com/tatyanazam/FL_covid_vis/blob/master/Change_in_age_group_dist_of_cases_for_top10covidcounties.png)

As can be seen from the plot, as time has gone by, a higher proportion of cases is being recorded among younger age groups, _particularly among 15-44 year olds_.
Though the elderly population were most heavily affected by coronavirus in countries like Italy and China, this does not appear to be the case (in terms of positive case distributions) in Florida, despite the high retiree population and high number of nursing homes. Instead, residents aged 15-54 are testing positive the most, particularly in June/July. 

``` r
fl_covid_data_f %>% group_by(County) %>% count() %>% arrange(desc(n)) -> top_10_covid_counties_df #order counts of case # from county with highest # counts to lowest 
top10_counties <- top_10_covid_counties_df$County[1:10] #take top 10 counties with highest # of cases
fl_covid_data_f$Age_group <- as.factor(fl_covid_data_f$Age_group) #convert age group variable to factor and reorder age ranges
fl_covid_data_f$Age_group <- factor(fl_covid_data_f$Age_group, levels= c("0-4 years", "5-14 years", "15-24 years", "25-34 years", "35-44 years", "45-54 years", "55-64 years", "65-74 years", "75-84 years", "85+ years", "Unknown"))
#barplot of age distribution change in daily cases from March-July 
#for top 10 counties with highest # cases 
ggplot(fl_covid_data_f[fl_covid_data_f$County %in% top10_counties,], 
       aes(reorder(as.factor(date), desc(date)), num_case)) + 
  geom_bar(aes(fill= Age_group), stat='identity', position='stack') + 
  scale_fill_brewer(palette="Spectral", direction=-1) + xlab("Date (3/10/2020-07/15/2020)") + 
  ylab("Daily Number of Cases") +
  facet_wrap(~County, ncol=2) + theme(axis.text.y = element_blank(), 
                                      axis.ticks.x = element_blank(),
                                      axis.ticks.y = element_blank()) + 
  coord_flip() + labs(fill="Age Group")
```


## Change in Age Group Distribution per County just for dates in July

To more clearly see the change in age group distribution for these top 10 COVID-19 Florida counties, I focused on the last 15 days (July 1-July 15)

![age_group_change_july](https://github.com/tatyanazam/FL_covid_vis/blob/master/just_july_dates_age_dist_changes.png)

In July, the highest proportion of cases has been consistently skewed towards younger age groups

``` r
last_15_dates <- (levels(unique(as.factor((fl_covid_data_f$date)))))[121:135] #actually last 15 - all july dates
ggplot(fl_covid_data_f[fl_covid_data_f$County %in% top10_counties & 
                         as.character(fl_covid_data_f$date) %in% last_15_dates,], 
       aes(reorder(as.factor(date), desc(date)), num_case)) + 
  geom_bar(aes(fill= Age_group), stat='identity', position='stack') + 
  scale_fill_brewer(palette="Spectral", direction=-1) + xlab("Date (Year-Month-Day)") + 
  ylab("Daily Number of Cases") +
  facet_wrap(~County, ncol=2) + theme(axis.ticks.x = element_blank(),
                                      axis.ticks.y = element_blank(),
                                     axis.text.y = element_text(size=7, vjust=0.5),
                                      panel.grid.major = element_blank(),
                                      panel.grid.minor = element_blank(),
                                      panel.border = element_blank(),
                                     panel.background = element_blank()) + 
  coord_flip() + labs(fill="Age Group")
```

To recreate these plots or to further explore Florida coronavirus data, feel free to use and expand upon the data and code here.



 
