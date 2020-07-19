#Import necessary libraries and Load data
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

#Visualize Map of the total cases per County, including mask ordinance information
#merge together coronavirus data from FDOH with the mask ordinance data
flcovid_mask_df <- merge(fl_covid_data, fl_county_mask_date_df, by.x = "County", by.y="CountyName", all.x=TRUE)

#count up total cases per county from March to July using group_by() and count() functions
total_case_count_per_fl_df <- flcovid_mask_df %>% group_by(County) %>% count() %>% rename(total_cases = n, county_name = County)
total_case_count_per_fl_w_dates_df <- flcovid_mask_df %>% group_by(County, date) %>% count() %>% rename(total_cases = n, county_name = County)

#to keep spatial information intact and add on total case information , use geo_join() from tigris package
counties2_v2 <- tigris::geo_join(counties2, total_case_count_per_fl_df, "CountName", "county_name")
counties2_w_dates_v2 <- tigris::geo_join(counties2, total_case_count_per_fl_w_dates_df, "CountName", "county_name")
counties2_v2_w_masks <- tigris::geo_join(counties2_v2, fl_county_mask_date_df, "CountName", "county_name")

# create map with counties filled in by # total cases and outlined by whether or not masks are enforced by mandatory order
#use ggplot geom_sf() to create map and use viridis color scale to shade counties by their total case counts. 
#a scalebar was added using the ggsn package
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
         
## Visualize Daily Case Numbers Per County from March to July
#first add a 0 entry for each day for each county with no recorded cases
full_counts_per_date_l <- lapply(unique(fl_covid_data_f$date), function(date_c){
newdf <- fl_covid_data_f[fl_covid_data_f$date == date_c,] %>% group_by(County) %>% count()
newdf$date = date_c
#print(date_c)
# for each day, if any counties are missing from the 67 total counties, add these counties with entries of 0
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
#bind together these empty entries to full dataset
full_case_counts_df <- dplyr::bind_rows(full_counts_per_date_l, .id="column_label")
full_case_counts_df <- full_case_counts_df %>% 
  separate(date, into=c("year", "month", "day"), 
           sep= "-") %>% 
  unite("newdate", c(month, day))
  
# create heatmap like plot of rise in daily case # across counties 
#use geom_tile function and shade each tile by daily case number, with blue indicating low counts and red indicating high counts
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

## Visualize Change in Age Group Distribution in Cases per County from March to July
# first order counts of case # from county with highest # counts to lowest and save as new dataframe
fl_covid_data_f %>% group_by(County) %>% count() %>% arrange(desc(n)) -> top_10_covid_counties_df 
top10_counties <- top_10_covid_counties_df$County[1:10] #take top 10 counties with highest # of cases
fl_covid_data_f$Age_group <- as.factor(fl_covid_data_f$Age_group) #convert age group variable to factor and reorder age ranges
fl_covid_data_f$Age_group <- factor(fl_covid_data_f$Age_group, levels= c("0-4 years", "5-14 years", "15-24 years", "25-34 years", "35-44 years", "45-54 years", "55-64 years", "65-74 years", "75-84 years", "85+ years", "Unknown"))
# create barplot of age distribution change in daily cases from March-July for top 10 counties with highest # cases 
ggplot(fl_covid_data_f[fl_covid_data_f$County %in% top10_counties,], 
       aes(reorder(as.factor(date), desc(date)), num_case)) + 
  geom_bar(aes(fill= Age_group), stat='identity', position='stack') + 
  scale_fill_brewer(palette="Spectral", direction=-1) + xlab("Date (3/10/2020-07/15/2020)") + 
  ylab("Daily Number of Cases") +
  facet_wrap(~County, ncol=2) + theme(axis.text.y = element_blank(), 
                                      axis.ticks.x = element_blank(),
                                      axis.ticks.y = element_blank()) + 
  coord_flip() + labs(fill="Age Group")

## Now Visualize Change in Age Group Distribution per County just for dates in July

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


 