#library a bunch of packages we may (or may not) use - install them first if not installed already. 
library(tidyverse)
library(tmap)
library(geojsonio)
library(plotly)
library(rgdal)
library(broom)
library(mapview)
library(crosstalk)
library(sf)
library(sp)
library(spdep)
library(car)
library(fs)
library(janitor)

#download.file("https://data.london.gov.uk/download/statistical-gis-boundary-files-london/9ba8c833-6370-4b11-abdc-314aa020d5e0/statistical-gis-boundaries-london.zip", 
#              destfile="statistical-gis-boundaries-london.zip")

Londonmsoas<-dir_info(here::here("statistical-gis-boundaries-london", 
                                 "ESRI"))%>%
  #$ means exact match
  dplyr::filter(str_detect(path, 
                           "MSOA_2011_London_gen_MHW.shp$"))%>%
  dplyr::select(path)%>%
  pull()%>%
  #read in the file in
  st_read()

qtm(Londonmsoas)

LondonmsoaProfiles <- read_csv(here::here("data", "msoa_travel_profile_merged.csv"), 
                              col_names = TRUE) %>%
  clean_names()

Datatypelist <- LondonmsoaProfiles %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist

LonmsoaProfiles <- Londonmsoas%>%
  left_join(.,
            LondonmsoaProfiles, 
            by = c("MSOA11CD" = "msoa11cd"))

tmap_mode("view")
qtm(LonmsoaProfiles, 
    fill = "pop_den_2011", 
    borders = NULL,  
    fill.palette = "Blues")

LonmsoaProfiles <- LonmsoaProfiles %>%
  mutate(log_pub_tra_stops_den = log10(pub_tra_stops_den+1))%>%
  mutate(log_land_area_hectares = log10(land_area_hectares+1))%>%
  mutate(log_median_house_price_2012 = log10(median_house_price_2012+1))

#run the linear regression model and store its outputs in an object called model1
Regressiondata<- LonmsoaProfiles%>%
  dplyr::select(pub_per,
                log_pub_tra_stops_den,
                pop_den_2011,
                log_median_house_price_2012,
                level_4_per_2011,
                log_land_area_hectares,
                one_car_or_more_per_2011,
                deprived_household_per_2010)

#now model
model1 <- Regressiondata %>%
  lm(pub_per ~
       one_car_or_more_per_2011,
     data=.)
summary(model1)
tidy(model1)
glance(model1)



