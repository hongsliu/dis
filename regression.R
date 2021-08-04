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
    fill = "Average GCSE capped point scores - 2014", 
    borders = NULL,  
    fill.palette = "Blues")