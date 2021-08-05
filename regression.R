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
  mutate(log_median_house_price_2012 = log10(median_house_price_2012+1))%>%
  mutate(level_4_per_2011 = level_4_per_2011*100)

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

q <- qplot(x = `level_4_per_2011`, 
            y = `pub_per`, 
            data=Regressiondata)

q + stat_smooth(method="lm", se=FALSE, size=1)

#now model
model1 <- Regressiondata %>%
  lm(pub_per ~
       log_pub_tra_stops_den+
       pop_den_2011+
       log_median_house_price_2012+
       level_4_per_2011+
       log_land_area_hectares+
       one_car_or_more_per_2011+
       deprived_household_per_2010,
     data=.)
summary(model1)
tidy(model1)
glance(model1)

model2 <- Regressiondata %>%
  lm(pub_per ~
       pop_den_2011+
       log_median_house_price_2012+
       level_4_per_2011+
       one_car_or_more_per_2011,
     data=.)
summary(model2)
tidy(model2)
glance(model2)

model3 <- Regressiondata %>%
  lm(pub_per ~
       level_4_per_2011+
       one_car_or_more_per_2011,
     data=.)
summary(model3)
tidy(model3)
glance(model3)

model4 <- Regressiondata %>%
  lm(pub_per ~
       log_median_house_price_2012+
       level_4_per_2011+
       one_car_or_more_per_2011,
     data=.)
summary(model4)
tidy(model4)
glance(model4)

model5 <- Regressiondata %>%
  lm(pub_per ~
       pop_den_2011+
       log_median_house_price_2012+
       level_4_per_2011+
       log_pub_tra_stops_den,
     data=.)
summary(model5)
tidy(model5)
glance(model5)

library(tidypredict)
Regressiondata %>%
  tidypredict_to_column(model2)

model_data <- model2 %>%
  augment

model_data%>%
  dplyr::select(.resid)%>%
  pull()%>%
  qplot()+ 
  geom_histogram() 

#and for future use, write the residuals out
model_data2 <- model2 %>%
  augment(., Regressiondata)

# also add them to the shapelayer
LonmsoaProfiles <- LonmsoaProfiles %>%
  mutate(model2resids = residuals(model2))

vif(model2)

#print some model diagnositcs. 
par(mfrow=c(2,2))    #plot to 2 by 2 array
plot(model2)

par(mfrow=c(1,1)) 

DW <- durbinWatsonTest(model2)
tidy(DW)

#now plot the residuals
tmap_mode("view")
#qtm(LonmsoaProfiles, fill = "model2_resids")

tm_shape(LonmsoaProfiles) +
  tm_polygons("model2resids",
              palette = "RdYlBu")

#calculate the centroids of all Wards in London
coordsM <- LonmsoaProfiles%>%
  st_centroid()%>%
  st_geometry()

plot(coordsM)

#Now we need to generate a spatial weights matrix (remember from the lecture a couple of weeks ago). We'll start with a simple binary matrix of queen's case neighbours

Lmsoa_nb <- LonmsoaProfiles %>%
  poly2nb(., queen=T)

#or nearest neighbours
knn_msoa <-coordsM %>%
  knearneigh(., k=4)

Lmsoa_knn <- knn_msoa %>%
  knn2nb()

#plot them
plot(Lmsoa_nb, st_geometry(coordsM), col="red")
plot(Lmsoa_knn, st_geometry(coordsM), col="blue")

#create a spatial weights matrix object from these weights

Lmsoa.knn_4_weight <- Lmsoa_knn %>%
  nb2listw(., style="C")

Nearest_neighbour <- LonmsoaProfiles %>%
  st_drop_geometry()%>%
  dplyr::select(model2resids)%>%
  pull()%>%
  moran.test(., Lmsoa.knn_4_weight)%>%
  tidy()

Nearest_neighbour

