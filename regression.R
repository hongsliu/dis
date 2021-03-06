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

#read in the merged data
LondonmsoaProfiles <- read_csv(here::here("data", "msoa_travel_profile_merged.csv"), 
                              col_names = TRUE) %>%
  clean_names()

Datatypelist <- LondonmsoaProfiles %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist

#join the data to MSOA
LonmsoaProfiles <- Londonmsoas%>%
  left_join(.,
            LondonmsoaProfiles, 
            by = c("MSOA11CD" = "msoa11cd"))

#log transformation for variables
LonmsoaProfiles <- LonmsoaProfiles %>%
  mutate(log_pub_tra_stops_den = log10(pub_tra_stops_den+1))%>%
  mutate(log_land_area_hectares = log10(land_area_hectares+1))%>%
  mutate(log_median_house_price_2012 = log10(median_house_price_2012+1))%>%
  mutate(level_4_per_2011 = level_4_per_2011*100)

# subset the variables for regression
Regressiondata<- LonmsoaProfiles%>%
  dplyr::select(pub_per,
                log_pub_tra_stops_den,
                pop_den_2011,
                log_median_house_price_2012,
                level_4_per_2011,
                log_land_area_hectares,
                one_car_or_more_per_2011,
                deprived_household_per_2010)

#run linear regression models and compare their results
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
       pop_den_2011+
       log_median_house_price_2012+
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

# examine the residuals of model2
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

#Before GWR, we have to calculate the centroids of all Wards in London
coordsM <- LonmsoaProfiles%>%
  st_centroid()%>%
  st_geometry()

plot(coordsM)

#Now we need to generate a spatial weights matrix 

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

#create a spatial weights matrix object from the weights

Lmsoa.knn_4_weight <- Lmsoa_knn %>%
  nb2listw(., style="C")

Nearest_neighbour <- LonmsoaProfiles %>%
  st_drop_geometry()%>%
  dplyr::select(model2resids)%>%
  pull()%>%
  moran.test(., Lmsoa.knn_4_weight)%>%
  tidy()

Nearest_neighbour

# add the Inner/Outer London as a dummy variable
newdata <- read_csv(here::here("data", "msoa_inner_outer.csv"), 
                               col_names = TRUE) %>%
  clean_names()

LonmsoaProfiles <- LonmsoaProfiles%>%
  left_join(.,
            newdata, 
            by = c("MSOA11CD" = "msoa11cd"))

p <- ggplot(LonmsoaProfiles, 
            aes(x=one_car_or_more_per_2011, 
                y=pub_per))
p + geom_point(aes(colour = inner_outer))

isitfactor <- LonmsoaProfiles %>%
  dplyr::select(inner_outer)%>%
  summarise_all(class)

isitfactor

LonmsoaProfiles<- LonmsoaProfiles %>%
  mutate(inner_outer=as.factor(inner_outer))

#now run the model
model6 <- lm(pub_per ~
               pop_den_2011+
               log_median_house_price_2012+
               level_4_per_2011+
               one_car_or_more_per_2011+
               inner_outer,
             data = LonmsoaProfiles)

tidy(model6)
summary(model6)
glance(model6)

#select some variables from the data file
myvars <- LonmsoaProfiles %>%
  dplyr::select(pub_per,
                pop_den_2011,
                log_median_house_price_2012,
                level_4_per_2011,
                one_car_or_more_per_2011,
                inner_outer)

#check their correlations are OK
library(corrr)
Correlation_myvars <- myvars %>%
  st_drop_geometry()%>%
  dplyr::select(-inner_outer)%>%
  correlate()

#run a final OLS model
model_final <- lm(pub_per ~
                    pop_den_2011+
                    log_median_house_price_2012+
                    level_4_per_2011+
                    one_car_or_more_per_2011+
                    inner_outer, 
                  data = myvars)

tidy(model_final)
tidy(model2)

LonmsoaProfiles <- LonmsoaProfiles %>%
  mutate(model_final_res = residuals(model_final))

par(mfrow=c(2,2))
plot(model_final)

par(mfrow=c(1,1))

qtm(LonmsoaProfiles, fill = "model_final_res")

final_model_Moran <- LonmsoaProfiles %>%
  st_drop_geometry()%>%
  dplyr::select(model_final_res)%>%
  pull()%>%
  moran.test(., Lmsoa.knn_4_weight)%>%
  tidy()

#run the GWR

library(spgwr)

st_crs(LonmsoaProfiles) = 27700

LonmsoaProfilesSP <- LonmsoaProfiles %>%
  as(., "Spatial")

st_crs(coordsM) = 27700
coordsMSP <- coordsM %>%
  as(., "Spatial")

coordsMSP

GWRbandwidth <- gwr.sel(pub_per ~
                          pop_den_2011+
                          log_median_house_price_2012+
                          level_4_per_2011+
                          one_car_or_more_per_2011+
                          inner_outer, 
                        data = LonmsoaProfilesSP, 
                        coords=coordsMSP,
                        adapt=T)

gwr.model = gwr(pub_per ~
                  pop_den_2011+
                  log_median_house_price_2012+
                  level_4_per_2011+
                  one_car_or_more_per_2011+
                  inner_outer, 
                data = LonmsoaProfilesSP, 
                coords=coordsMSP, 
                adapt=GWRbandwidth, 
                hatmatrix=TRUE, 
                se.fit=TRUE)

#print the results of the model
gwr.model

results <- as.data.frame(gwr.model$SDF)
names(results)

#attach coefficients to original SF
LonmsoaProfiles2 <- LonmsoaProfiles %>%
  mutate(coefPopden = results$pop_den_2011,
         coefHouseprice = results$log_median_house_price_2012,
         coefLev4Qual = results$level_4_per_2011,
         coefCar = results$one_car_or_more_per_2011,
         coefintercept = results$X.Intercept.,
         coefouter = results$inner_outerOuter,
         local_R2 = results$localR2)

#plot the coefficients of variables (e.g. car ownership) on the map of London 
tmap_mode("plot")
tm <- tm_shape(LonmsoaProfiles2) +
  tm_polygons(col = "coefCar", 
              palette = "RdBu", 
              alpha = 0.5,
              title='Coefficients')+
  tm_scale_bar(width = 0.15, color.dark = 'gray60',
               position = c('left', 'bottom')) +
  tm_compass(color.dark = 'gray60', text.color ='gray60',
             position = c('right','top'))

tmap_save(tm, filename = "GWR_car.png")

