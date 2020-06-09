#Followng this tutorial: http://urbanspatialanalysis.com/introduction-to-spatial-site-suitability-analysis-in-r/

#Set up routine####

#Check working directory
getwd()

#Ensure strings come in as character types
options(stringsAsFactors = F) 

#Load packages
library(tidyverse)
library(sf)

#Read in the four shapefiles for the analysis
blocks = st_read('data/Gettysburg_blocks.shp')
gettyBoundary = st_read('data/Gettysburg_boundary.shp')
highways = st_read('data/gettysburgHighways.shp')
nParks = st_read('data/gettysburgNationalParks.shp')


#Check files
class(blocks) #As both a SimpleFeatures (sf like shapefile) it can be mapped but as a dataframe can also be analyzed using dplry
head(blocks) 

# STFID – Unique ID from the U.S. Census
# AREA_HA – Area in hectacres
# pop2000 – Population in 2000
# hu2000 – Number of housing units in 2000
# pden2000 – Population density in 2000
# hden2000 – Housing unit density in 2000
# zoning – A categorical zoning classification

#Plotting#### 

#Basic 
plot(blocks)
plot(blocks[,4])

#Ggplot
ggplot() + 
  geom_sf(data = blocks,
          aes(fill = pop2000))
blocks %>% 
  mutate(zoning = ifelse(zoning == 1, 'Residential', 
                         ifelse(zoning ==2, 'Commercial', 'Industrial'))) %>%
  select(pden2000, hden2000, zoning) %>%
  as.data.frame() %>%
  gather(variable, value, pden2000:hden2000) %>%
  st_sf() %>%
  ggplot() + 
  geom_sf(aes(fill = value, color = value)) + 
  facet_wrap(~variable)

#Basic geoprocessing#### 
#Area
blocks$area = blocks %>%
  st_area()  #Calculate the area in units that correspond to the coordinate system of the input sf object (hence recorde into integer)

ggplot() + 
  geom_sf(data = blocks, aes(fill = as.integer(area)))

#Buffer
#Buffer the first block and buffer it by 100 meters. Plot with original overlaying. 

blocks[1,] %>%
  st_buffer(100) %>%
  ggplot() + 
  geom_sf(fill = 'dodgerblue4') +
  geom_sf(data = blocks[1,], fill = 'black')

#Select by overlay (clip/Select by location)
dummyBuffer = 
  blocks[275,] %>% #Centrally located feature
  st_centroid() %>% #Find centroid of that feature
  st_buffer(1700) #Create a 1700  buffer to select blocks that intersect it.

ggplot() + 
  geom_sf(data = blocks, fill = 'black') + 
  geom_sf(data = dummyBuffer, fill = 'dodgerblue4')

blocks[dummyBuffer,] %>% #grab all blocks that intersect with the buffer
  ggplot() +
  geom_sf(data = dummyBuffer, fill = 'black') +
  geom_sf(fill = 'dodgerblue4')
  
dummyBuffer = 
  blocks[275,] %>% #Centrally located feature
  st_centroid() %>% #Find centroid of that feature
  st_buffer(1700) %>% #Create a 1700  buffer to select blocks that intersect it.
  mutate(geography = 'dummyBuffer') %>%
  select(geography)


blocksGeography = blocks %>%
  mutate(geography = 'blocks') %>%
  select(geography)

blocksWithDummyBuffer = 
  rbind(blocksGeography, dummyBuffer)

ggplot() +
  geom_sf(data = blocksWithDummyBuffer, aes(fill=geography)) + 
  scale_fill_manual(values = c('dark blue', 'purple'),
                    labels = c('Blocks', 'Buffer'),
                    name = '') + 
  labs(title= 'Blocks and the Buffer', 
       subtitle = 'Gettysburg, PA')
