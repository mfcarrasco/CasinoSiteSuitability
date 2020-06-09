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

#Location Criteria/Decision Factors for a Casino#### 
# It can’t be downtown, so it should be within 1 mile from the Gettysburg town boundary (Buffer)
# It has to be at least ¼ mile away from the National Park (Buffer)
# It has to be adjacent to a major thoroughfare (Buffer)
# It has to be a commercial zone (dplyr)
# It has to have an existing population density less than the study area median (dplyr)
# It has to be greater than 40 acres (dplyr).


#Buffer away from downtown 
gettyBuffer = gettyBoundary %>%
  st_buffer(1609)

#Buffer away from National Park
npBuffer = nParks %>%
  st_buffer(402)

#Adjacent to highway
hwyBuffer = st_buffer(highways,50)


ggplot() + 
  theme_classic()+
  geom_sf(data=blocks, fill = 'grey55') + 
  geom_sf(data = blocks[st_buffer(highways, 50),], fill = 'blue') +
  geom_sf(data = gettyBoundary,color = 'red', fill='transparent') +
  geom_sf(data = gettyBuffer, color = 'green', fill = 'transparent', size = 2) +
  geom_sf(data = npBuffer, color = 'darkgreen', fill = 'transparent', size = 2) +
  geom_sf(data = hwyBuffer, color = 'dark blue')

# Existing population density less than median/Greater than 40 acres/Must be commercial

attributeSelect = blocks %>%
  mutate(area = as.integer(st_area(blocks))* 0.000247105) %>%
  filter(area > 40 &
           pden2000 < median(pden2000) &
           zoning == 2)
ggplot() + 
  theme_classic()+
  geom_sf(data=blocks, fill = 'grey55') + 
  geom_sf(data = attributeSelect, fill = 'yellow')


#Selection by Location ####

#Find blocks that are part of the attribute select and inside the gettysburg buffer
#Of those, find those that are not in the national park buffer

dissolve = npBuffer %>%
  group_by(STATE) %>% #There's only one state but this allows us to 'dissolve' wawya the four polygons into one multipolygon.
  summarize(first(STATE))  #there is no sf package 'dissolve'; use a combination of group_by and summarize

blocksInBuffer = attributeSelect[gettyBuffer,] #Find those dplyr-selected properties that fall within the gettysburg mile buffer. 

#use st_disjoin to find blocks that do not intersect the dissolved Gettysburg Buffer since we DON'T want potential areas in National Parks

noPark = st_disjoint(dissolve, blocksInBuffer, sparse = F)
finalSelection = blocksInBuffer[noPark,]


ggplot() + 
  theme_classic()+
  geom_sf(data=blocks, fill = 'grey55') + 
  geom_sf(data = finalSelection, fill = 'gold')
