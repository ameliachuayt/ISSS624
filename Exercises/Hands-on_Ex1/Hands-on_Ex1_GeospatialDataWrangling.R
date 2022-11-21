#Hands-on Exercise 1 Part 1

setwd("C:/ameliachuayt/ISSS624/Hands-on_Ex/Hands-on_Ex1")

packages = c('sf','tidyverse')
for (p in packages){
  if(!require(p, character.only = T)){
    install.packages(p)
  }
  library(p, character.only = T)
}

# or pacman::p_load(sf, tidyverse)
#1.4.1. Import polygon feature date in shapefile shp format ####

mpsz = st_read(dsn="data\\geospatial",
               layer = "MP14_SUBZONE_WEB_PL")
#st_read has two arguments
#dsn: define data path
#layer: provide shapefile name
#output shows that there are 323 multipolygon features and 15 fields 
#bounding box provides the x extend and y extend of the data

#1.4.2 Import polyline feature data in shapefile shp form ####
cyclingpath = st_read(dsn = "data\\geospatial",
                      layer = 'CyclingPath')
#Output shows that there are  1625 features and 2 fields in cyclingpath linestring
# feature data frame and it is in svy21 projected coordinates system too.

#1.4.3 Import GIS data in kml format ####
preschool = st_read("data\\geospatial\\pre-schools-location-kml.kml")

#The message above reveals that preschool is a point feature data frame. (See 'Geometry type: POINT')
#Different from the previous two simple feature data frame, preschool is in wgs84 coordinates system.

#1.5.1 Working with st_geometry() ####
st_geometry(mpsz)

#1.5.2 Working with glimpse() ####

glimpse(mpsz)
#learn more about the associated attribute information in the data frame
#reveals data type of each field 
#e.g. FMEL-UPD_D is in date data '<date>'
#e.g. X_ADDR, Y_ADDR are in double precision values '<dbl>'

#1.5.3 Working with head() ####
head(mpsz, n=5)

#1.6 Plotting the Geospatial Data ####
plot(mpsz) #default plot of sf object is a multi-plot of all attributes 

#plot only geometry by using this:
plot(st_geometry(mpsz))

#Can also choose to plot the sf object by using a specific attribute
plot(mpsz["PLN_AREA_N"])

# plot() is mean for plotting the geospatial object for quick look
# for high cartographic quality plot, tmap or other packages shld be used.

#1.7 Working with Projection ####
# Ensure that both geospatial data are projected using similar coordinate system.
# Learn Projection Transformation: How to project a simple feature data frame 
                            # from one coordinate system to another coordinate system.

#1.7.1 Assigning EPSG code to a simple feature data frame ####
#Check coordiante system
st_crs(mpsz)
#Altho mpsz df is projected in 'svy21', when we read till the end of the print
#it indicates that EPSG is 9001, which is wrong.(#Note that last line is ID["EPSG",9001])
#The correct EPSG code for 'svy21' shoudl be '3414'

#Assign correct EPSG code
mpsz3414 <- st_set_crs(mpsz, 3414)
st_crs(mpsz3414)
#Note that last line is now ID["EPSG",3414]


#1.7.2 Transforming projection of preschool from wgs84 to svy21
#Transform geographic coordinate system >>> projected coordinate system
#Geographic coordinate system is inappropriate if analysis require use of distance/area measurements

st_geometry(preschool)
#From output, we see that it is in wgs84 coordinate system.

#Projection Transformation must be done using st_transform() instead of st_set_crs()
#as we need to reproject from one coord. system to another MATHEMATICALL

preschool3414 <- st_transform(preschool,
                              crs = 3414)
#Note: In practice, we need find out the appropriate project coordinate system 
#to use before performing the projection transformation.

#Check
st_geometry(preschool3414)
#It is now in svy21 projected coordinate system now. 
# Bounding box:, the values are greater than 0-360 range of decimal degree commonly used by most of the geographic coordinate systems.

#1.8 Importing and Covnerting An Aspatial Data ####
#Aspatial Data: X & Y coords are among other data fields

#1.8.1 Importing aspatial data ####
listings <- read_csv('data\\aspatial\\listings.csv')
list(listings) #note list() instead of glimpse() is used

#Note LatLong are in decimal degree format
#As a best guess, we assume data is in wgs84 Geographic Coordinate System

#1.8.2 Create simple feature dataframe from aspatial dataframe ####

#Convert listing df into simple feature df
listings_sf <- st_as_sf(listings,
                        coords = c("longitude","latitude"), #x-coord first, then y-coord
                        crs=4326) %>% #provide coordinates system in epsg format
                                      #EPSG:4326 is wgs84
                                      #EPSG:3414 is Singapore's SVY21 Projected Coordinate System
  st_transform(crs = 3414)

glimpse(listings_sf)
#Note: new col: geometry added to df, latlong cols dropped from df

#1.9 Geo-processing with sf package ####
#1.9.1 Buffering ####
# Buffering involves measuring the distance outward in all directions from an object. 
# Returns polygon

#To upgrade the cycling path, govt need to acquire 5 metres of reserved land on the both sides of the current cycling path. 
#Determine the extent of the land needed to be acquired and their total area.

#Step 1: Compute the 5-meter buffers around cycling paths
buffer_cycling <- st_buffer(cyclingpath,
                            dist = 5, #5 metres
                            nQuadSegs = 30)

#Step 2: Calculate area of buffers
#Create new col AREA by calculating area of polygons
buffer_cycling$AREA <- st_area(buffer_cycling)

#Step 3: Derive Total Land
sum(buffer_cycling$AREA)

#1.9.2 Point-in-polygon count ####
#Find out the numbers of pre-schools in each Planning Subzone and density

#Step 1: Identify pre-schools located in each Subzone using st_interesecst() and
#Calculate no. of pre-schools that fall inside each Subzone using lengths()

mpsz3414$`PreSch Count` <- lengths(st_intersects(mpsz3414, preschool3414))
summary(mpsz3414$`PreSch Count`)

#List subzone with most pre schools
top_n(mpsz3414, 1, `PreSch Count`)


#Step 2: Derive areaof each subzone
mpsz3414$Area <- mpsz3414 %>% #New col created
  st_area()

#Step 3: Calculate Density
mpsz3414 <- mpsz3414 %>%
  mutate(`PreSch Density` = `PreSch Count`/Area * 1000000)

#1.10 Exploratory Data Analysis (EDA) ####
#1.10.1 histogram 
hist(mpsz3414$`PreSch Density`) #not so good

#use ggplot2 functions instead
ggplot(data=mpsz3414,
       aes(x=as.numeric(`PreSch Density`))) +
  geom_histogram(bins=20,
                 color="black",
                 fill = "light blue") + 
  labs(title = "Are pre-schools evenly distributed in Singapore?",
       subtitle = "There are many planning sub-zones with a single pre-school, on the other hand, \nthere are two planning sub-zones with at least 20 pre-schools",
       x = "Pre-school density (per km sq)",
       y = "Frequency")

#1.10.2 Scatterplot

library(units) # Have to run this if not the code don't work
ggplot(data=mpsz3414, aes(x=`PreSch Density`, y=`PreSch Count`))+
  geom_point()+ 
  labs(x = "Pre-school density (per km sq)",
       y = "Pre-school count")

#Non-ggplot2 methods
plot(mpsz3414$`PreSch Density`, mpsz3414$`PreSch Count`, main="Scatterplot",
    pch=19)
