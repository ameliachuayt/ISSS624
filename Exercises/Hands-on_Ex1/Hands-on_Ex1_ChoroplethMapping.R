setwd("C:\\ameliachuayt\\ISSS624\\Hands-on_Ex\\Hands-on_Ex1")
getwd()

#2.3.1 install and load packages ####
pacman::p_load(sf, tmap, tidyverse)

#More on tmap
#https://cran.r-project.org/web/packages/tmap/vignettes/tmap-getstarted.html

#2.3.2 Import Geospatial Data ####

mpsz <- st_read(dsn="data\\geospatial",
                layer='MP14_SUBZONE_WEB_PL')


#2.3.3 Import Attribute Data
popdata <- read_csv('data\\aspatial\\respopagesextod2011to2020.csv')

#2.3.4 Data Preparation
popdata2020 <- popdata %>%
  filter(Time==2020) %>%
  group_by(PA, SZ, AG) %>%
  summarise(POP = sum(Pop)) %>%
  ungroup() %>%
  pivot_wider(names_from=AG, 
              values_from=POP) %>%
  mutate(YOUNG = rowSums(.[3:6])
         +rowSums(.[12])) %>%
  mutate(`ECONOMY ACTIVE` = rowSums(.[7:11])+
           rowSums(.[13:15])) %>%
  mutate(AGED = rowSums(.[16:21])) %>%
  mutate(TOTAL = rowSums(.[3:21])) %>%
  mutate(DEPENDENCY = (YOUNG + AGED) / `ECONOMY ACTIVE`) %>%
  select(PA, SZ, YOUNG, `ECONOMY ACTIVE`, AGED, TOTAL, DEPENDENCY)
  
#2.3.4.2 Joining attribute data and geospatial data

popdata2020 <- popdata2020 %>%
  mutate_at(.vars = vars(PA,SZ),
            .funs = funs(toupper)) %>%
  filter(`ECONOMY ACTIVE` > 0)
  
mpsz_pop2020 <- left_join(mpsz, popdata2020, 
                          by= c("SUBZONE_N" = "SZ"))

write_rds(mpsz_pop2020, "data\\rds\\mpszpop2020_amelia.rds")

#2.4 Choropleth Mapping Geospatial Data Using tmap ####
#2.4.1 Plotting choropleth quickly using qtm() ####
tmap_mode('plot')
qtm(mpsz_pop2020,
    fill = "DEPENDENCY")

#2.4.2 Creating choropleth map using tmap's elements ####
tm_shape(mpsz_pop2020) +
  tm_fill("DEPENDENCY",
          style = "quantile",
          palette = "Blues",
          title = "Dependency Ratio") +
  tm_layout(main.title = "Distribution of Dependency Ratio by planning subzone",
            main.title.position = 'center',
            main.title.size = 1.2, 
            legend.height = 0.45,
            legend.width = 0.35,
            frame = TRUE) +
  tm_borders(alpha = 0.5) + 
  tm_compass(type="8star", size = 2) + 
  tm_scale_bar() + 
  tm_grid(alpha = 0.2) + 
  tm_credits("Source: Planning Sub-zone boundary from Urban Redevelopment Authorithy (URA)\n and Population data from Department of Statistics DOS", 
             position = c("left", "bottom"))

#2.4.2.1-3 Drawing a base map ####
tm_shape(mpsz_pop2020) +
  tm_polygons()

#2.4.2.2 Drawing a choropleth map using tm_polygons() ####
tm_shape(mpsz_pop2020) +
  tm_polygons("DEPENDENCY")

#2.4.2.3 Drawing a choropleth map using tm_fill() and *tm_border()**
tm_shape(mpsz_pop2020) + 
  tm_fill("DEPENDENCY") + 
  tm_borders(lwd = 0.1, alpha = 1)
    #col = border colour,
    #lwd = border line width. The default is 1, and
    #lty = border line type. The default is “solid”.

#2.4.3 Data classification methods of tmap
#2.4.3.1 Plotting choropleth maps with built-in classification methods ####
tm_shape(mpsz_pop2020) + 
  tm_fill("DEPENDENCY", 
          n = 5, 
          style = 'jenks') + 
  tm_borders(alpha = 0.5)

#2.4.3.2 Plotting choropleth map with custom breakS ####

#some descriptive stats first
summary(mpsz_pop2020$DEPENDENCY)

#tmap: need to include min and max breaks
#also, we want our custom breaks to be at 0.60, 0.70, 0.80, 0.90

tm_shape(mpsz_pop2020) + 
  tm_fill("DEPENDENCY",
          breaks = c(0, 0.60, 0.70, 0.80, 0.90, 1.00)) +
  tm_borders(alpha = 0.5)


#2.4.4 Colour Scheme ####
#tmap supports colour ramps either defined by the user or 
# a set of predefined colour ramps from the RColorBrewer package.

#2.4.4.1 Using ColourBrewer Palette ####
tm_shape(mpsz_pop2020) + 
  tm_fill("DEPENDENCY",
          n = 6, 
          style = "quantile",
          palette = "Blues") + #Also available: Greens
  tm_borders(alpha = 0.5)

#Add a '-' prefix to reverse the colour
tm_shape(mpsz_pop2020) + 
  tm_fill("DEPENDENCY",
          n = 6, 
          style = "quantile",
          palette = "-Blues") + #add here!
  tm_borders(alpha = 0.5)


#2.4.5 Map Layouts ####
#Combination of all map elements into a cohesive map
#Includes: objects to be mapped,title, scale bar,  compass, margins and aspects ratios

#2.4.5.1 Map Legend ####
tm_shape(mpsz_pop2020)+
  tm_fill("DEPENDENCY", 
          style = "jenks", 
          palette = "Blues", 
          legend.hist = TRUE, 
          legend.is.portrait = TRUE,
          legend.hist.z = 0.1) +
  tm_layout(main.title = "Distribution of Dependency Ratio by planning subzone \n(Jenks classification)",
            main.title.position = "center",
            main.title.size = 1,
            legend.height = 0.45, 
            legend.width = 0.35,
            legend.outside = FALSE,
            legend.position = c("right", "bottom"),
            frame = FALSE) +
  tm_borders(alpha = 0.5)


#2.4.5.2 Map style ####
#Change layout settings using tmap_style()

tm_shape(mpsz_pop2020) + 
  tm_fill("DEPENDENCY",
          style = "quantile", 
          palette = "-Greens") + 
  tm_borders(alpha = 0.5) +
  tmap_style("classic")
#other avail styles: "white", "gray", "natural", "cobalt", "col_blind", "albatross", "beaver", "bw", "watercolor" 

#2.4.5.3 Cartographic Furniture ####
# Draw other map furniture such as compass, scale bar and grid lines.
# using tm_compass(), tm_scale_bar() and tm_grid() 

tm_shape(mpsz_pop2020) + 
  tm_fill("DEPENDENCY",
          style = "quantile", 
          palette = "Blues",
          title = "No. of persons") + 
  tm_layout(main.title = "Distribution of Dependency Ratio \nby planning subzone",
            main.title.position = "center",
            main.title.size = 1.2, 
            legend.height = 0.45,
            legend.width = 0.35,
            frame = TRUE) +
  tm_borders(alpha = 0.5) + 
  tm_compass(type="8star", size = 2) + 
  tm_scale_bar(width = 0.15) + 
  tm_grid(lwd = 0.1, alpha = 0.2) + 
  tm_credits("Source: Planning Sub-zone boundary from Urban Redevelopment Authorithy (URA)\n and Population data from Department of Statistics DOS", 
             position = c("left", "bottom"))

#Reset the default style
tmap_style("white")



#2.4.6 Drawing Small Multiple Choropleth Maps / Facet Maps####
#Facet maps are composed of many maps arranged side-by-side
#Enable the visualisation of how spatial relationships change with respect to another variable, such as time.

#In tmap, small multiple maps can be plotted in three ways:
  # by assigning multiple values to at least one of the aesthetic arguments,
  # by defining a group-by variable in tm_facets(), and
  # by creating multiple stand-alone maps with tmap_arrange().

#2.4.6.1 By assigning multiple values to at least one of the aesthetic arguments ####
#small choropleths are created by defining ncols in tm_fill()

tm_shape(mpsz_pop2020) + 
  tm_fill(c("YOUNG","AGED"),
          style = "equal", #"equal" style divides the range of values into n equal-sized intervals
          palette = "Blues") + 
  tm_layout(legend.position = c("right", "bottom")) +
  tm_borders(alpha = 0.5) + 
  tmap_style("white")


tm_shape(mpsz_pop2020)+ 
  tm_polygons(c("DEPENDENCY","AGED"),
              style = c("equal", "quantile"), 
              palette = list("Blues","Greens")) +
  tm_layout(legend.position = c("right", "bottom"))


#2.4.6.2 By defining a group-by variable in tm_facets() ####
#multiple small choropleth maps are created by using tm_facets()

tm_shape(mpsz_pop2020) + 
  tm_fill("DEPENDENCY",
          style = "quantile",
          palette = "Blues",
          thres.poly = 0) + 
  tm_facets(by = "REGION_N",
            free.coords = TRUE,
            drop.units  = TRUE) + #instead of drop.shapes as it is deprecated
  tm_layout(legend.show = FALSE,
            title.position = c("center", "center"),
            title.size = 20) + 
  tm_borders(alpha = 0.5)

#2.4.6.3 By creating multiple stand-alone maps with tmap_arrange() ####
#multiple small choropleth maps are created by creating multiple stand-alone maps with tmap_arrange().

#Create standalone maps
youngmap <- tm_shape(mpsz_pop2020)+ 
  tm_polygons("YOUNG", 
              style = "quantile", 
              palette = "Blues")

agedmap <- tm_shape(mpsz_pop2020)+ 
  tm_polygons("AGED", 
              style = "quantile", 
              palette = "Blues")

#Join
tmap_arrange(youngmap, agedmap, asp=1, ncol=3) #ncol = no. of cols #asp =  aspect ratio of each map.

#if got more maps, just add on. for e.g.
tmap_arrange(youngmap, agedmap, youngmap, agedmap, asp=1, ncol=2)


#2.4.7 Mappping Spatial Object Meeting a Selection Criterion ####
#Use selection function to map spatial objects meeting the selection criterion.

tm_shape(mpsz_pop2020[mpsz_pop2020$REGION_N=="CENTRAL REGION", ])+
  tm_fill("DEPENDENCY", 
          style = "quantile", 
          palette = "Blues", 
          legend.hist = TRUE, 
          legend.is.portrait = TRUE,
          legend.hist.z = 0.1) +
  tm_layout(legend.outside = TRUE,
            legend.height = 0.45, 
            legend.width = 5.0,
            legend.position = c("right", "bottom"),
            frame = FALSE) +
  tm_borders(alpha = 0.5)





