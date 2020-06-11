
# Crime data may be cut down to recent 3 years of data if the plotting takes too much time.


# install.packages('leaflet')
# install.packages('dplyr')
# install.packages('sp')
# install.packages('RDSTK')
# install.packages('sf')
# install.packages('mapview')
# install.packages('leaflet.extras')

# include libraries
library(dplyr)
library(leaflet)
library(sp)
library(sf)
library(RDSTK)
library(leaflet.extras)


# import crime data by category
felony <- read.csv(file = "felony.csv", header = TRUE, sep = ",")
misdemeanor <- read.csv(file = "misdemeanor.csv", header = TRUE, sep = ",")
violation <- read.csv(file = "violation.csv", header = TRUE, sep = ",")

# import shooting data
nyc_shooting <- read.csv(file="nyc_shooting.csv", header=TRUE, sep=",")
nyc_shooting

# get user input for the desired address
addr <- readline(prompt = "Enter address (e.g. 2990 Broadway, New York, New York): ")

# generate home icon for to mark the user input
home_icon<- awesomeIcons(
  icon = 'home',
  library = 'ion',
  markerColor = 'green'
)

# get longitude and latitude for the user input
addr_lon <- street2coordinates(addr)$longitude
addr_lat <- street2coordinates(addr)$latitude

# create nyc_crime_map
nyc_crime_map <- leaflet() %>%
  addTiles() %>%
  #addPolygons(data=shapeData,weight=1,col = 'red', fill = NA) %>% 
  setView(-73.9, 40.73, zoom = 10) %>%
  addProviderTiles("CartoDB.Positron") 

# mark user input on the map
if (is.numeric(addr_lon) & is.numeric(addr_lat)){
  nyc_crime_map <- nyc_crime_map %>%
    addAwesomeMarkers(lng = addr_lon, lat = addr_lat, icon = home_icon, group = 'Home')
}
# Add heat map of crimes to the map
nyc_crime_map <- nyc_crime_map %>%
  addCircleMarkers(data = nyc_shooting,  radius = 0.5, color = 'black', group = 'Shootings') %>%
  addHeatmap(data = felony,  blur = 20, max = 0.05, radius = 10, group = 'Felonies') %>%
  addHeatmap(data = misdemeanor,  blur = 20, max = 0.05, radius = 10, group = 'Misdemeanors') %>%
  addHeatmap(data = violation,  blur = 20, max = 0.05, radius = 10, group = 'Violations')

# import shapefile for city council districts
zipcode <- st_read('ZIP_CODE/ZIP_CODE_040114.shp')
shapeData <- st_transform(zipcode, CRS('+proj=longlat +datum=WGS84'))
addr_zip <- NA



# get zipcode from address given by the user
if (is.numeric(addr_lon) & is.numeric(addr_lat)){
  addr_st <- st_point(c(addr_lon, addr_lat))
  idx <- as.integer(st_intersects(addr_st, shapeData$geometry))
  addr_zip <- shapeData$ZIPCODE[idx]
  addr_zip_sf <- shapeData$geometry[idx]
}


# Mark selected zip with bold line
if (!is.na(addr_zip)) {
  nyc_crime_map <- nyc_crime_map %>%
  addPolygons(data=addr_zip_sf,weight=3,col = 'red', fillOpacity = 0,
              group = "Home")
}



# Mark area by zipcode and add layers control
nyc_crime_map <- nyc_crime_map %>%
  addPolygons(data=shapeData$geometry,weight=1,col = 'grey', fillOpacity = 0,
              highlightOptions = highlightOptions(
                weight = 2,
                color = 'black', fillOpacity = 0,
                bringToFront = TRUE, sendToBack = TRUE),
              popup = shapeData$ZIPCODE,
              group = "Area by zipcode"
  )%>%
  addLayersControl(overlayGroups = c("Violations", "Misdemeanors", "Felonies", "Shootings", "Area by zipcode", "Home"))

nyc_crime_map


# zip_df <- zip_df <- data.frame(shapeData$ZIPCODE)
# names(zip_df) <- "ZIPCODE"
zip_df <- read.csv("zip_df.csv")


# count number of crimes by zipcode

# felony_sf <- felony %>%
#   mutate_at(c("Longitude","Latitude"), as.numeric) %>%   # coordinates must be numeric
#   st_as_sf(
#     coords = c("Longitude","Latitude"),
#     agr = "constant",
#     crs = CRS('+proj=longlat +datum=WGS84'),        # nad83 / new york long island projection
#     stringsAsFactors = FALSE,
#     remove = TRUE
#   )
# 
# felony_in_zip <- st_join(felony_sf,shapeData, join = st_within)
# felony_zip_count <- count(as_tibble(felony_in_zip), "ZIPCODE")
# 
# felony_zip_count <- join(zip_df, data.frame(felony_zip_count))
# count(felony_zip_count,ZIPCODE)
# felony_zip_count <- join(zip_df, data.frame(felony_zip_count))
# felony_zip_count[is.na(felony_zip_count$n),"n"] <- 0
# felony_zip_count <- felony_zip_count[order(-felony_zip_count$n),]
# rownames(felony_zip_count) <- NULL
# felony_zip_count
# 
# 
# misdemeanor_sf <- misdemeanor %>%
#   mutate_at(c("Longitude","Latitude"), as.numeric) %>%   # coordinates must be numeric
#   st_as_sf(
#     coords = c("Longitude","Latitude"),
#     agr = "constant",
#     crs = CRS('+proj=longlat +datum=WGS84'),        # nad83 / new york long island projection
#     stringsAsFactors = FALSE,
#     remove = TRUE
#   )
# 
# misdemeanor_in_zip <- st_join(misdemeanor_sf,shapeData, join = st_within)
# count(as_tibble(misdemeanor_in_zip), ZIPCODE)
# misdemeanor_zip_count <- join(zip_df, data.frame(misdemeanor_zip_count))
# misdemeanor_zip_count[is.na(misdemeanor_zip_count$n),"n"] <- 0
# misdemeanor_zip_count <- misdemeanor_zip_count[order(-misdemeanor_zip_count$n),]
# 
# rownames(misdemeanor_zip_count) <- NULL
# misdemeanor_zip_count
# 
# violation_sf <- violation %>%
#   mutate_at(c("Longitude","Latitude"), as.numeric) %>%   # coordinates must be numeric
#   st_as_sf(
#     coords = c("Longitude","Latitude"),
#     agr = "constant",
#     crs = CRS('+proj=longlat +datum=WGS84'),        # nad83 / new york long island projection
#     stringsAsFactors = FALSE,
#     remove = TRUE
#   )
# 
# violation_in_zip <- st_join(violation_sf,shapeData, join = st_within)
# violation_zip_count <- count(as_tibble(violation_in_zip), ZIPCODE)
# 
# violation_zip_count <- join(zip_df, data.frame(violation_zip_count))
# violation_zip_count[is.na(violation_zip_count$n),"n"] <- 0
# violation_zip_count <- violation_zip_count[order(-violation_zip_count$n),]
# 
# rownames(violation_zip_count) <- NULL
# violation_zip_count
# 
# 
# shooting_sf <- nyc_shooting %>%
#   mutate_at(c("Longitude", "Latitude"), as.numeric) %>%   # coordinates must be numeric
#   st_as_sf(
#     coords = c("Longitude", "Latitude"),
#     agr = "constant",
#     crs = CRS('+proj=longlat +datum=WGS84'),        # nad83 / new york long island projection
#     stringsAsFactors = FALSE,
#     remove = TRUE
#   )
# 
# shooting_in_zip <- st_join(shooting_sf,shapeData, join = st_within)
# shooting_zip_count <- count(as_tibble(shooting_in_zip), ZIPCODE)
# 
# shooting_zip_count <- join(zip_df, data.frame(shooting_zip_count))
# shooting_zip_count[is.na(shooting_zip_count$n),"n"] <- 0
# shooting_zip_count <- shooting_zip_count[order(-shooting_zip_count$n),]
# 
# rownames(shooting_zip_count) <- NULL
# 
# write.csv(felony_zip_count, "felony_zip_count.csv", row.names = F)
# write.csv(misdemeanor_zip_count, "misdemeanor_zip_count.csv", row.names = F)
# write.csv(violation_zip_count, "violation_zip_count.csv", row.names = F)
# write.csv(shooting_zip_count, "shooting_zip_count.csv", row.names = F)

felony_zip_count <- read.csv(file = "felony_zip_count.csv", header = TRUE, sep = ",")
misdemeanor_zip_count <- read.csv(file = "misdemeanor_zip_count.csv", header = TRUE, sep = ",")
violation_zip_count <- read.csv(file = "violation_zip_count.csv", header = TRUE, sep = ",")
shooting_zip_count <- read.csv(file = "shooting_zip_count.csv", header = TRUE, sep = ",")
all_crimes_zip_count <- data.frame(ZIPCODE = zip_df, n = felony_zip_count$n + misdemeanor_zip_count$n + violation_zip_count$n)

# longitude and latitude to zip
nyc_summary_map <- leaflet() %>%
  addTiles() %>%
  #addPolygons(data=shapeData,weight=1,col = 'red', fill = NA) %>% 
  setView(-73.9, 40.73, zoom = 10) %>%
  addProviderTiles("CartoDB.Positron")

# Add borderlines for each zipcode area
nyc_summary_map <- nyc_summary_map %>%
  addPolygons(data=shapeData$geometry,weight=1,col = 'grey', fillOpacity = 0,
              highlightOptions = highlightOptions(
                weight = 2,
                color = 'black', fillOpacity = 0,
                bringToFront = TRUE, sendToBack = TRUE),
              popup = shapeData$ZIPCODE
              )

# Mark user input address
if (is.numeric(addr_lon) & is.numeric(addr_lat)){
  nyc_summary_map <- nyc_summary_map %>%
    addAwesomeMarkers(lng = addr_lon, lat = addr_lat, icon = home_icon)
}


# Mark selected zip with bold line
if (!is.na(addr_zip)) {
  nyc_summary_map <- nyc_summary_map %>%
    addPolygons(data=addr_zip_sf,weight=3,col = 'red', fillOpacity = 0)
}

nyc_summary_map

if (!is.na(addr_zip)){
  # given address is top n% in number of felonies reported
  felony_perc <- as.integer(rownames(felony_zip_count)[felony_zip_count$ZIPCODE == addr_zip]) / nrow(zip_df) * 100
  if (length(felony_perc) == 0) {
    felony_perc = 100
  }
  felony_perc
  
  # given address is top n% in number of shooting instances reported
  shooting_perc <- as.integer(rownames(shooting_zip_count)[shooting_zip_count$ZIPCODE == addr_zip]) / nrow(zip_df) * 100
  if (length(shooting_perc) == 0) {
    shooting_perc = 100
  }
  shooting_perc
  
  # given address is top n% in number of misdemeanors reported
  misdemeanor_perc <- as.integer(rownames(misdemeanor_zip_count)[misdemeanor_zip_count$ZIPCODE == addr_zip]) / nrow(zip_df) * 100
  if (length(misdemeanor_perc) == 0) {
    misdemeanor_perc = 100
  }
  misdemeanor_perc
  
  # given address is top n% in number of violations reported
  violation_perc <- as.integer(rownames(violation_zip_count)[violation_zip_count$ZIPCODE == addr_zip]) / nrow(zip_df) * 100
  if (length(violation_perc) == 0) {
    violation_perc = 100
  }
  violation_perc
  
  # given address is top n% in number of all crimes reported
  all_crimes_perc <- as.integer(rownames(all_crimes_zip_count)[all_crimes_zip_count$ZIPCODE == addr_zip]) / nrow(zip_df) * 100
  if (length(all_crimes_perc) == 0) {
    all_crimes_perc = 100
  }
  all_crimes_perc
  
  # summary table for crimes
  crime_summary <- data.frame(all_crimes_perc = all_crimes_perc, shooting_perc = shooting_perc, felony_perc = felony_perc, misdemeanor_perc = misdemeanor_perc, violation_perc = violation_perc)
  crime_summary
}


# boxplot for all crimes
boxplot(all_crimes_zip_count$n,
        main = "All Crimes",
        staplewex = 1,
        col = "lavender",
        border = "darkblue",
        horizontal = TRUE
)
if (!is.na(addr_zip)){
  points(x = all_crimes_zip_count[all_crimes_zip_count$ZIPCODE == addr_zip,"n"],y = 1, pch = 19, cex = 1.15,
         col = "red")
  text(x=all_crimes_zip_count[all_crimes_zip_count$ZIPCODE == addr_zip,"n"], labels = all_crimes_zip_count[all_crimes_zip_count$ZIPCODE == addr_zip,"n"], y = 1.05, cex = 0.7)
}
text(x=boxplot.stats(all_crimes_zip_count$n)$stats[seq(1,5,2)], labels = boxplot.stats(all_crimes_zip_count$n)$stats[seq(1,5,2)], y = 1.25, cex = 0.7)
text(x=boxplot.stats(all_crimes_zip_count$n)$stats[seq(2,5,2)], labels = boxplot.stats(all_crimes_zip_count$n)$stats[seq(2,5,2)], y = 0.75, cex = 0.7)

# boxplot for shooting incidents
boxplot(shooting_zip_count$n,
        main = "Shooting Incidents",
        staplewex = 1,
        col = "lavender",
        border = "darkblue",
        horizontal = TRUE
)
if (!is.na(addr_zip)){
  points(x = shooting_zip_count[shooting_zip_count$ZIPCODE == addr_zip,"n"],y = 1, pch = 19, cex = 1.15,
         col = "red")
  text(x=shooting_zip_count[shooting_zip_count$ZIPCODE == addr_zip,"n"], labels = shooting_zip_count[shooting_zip_count$ZIPCODE == addr_zip,"n"], y = 1.05, cex = 0.7)
}
text(x=boxplot.stats(shooting_zip_count$n)$stats[seq(1,5,2)], labels = boxplot.stats(shooting_zip_count$n)$stats[seq(1,5,2)], y = 1.25, cex = 0.7)
text(x=boxplot.stats(shooting_zip_count$n)$stats[seq(2,5,2)], labels = boxplot.stats(shooting_zip_count$n)$stats[seq(2,5,2)], y = 0.75, cex = 0.7)

# boxplot for felonies
boxplot(felony_zip_count$n,
        main = "Felonies",
        staplewex = 1,
        col = "lavender",
        border = "darkblue",
        horizontal = TRUE
)
if (!is.na(addr_zip)){
  points(x = felony_zip_count[felony_zip_count$ZIPCODE == addr_zip,"n"],y = 1, pch = 19, cex = 1.15,
       col = "red")
  text(x=felony_zip_count[felony_zip_count$ZIPCODE == addr_zip,"n"], labels = felony_zip_count[felony_zip_count$ZIPCODE == addr_zip,"n"], y = 1.05, cex = 0.7)
}
text(x=boxplot.stats(felony_zip_count$n)$stats[seq(1,5,2)], labels = boxplot.stats(felony_zip_count$n)$stats[seq(1,5,2)], y = 1.25, cex = 0.7)
text(x=boxplot.stats(felony_zip_count$n)$stats[seq(2,5,2)], labels = boxplot.stats(felony_zip_count$n)$stats[seq(2,5,2)], y = 0.75, cex = 0.7)

# boxplot for misdemeanors
boxplot(misdemeanor_zip_count$n,
        main = "Misdemeanors",
        staplewex = 1,
        col = "lavender",
        border = "darkblue",
        horizontal = TRUE
)
if (!is.na(addr_zip)){
  points(x = misdemeanor_zip_count[misdemeanor_zip_count$ZIPCODE == addr_zip,"n"],y = 1, pch = 19, cex = 1.15,
       col = "red")
  text(x=misdemeanor_zip_count[misdemeanor_zip_count$ZIPCODE == addr_zip,"n"], labels = misdemeanor_zip_count[misdemeanor_zip_count$ZIPCODE == addr_zip,"n"], y = 1.05, cex = 0.7)
}
text(x=boxplot.stats(misdemeanor_zip_count$n)$stats[seq(1,5,2)], labels = boxplot.stats(misdemeanor_zip_count$n)$stats[seq(1,5,2)], y = 1.25, cex = 0.7)
text(x=boxplot.stats(misdemeanor_zip_count$n)$stats[seq(2,5,2)], labels = boxplot.stats(misdemeanor_zip_count$n)$stats[seq(2,5,2)], y = 0.75, cex = 0.7)

# boxplot for violations
boxplot(violation_zip_count$n,
        main = "Violations",
        staplewex = 1,
        col = "lavender",
        border = "darkblue",
        horizontal = TRUE
)
if (!is.na(addr_zip)){
  points(x = violation_zip_count[violation_zip_count$ZIPCODE == addr_zip,"n"],y = 1, pch = 19, cex = 1.15,
       col = "red")
  text(x=violation_zip_count[violation_zip_count$ZIPCODE == addr_zip,"n"], labels = violation_zip_count[violation_zip_count$ZIPCODE == addr_zip,"n"], y = 1.05, cex = 0.7)
}
text(x=boxplot.stats(violation_zip_count$n)$stats[seq(1,5,2)], labels = boxplot.stats(violation_zip_count$n)$stats[seq(1,5,2)], y = 1.25, cex = 0.7)
text(x=boxplot.stats(violation_zip_count$n)$stats[seq(2,5,2)], labels = boxplot.stats(violation_zip_count$n)$stats[seq(2,5,2)], y = 0.75, cex = 0.7)


air <- read.csv('air.csv')[,c(2:3)]
haz <- read.csv('hazadous.csv')[,c(2:3)]
noise <- read.csv('noise.csv')[,c(2:3)]
rodent <- read.csv('rodent_ars_zip.csv')[,c(2:3)]

air <- join(zip_df, air)
air[is.na(air$air),"air"] <- 0
air <- air[order(-air$air),]
rownames(air) <- NULL

haz <- join(zip_df, haz)
haz[is.na(haz$hazadous),"hazadous"] <- 0
haz <- haz[order(-haz$hazadous),]
rownames(haz) <- NULL

noise <- join(zip_df, noise)
noise[is.na(noise$noise),"noise"] <- 0
noise <- noise[order(-noise$noise),]
rownames(noise) <- NULL

rodent <- join(zip_df, rodent)
rodent[is.na(rodent$rat),"rat"] <- 0
rodent <- rodent[order(-rodent$rat),]
rownames(rodent) <- NULL

all_env <- data.frame(ZIPCODE = zip_df, n = air$air + haz$hazadous + noise$noise + rodent$rat)

if (!is.na(addr_zip)){
  # given address is top n% in number of all environmental pollutions reported
  all_env_perc <- as.integer(rownames(all_env)[all_env$ZIPCODE == addr_zip]) / nrow(zip_df) * 100
  if (length(all_env_perc) == 0) {
    all_env_perc = 100
  }
  all_env_perc
  
  # given address is top n% in number of air pollutions reported
  air_perc <- as.integer(rownames(air)[air$ZIPCODE == addr_zip]) / nrow(zip_df) * 100
  if (length(air_perc) == 0) {
    air_perc = 100
  }
  air_perc
  
  # given address is top n% in number of hazardous materials reported
  haz_perc <- as.integer(rownames(haz)[haz$ZIPCODE == addr_zip]) / nrow(zip_df) * 100
  if (length(haz_perc) == 0) {
    haz_perc = 100
  }
  haz_perc
  
  # given address is top n% in number of noise pollutions reported
  noise_perc <- as.integer(rownames(noise)[noise$ZIPCODE == addr_zip]) / nrow(zip_df) * 100
  if (length(noise_perc) == 0) {
    noise_perc = 100
  }
  noise_perc
  
  # given address is top n% in number of active rat signs reported
  rodent_perc <- as.integer(rownames(rodent)[rodent$ZIPCODE == addr_zip]) / nrow(zip_df) * 100
  if (length(rodent_perc) == 0) {
    rodent_perc = 100
  }
  rodent_perc
  
  # Summary table for environmental factors
  env_summary <- data.frame(all_env_perc = all_env_perc, air_perc = air_perc, haz_perc = haz_perc, noise_perc = noise_perc, rodent_perc = rodent_perc)
  env_summary
}

# boxplot for all environmental pollutions
boxplot(all_env$n,
        main = "All Environmental Factors",
        staplewex = 1,
        col = "lavender",
        border = "darkblue",
        horizontal = TRUE
)
if (!is.na(addr_zip)){
  points(x = all_env[all_env$ZIPCODE == addr_zip,"n"],y = 1, pch = 19, cex = 1.15,
       col = "red")
  text(x=all_env[all_env$ZIPCODE == addr_zip,"n"], labels = all_env[all_env$ZIPCODE == addr_zip,"n"], y = 1.05, cex = 1)
}
text(x=boxplot.stats(all_env$n)$stats[seq(1,5,2)], labels = boxplot.stats(all_env$n)$stats[seq(1,5,2)], y = 1.25, cex = 1)
text(x=boxplot.stats(all_env$n)$stats[seq(2,5,2)], labels = boxplot.stats(all_env$n)$stats[seq(2,5,2)], y = 0.75, cex = 1)



# boxplot for all environmental pollutions
boxplot(air$air,
        main = "Air Pollution",
        staplewex = 1,
        col = "lavender",
        border = "darkblue",
        horizontal = TRUE
)
if (!is.na(addr_zip)){
  points(x = air[air$ZIPCODE == addr_zip,"air"],y = 1, pch = 19, cex = 1.15,
       col = "red")
  text(x=air[air$ZIPCODE == addr_zip,"air"], labels = air[air$ZIPCODE == addr_zip,"air"], y = 1.1, cex = 0.7)
}
text(x=boxplot.stats(air$air)$stats[seq(1,5,2)], labels = boxplot.stats(air$air)$stats[seq(1,5,2)], y = 1.25, cex = 0.7)
text(x=boxplot.stats(air$air)$stats[seq(2,5,2)], labels = boxplot.stats(air$air)$stats[seq(2,5,2)], y = 0.75, cex = 0.7)

# boxplot for hazardous materials
boxplot(haz$hazadous,
        main = "Hazardous Material",
        staplewex = 1,
        col = "lavender",
        border = "darkblue",
        horizontal = TRUE
)
if (!is.na(addr_zip)){
  points(x = haz[haz$ZIPCODE == addr_zip,"hazadous"],y = 1, pch = 19, cex = 1.15,
       col = "red")
  text(x=haz[haz$ZIPCODE == addr_zip,"hazadous"], labels = haz[haz$ZIPCODE == addr_zip,"hazadous"], y = 1.1, cex = 0.7)
}
text(x=boxplot.stats(haz$hazadous)$stats[seq(1,5,2)], labels = boxplot.stats(haz$hazadous)$stats[seq(1,5,2)], y = 1.25, cex = 0.7)
text(x=boxplot.stats(haz$hazadous)$stats[seq(2,5,2)], labels = boxplot.stats(haz$hazadous)$stats[seq(2,5,2)], y = 0.75, cex = 0.7)

# boxplot for noise pollutions
boxplot(noise$noise,
        main = "Noise Pollution",
        staplewex = 1,
        col = "lavender",
        border = "darkblue",
        horizontal = TRUE
)
if (!is.na(addr_zip)){
  points(x = noise[noise$ZIPCODE == addr_zip,"noise"],y = 1, pch = 19, cex = 1.15,
       col = "red")
  text(x=noise[noise$ZIPCODE == addr_zip,"noise"], labels = noise[noise$ZIPCODE == addr_zip,"noise"], y = 1.1, cex = 0.7)
}
text(x=boxplot.stats(noise$noise)$stats[seq(1,5,2)], labels = boxplot.stats(noise$noise)$stats[seq(1,5,2)], y = 1.25, cex = 0.7)
text(x=boxplot.stats(noise$noise)$stats[seq(2,5,2)], labels = boxplot.stats(noise$noise)$stats[seq(2,5,2)], y = 0.75, cex = 0.7)

# boxplot for active rat signs
boxplot(rodent$rat,
        main = "Hazardous Material",
        staplewex = 1,
        col = "lavender",
        border = "darkblue",
        horizontal = TRUE
)
if (!is.na(addr_zip)){
  points(x = rodent[rodent$ZIPCODE == addr_zip,"rat"],y = 1, pch = 19, cex = 1.15,
       col = "red")
  text(x=rodent[rodent$ZIPCODE == addr_zip,"rat"], labels = rodent[rodent$ZIPCODE == addr_zip,"rat"], y = 1.1, cex = 0.7)
}
text(x=boxplot.stats(rodent$rat)$stats[seq(1,5,2)], labels = boxplot.stats(rodent$rat)$stats[seq(1,5,2)], y = 1.25, cex = 0.7)
text(x=boxplot.stats(rodent$rat)$stats[seq(2,5,2)], labels = boxplot.stats(rodent$rat)$stats[seq(2,5,2)], y = 0.75, cex = 0.7)



