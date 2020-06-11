
nyc_shooting <- read.csv(file="nyc_shooting.csv", header=TRUE, sep=",")
felony <- read.csv(file = "felony.csv", header = TRUE, sep = ",")
misdemeanor <- read.csv(file = "misdemeanor.csv", header = TRUE, sep = ",")
violation <- read.csv(file = "violation.csv", header = TRUE, sep = ",")

childcare <-read_csv(file = 'childcare_AppUsed.csv')
colnames(childcare) <- make.names(colnames(childcare), unique=TRUE)
# childcare <-read.csv(file = 'childcare_AppUsed.csv')
zipcode <- st_read('ZIP_CODE_040114.shp')
shapeData <- st_transform(zipcode, CRS('+proj=longlat +datum=WGS84'))

zip_df <- read.csv("zip_df.csv")

names(zip_df) <- "ZIPCODE"

felony_zip_count <- read.csv(file = "felony_zip_count.csv", header = TRUE, sep = ",")
felony_zip_count <- felony_zip_count[order(-felony_zip_count$freq),]
rownames(felony_zip_count) <- NULL
misdemeanor_zip_count <- read.csv(file = "misdemeanor_zip_count.csv", header = TRUE, sep = ",")
misdemeanor_zip_count <- misdemeanor_zip_count[order(-misdemeanor_zip_count$freq),]
rownames(misdemeanor_zip_count) <- NULL
violation_zip_count <- read.csv(file = "violation_zip_count.csv", header = TRUE, sep = ",")
violation_zip_count <- violation_zip_count[order(-violation_zip_count$freq),]
rownames(violation_zip_count) <- NULL
shooting_zip_count <- read.csv(file = "shooting_zip_count.csv", header = TRUE, sep = ",")
shooting_zip_count <- shooting_zip_count[order(-shooting_zip_count$freq),]
rownames(shooting_zip_count) <- NULL
all_crimes_zip_count<-left_join(zip_df,felony_zip_count,by="ZIPCODE")
all_crimes_zip_count<-left_join(all_crimes_zip_count,misdemeanor_zip_count,by="ZIPCODE")
all_crimes_zip_count<-left_join(all_crimes_zip_count,violation_zip_count,by="ZIPCODE")
all_crimes_zip_count<-left_join(all_crimes_zip_count,shooting_zip_count,by="ZIPCODE")
colnames(all_crimes_zip_count)<-c("ZIPCODE","felony","misdemeanor","violation","shooting")

all_crimes_zip_count$n<-all_crimes_zip_count$felony+all_crimes_zip_count$misdemeanor+all_crimes_zip_count$violation
all_crimes_zip_count <- all_crimes_zip_count[order(-all_crimes_zip_count$n),]
rownames(all_crimes_zip_count) <- NULL
home_icon<- awesomeIcons(
  icon = 'home',
  library = 'ion',
  markerColor = 'green'
)


server = function(input, output, session) {
  
  location1 <- reactive({
    input$address1
  })
  
  location2 <- reactive({
    input$address2
  })
  
  output$userp<-renderText({paste("<b>Welcome to Find Cozy Home NYC!</b><br>"
                                  ,"FiCoH NYC is designed to provide you with living environment information like safety, other environmental factors and nearby childcare centers based on the zip code or the address you provide. Note that only your address and the matching zipcode for the address will be considered if you provide both.<br>",
                                  "<b>At a Glance</b> gives you nyc map divided by zipcode. You can click on each section to find out the zipcode of the area. This tab will give you general information of your desired area for all three categories.<br>",
                                  "<b>Safety tab</b> gives you more specific information by showing you boxplots with your desired area marked in red, so you can find out in a glance the safety level of the region compared to other regions of the city.<br>",
                                  "<b>Environment tab</b> operates similarly as the crime feature, showing the environmental pollution and related information of the desired area.<br>",
                                  "<b>Childcare tab</b> let you set conditions for the center you're looking for and gives you information on the centers that match the conditions.We only included childcare centers that had violation rates less than average of their categories, but for those of you who want to take extra care choosing the center, we also included an option for centers with no violations. <br><br>",
                                  "We hope you find your dream home. Good luck and enjoy exploring!")})
  
  output$plot1<-renderLeaflet({
    
    addr <- location1()
    
    addr_lon <- street2coordinates(addr)$longitude
    addr_lat <- street2coordinates(addr)$latitude
    if(is.null(addr_lon) & is.null(addr_lat)){
      addr_zip <- location2()
      if (length(zip_df[zip_df$ZIPCODE==addr_zip,]) == 0){
        addr_zip <- NA
      }
      else{
        addr_zip_sf <- shapeData[shapeData$ZIPCODE == addr_zip, "geometry"]
      }
    }
    else{
      addr_zip <- NA
      # get zipcode from address given by the user
      if (is.numeric(addr_lon) & is.numeric(addr_lat)){
        addr_st <- st_point(c(addr_lon, addr_lat))
        idx <- as.integer(st_intersects(addr_st, shapeData$geometry))
        addr_zip <- shapeData$ZIPCODE[idx]
        addr_zip_sf <- shapeData$geometry[idx]
      }
    }
    
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
    
  })
  
  output$text1 <- renderText({"The selected area rank top n% among the whoe city for the given categories in the first two tables."})
  
  output$table1a<-renderTable({
    addr <- location1()
    
    addr_lon <- street2coordinates(addr)$longitude
    addr_lat <- street2coordinates(addr)$latitude
    if(is.null(addr_lon) & is.null(addr_lat)){
      addr_zip <- location2()
      if (length(zip_df[zip_df$ZIPCODE==addr_zip,]) == 0){
        addr_zip <- NA
      }
      else{
        addr_zip_sf <- shapeData[shapeData$ZIPCODE == addr_zip, "geometry"]
      }
    }
    else{
      addr_zip <- NA
      # get zipcode from address given by the user
      if (is.numeric(addr_lon) & is.numeric(addr_lat)){
        addr_st <- st_point(c(addr_lon, addr_lat))
        idx <- as.integer(st_intersects(addr_st, shapeData$geometry))
        addr_zip <- shapeData$ZIPCODE[idx]
        addr_zip_sf <- shapeData$geometry[idx]
      }
    }
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
        all_crimes_perc = 200
      }
      all_crimes_perc
      
      # summary table for crimes
      crime_summary <- data.frame(all_crimes_perc = all_crimes_perc, shooting_perc = shooting_perc, felony_perc = felony_perc, misdemeanor_perc = misdemeanor_perc, violation_perc = violation_perc)
      crime_summary
    }
    
  })
  
  output$table1b<-renderTable({
    addr <- location1()
    
    addr_lon <- street2coordinates(addr)$longitude
    addr_lat <- street2coordinates(addr)$latitude
    if(is.null(addr_lon) & is.null(addr_lat)){
      addr_zip <- location2()
      if (length(zip_df[zip_df$ZIPCODE==addr_zip,]) == 0){
        addr_zip <- NA
      }
      else{
        addr_zip_sf <- shapeData[shapeData$ZIPCODE == addr_zip, "geometry"]
      }
    }
    else{
      addr_zip <- NA
      # get zipcode from address given by the user
      if (is.numeric(addr_lon) & is.numeric(addr_lat)){
        addr_st <- st_point(c(addr_lon, addr_lat))
        idx <- as.integer(st_intersects(addr_st, shapeData$geometry))
        addr_zip <- shapeData$ZIPCODE[idx]
        addr_zip_sf <- shapeData$geometry[idx]
      }
    }
    
    if (!is.na(addr_zip)){
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
  })
  
  
  output$table1c<-renderTable({
    addr <- location1()
    
    addr_lon <- street2coordinates(addr)$longitude
    addr_lat <- street2coordinates(addr)$latitude
    if(is.null(addr_lon) & is.null(addr_lat)){
      addr_zip <- location2()
      if (length(zip_df[zip_df$ZIPCODE==addr_zip,]) == 0){
        addr_zip <- NA
      }
      else{
        addr_zip_sf <- shapeData[shapeData$ZIPCODE == addr_zip, "geometry"]
      }
    }
    else{
      addr_zip <- NA
      # get zipcode from address given by the user
      if (is.numeric(addr_lon) & is.numeric(addr_lat)){
        addr_st <- st_point(c(addr_lon, addr_lat))
        idx <- as.integer(st_intersects(addr_st, shapeData$geometry))
        addr_zip <- shapeData$ZIPCODE[idx]
        addr_zip_sf <- shapeData$geometry[idx]
      }
    }
    
    if (!is.na(addr_zip)){
      childcare <- childcare[childcare$lessThanAve == 1,]
      childcare[is.na(childcare$Child.Care.Type),]
      childcare <- childcare[complete.cases(childcare),]
      
      g <- childcare %>% group_by(ZipCode, Child.Care.Type, noViolation) %>%
        tally()
      
      groups <- data.frame(Child.Care.Type = rep(unique(g$Child.Care.Type), each = 2), noViolation = rep(c(0,1), times = 3))
      childcare_summary <- join(groups, g[g$ZipCode == addr_zip,], by = c("Child.Care.Type", "noViolation"))
      childcare_summary[is.na(childcare_summary$n),"n"] <- 0
      
      childcare_summary$noViolation <- as.logical(childcare_summary$noViolation)
      childcare_summary$n <- as.logical(childcare_summary$n)
      names(childcare_summary)[4] <- "Exists"
      
      childcare_summary <- subset(childcare_summary, select = c("Child.Care.Type", "noViolation", "Exists"))
      childcare_summary
      
      childcare_summary
    }
  })
  
  
  output$plot2<-renderLeaflet({
    
    addr <- location1()
    
    addr_lon <- street2coordinates(addr)$longitude
    addr_lat <- street2coordinates(addr)$latitude
    if(is.null(addr_lon) & is.null(addr_lat)){
      addr_zip <- location2()
      if (length(zip_df[zip_df$ZIPCODE==addr_zip,]) == 0){
        addr_zip <- NA
      }
      else{
        addr_zip_sf <- shapeData[shapeData$ZIPCODE == addr_zip, "geometry"]
      }
    }
    else{
      addr_zip <- NA
      # get zipcode from address given by the user
      if (is.numeric(addr_lon) & is.numeric(addr_lat)){
        addr_st <- st_point(c(addr_lon, addr_lat))
        idx <- as.integer(st_intersects(addr_st, shapeData$geometry))
        addr_zip <- shapeData$ZIPCODE[idx]
        addr_zip_sf <- shapeData$geometry[idx]
      }
    }
    
    # get zipcode from address given by the user
    if (is.numeric(addr_lon) & is.numeric(addr_lat)){
      addr_st <- st_point(c(addr_lon, addr_lat))
      idx <- as.integer(st_intersects(addr_st, shapeData$geometry))
      addr_zip <- shapeData$ZIPCODE[idx]
      addr_zip_sf <- shapeData$geometry[idx]
    }
    
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
    
  })
  
  
  output$boxplot2a <- renderPlot({
    addr <- location1()
    
    addr_lon <- street2coordinates(addr)$longitude
    addr_lat <- street2coordinates(addr)$latitude
    if(is.null(addr_lon) & is.null(addr_lat)){
      addr_zip <- location2()
      if (length(zip_df[zip_df$ZIPCODE==addr_zip,]) == 0){
        addr_zip <- NA
      }
      else{
        addr_zip_sf <- shapeData[shapeData$ZIPCODE == addr_zip, "geometry"]
      }
    }
    else{
      addr_zip <- NA
      # get zipcode from address given by the user
      if (is.numeric(addr_lon) & is.numeric(addr_lat)){
        addr_st <- st_point(c(addr_lon, addr_lat))
        idx <- as.integer(st_intersects(addr_st, shapeData$geometry))
        addr_zip <- shapeData$ZIPCODE[idx]
        addr_zip_sf <- shapeData$geometry[idx]
      }
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
      text(x=all_crimes_zip_count[all_crimes_zip_count$ZIPCODE == addr_zip,"n"], labels = all_crimes_zip_count[all_crimes_zip_count$ZIPCODE == addr_zip,"n"], y = 1.05, cex = 1)
    }
    text(x=boxplot.stats(all_crimes_zip_count$n)$stats[seq(1,5,2)], labels = boxplot.stats(all_crimes_zip_count$n)$stats[seq(1,5,2)], y = 1.25, cex = 1)
    text(x=boxplot.stats(all_crimes_zip_count$n)$stats[seq(2,5,2)], labels = boxplot.stats(all_crimes_zip_count$n)$stats[seq(2,5,2)], y = 0.75, cex = 1)
  })
  
  
  output$boxplot2b <- renderPlot({
    addr <- location1()
    
    addr_lon <- street2coordinates(addr)$longitude
    addr_lat <- street2coordinates(addr)$latitude
    if(is.null(addr_lon) & is.null(addr_lat)){
      addr_zip <- location2()
      if (length(zip_df[zip_df$ZIPCODE==addr_zip,]) == 0){
        addr_zip <- NA
      }
      else{
        addr_zip_sf <- shapeData[shapeData$ZIPCODE == addr_zip, "geometry"]
      }
    }
    else{
      addr_zip <- NA
      # get zipcode from address given by the user
      if (is.numeric(addr_lon) & is.numeric(addr_lat)){
        addr_st <- st_point(c(addr_lon, addr_lat))
        idx <- as.integer(st_intersects(addr_st, shapeData$geometry))
        addr_zip <- shapeData$ZIPCODE[idx]
        addr_zip_sf <- shapeData$geometry[idx]
      }
    }
    
    par(mfrow=c(2,2))
    
    # boxplot for shooting incidents
    boxplot(shooting_zip_count$freq,
            main = "Shooting Incidents",
            staplewex = 1,
            col = "lavender",
            border = "darkblue",
            horizontal = TRUE
    )
    if (!is.na(addr_zip)){
      points(x = shooting_zip_count[shooting_zip_count$ZIPCODE == addr_zip,"freq"],y = 1, pch = 19, cex = 1.15,
             col = "red")
      text(x=shooting_zip_count[shooting_zip_count$ZIPCODE == addr_zip,"freq"], labels = shooting_zip_count[shooting_zip_count$ZIPCODE == addr_zip,"freq"], y = 1.1, cex = 0.7)
    }
    # text(x=boxplot.stats(shooting_zip_count$n)$stats[seq(1,5,2)], labels = boxplot.stats(shooting_zip_count$n)$stats[seq(1,5,2)], y = 1.25, cex = 0.7)
    # text(x=boxplot.stats(shooting_zip_count$n)$stats[seq(2,5,2)], labels = boxplot.stats(shooting_zip_count$n)$stats[seq(2,5,2)], y = 0.75, cex = 0.7)
    
    # boxplot for felonies
    boxplot(felony_zip_count$freq,
            main = "Felonies",
            staplewex = 1,
            col = "lavender",
            border = "darkblue",
            horizontal = TRUE
    )
    if (!is.na(addr_zip)){
      points(x = felony_zip_count[felony_zip_count$ZIPCODE == addr_zip,"freq"],y = 1, pch = 19, cex = 1.15,
             col = "red")
      text(x=felony_zip_count[felony_zip_count$ZIPCODE == addr_zip,"freq"], labels = felony_zip_count[felony_zip_count$ZIPCODE == addr_zip,"freq"], y = 1.1, cex = 0.7)
    }
    # text(x=boxplot.stats(felony_zip_count$n)$stats[seq(1,5,2)], labels = boxplot.stats(felony_zip_count$n)$stats[seq(1,5,2)], y = 1.25, cex = 0.7)
    # text(x=boxplot.stats(felony_zip_count$n)$stats[seq(2,5,2)], labels = boxplot.stats(felony_zip_count$n)$stats[seq(2,5,2)], y = 0.75, cex = 0.7)
    
    # boxplot for misdemeanors
    boxplot(misdemeanor_zip_count$freq,
            main = "Misdemeanors",
            staplewex = 1,
            col = "lavender",
            border = "darkblue",
            horizontal = TRUE
    )
    if (!is.na(addr_zip)){
      points(x = misdemeanor_zip_count[misdemeanor_zip_count$ZIPCODE == addr_zip,"freq"],y = 1, pch = 19, cex = 1.15,
             col = "red")
      text(x=misdemeanor_zip_count[misdemeanor_zip_count$ZIPCODE == addr_zip,"freq"], labels = misdemeanor_zip_count[misdemeanor_zip_count$ZIPCODE == addr_zip,"freq"], y = 1.1, cex = 0.7)
    }
    # text(x=boxplot.stats(misdemeanor_zip_count$n)$stats[seq(1,5,2)], labels = boxplot.stats(misdemeanor_zip_count$n)$stats[seq(1,5,2)], y = 1.25, cex = 0.7)
    # text(x=boxplot.stats(misdemeanor_zip_count$n)$stats[seq(2,5,2)], labels = boxplot.stats(misdemeanor_zip_count$n)$stats[seq(2,5,2)], y = 0.75, cex = 0.7)
    
    # boxplot for violations
    boxplot(violation_zip_count$freq,
            main = "Violations",
            staplewex = 1,
            col = "lavender",
            border = "darkblue",
            horizontal = TRUE
    )
    if (!is.na(addr_zip)){
      points(x = violation_zip_count[violation_zip_count$ZIPCODE == addr_zip,"freq"],y = 1, pch = 19, cex = 1.15,
             col = "red")
      text(x=violation_zip_count[violation_zip_count$ZIPCODE == addr_zip,"freq"], labels = violation_zip_count[violation_zip_count$ZIPCODE == addr_zip,"freq"], y = 1.1, cex = 0.7)
    }
    # text(x=boxplot.stats(violation_zip_count$n)$stats[seq(1,5,2)], labels = boxplot.stats(violation_zip_count$n)$stats[seq(1,5,2)], y = 1.25, cex = 0.7)
    # text(x=boxplot.stats(violation_zip_count$n)$stats[seq(2,5,2)], labels = boxplot.stats(violation_zip_count$n)$stats[seq(2,5,2)], y = 0.75, cex = 0.7)
    
  })
  
  
  output$plot3<-renderLeaflet({
    
    air<-read.csv("air.csv")
    air<-air[,2:3]
    air$ZIPCODE<-as.factor(air$ZIPCODE)
    noise<-read.csv("noise.csv")
    noise<-noise[,2:3]
    noise$ZIPCODE<-as.factor(noise$ZIPCODE)
    hazadous<-read.csv('hazadous.csv')
    hazadous<-hazadous[,2:3]
    hazadous$ZIPCODE<-as.factor(hazadous$ZIPCODE)
    rodent_ars_zip<-read.csv("rodent_ars_zip.csv")
    rodent_ars_zip<-rodent_ars_zip[,2:3]
    rodent_ars_zip$ZIPCODE<-as.factor(rodent_ars_zip$ZIPCODE)
    ## interactive map, merge these four datasets together.
    colnames(hazadous)<-c("ZIPCODE","hazadous")
    colnames(air)<-c("ZIPCODE","air")
    colnames(noise)<-c("ZIPCODE","noise")
    colnames(rodent_ars_zip)<-c("ZIPCODE","rat")
    zip_code <- st_read('ZIP_CODE_040114.shp')
    zip_code <- subset(zip_code, select = c(ZIPCODE,geometry))
    zip_code<-left_join(zip_code,air,by="ZIPCODE")
    zip_code<-left_join(zip_code,noise,by="ZIPCODE")
    zip_code<-left_join(zip_code,hazadous,by="ZIPCODE")
    zip_code<-left_join(zip_code,rodent_ars_zip,by="ZIPCODE")
    
    shapeData <- st_transform(zip_code, CRS("+proj=longlat +datum=WGS84"))
    
    
    pal <- colorNumeric(
      palette = "Blues",
      domain = NULL)
    
    nyc_map <- leaflet() %>%
      addTiles() %>%
      addPolygons(data=shapeData,weight=1,color = ~pal(air), fillOpacity = 1,label = ~paste0("Air Pollution: ", air), highlight = highlightOptions(weight = 2,color = "red",bringToFront = TRUE),group="Air Pollution") %>%
      addPolygons(data=shapeData,weight=1,color = ~pal(noise), fillOpacity = 1,label = ~paste0("Noise Pollution: ", noise), highlight = highlightOptions(weight = 2,color = "red",bringToFront = TRUE),group="Noise Pollution") %>%
      addPolygons(data=shapeData,weight=1,color = ~pal(hazadous), fillOpacity = 1,label = ~paste0("Hazadous Materials: ", hazadous), highlight = highlightOptions(weight = 2,color = "red",bringToFront = TRUE),group="Hazadous Materials") %>%
      addPolygons(data=shapeData,weight=1,color = ~pal(rat), fillOpacity = 1,label = ~paste0("Active Rat Sign: ", rat), highlight = highlightOptions(weight = 2,color = "red",bringToFront = TRUE),group="Active Rat Sign")%>%
      addLegend(pal = pal, values = c(shapeData$air,shapeData$noise,shapeData$hazadous,shapeData$rat), opacity = 0.7, title = NULL,   position = "bottomright") %>%
      setView(-73.9, 40.73, zoom = 10) %>%
      addProviderTiles("CartoDB.Positron")  %>%
      addLayersControl(baseGroups = c("Air Pollution", "Noise Pollution","Hazadous Materials","Active Rat Sign"))
    
    nyc_map
    
    
  })
  
  
  output$boxplot3a <- renderPlot({
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
    
    addr <- location1()
    
    addr_lon <- street2coordinates(addr)$longitude
    addr_lat <- street2coordinates(addr)$latitude
    if(is.null(addr_lon) & is.null(addr_lat)){
      addr_zip <- location2()
      if (length(zip_df[zip_df$ZIPCODE==addr_zip,]) == 0){
        addr_zip <- NA
      }
      else{
        addr_zip_sf <- shapeData[shapeData$ZIPCODE == addr_zip, "geometry"]
      }
    }
    else{
      addr_zip <- NA
      # get zipcode from address given by the user
      if (is.numeric(addr_lon) & is.numeric(addr_lat)){
        addr_st <- st_point(c(addr_lon, addr_lat))
        idx <- as.integer(st_intersects(addr_st, shapeData$geometry))
        addr_zip <- shapeData$ZIPCODE[idx]
        addr_zip_sf <- shapeData$geometry[idx]
      }
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
    
  })
  
  output$boxplot3b <- renderPlot({
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
    
    addr <- location1()
    
    addr_lon <- street2coordinates(addr)$longitude
    addr_lat <- street2coordinates(addr)$latitude
    if(is.null(addr_lon) & is.null(addr_lat)){
      addr_zip <- location2()
      if (length(zip_df[zip_df$ZIPCODE==addr_zip,]) == 0){
        addr_zip <- NA
      }
      else{
        addr_zip_sf <- shapeData[shapeData$ZIPCODE == addr_zip, "geometry"]
      }
    }
    else{
      addr_zip <- NA
      # get zipcode from address given by the user
      if (is.numeric(addr_lon) & is.numeric(addr_lat)){
        addr_st <- st_point(c(addr_lon, addr_lat))
        idx <- as.integer(st_intersects(addr_st, shapeData$geometry))
        addr_zip <- shapeData$ZIPCODE[idx]
        addr_zip_sf <- shapeData$geometry[idx]
      }
    }
    
    par(mfrow=c(2,2))
    
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
            main = "Active Rat Signs",
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
    
  })
  
  output$plot4<-renderLeaflet({
    
    
    childcare<-subset(childcare,Child.Care.Type==input$childcare)
    childcare<-childcare[childcare[,input$variable]==1,]
    addr <- location1()
    
    addr_lon <- street2coordinates(addr)$longitude
    addr_lat <- street2coordinates(addr)$latitude
    if(is.null(addr_lon) & is.null(addr_lat)){
      addr_zip <- location2()
      if (length(zip_df[zip_df$ZIPCODE==addr_zip,]) == 0){
        addr_zip <- NA
      }
      else{
        addr_zip_sf <- shapeData[shapeData$ZIPCODE == addr_zip, "geometry"]
      }
    }
    else{
      addr_zip <- NA
      # get zipcode from address given by the user
      if (is.numeric(addr_lon) & is.numeric(addr_lat)){
        addr_st <- st_point(c(addr_lon, addr_lat))
        idx <- as.integer(st_intersects(addr_st, shapeData$geometry))
        addr_zip <- shapeData$ZIPCODE[idx]
        addr_zip_sf <- shapeData$geometry[idx]
      }
    }
    
    zc=addr_zip
    
    if(input$nearby){
      childcare<-childcare[childcare$ZipCode==zc,]
    }
    
    
    
    icon <- awesomeIcons(
      icon = 'ios-close',
      iconColor = 'black',
      library = 'ion',
      markerColor = 'red'
    )
    nyc_map <- leaflet() %>%
      addTiles() %>%
      setView(-73.9, 40.73, zoom = 10) %>%
      addProviderTiles("CartoDB.Positron") 
    
    if(nrow(childcare) != 0){
      nyc_map <- nyc_map %>%
        addAwesomeMarkers(data=childcare,childcare$longitude,childcare$latitude,label = as.character(childcare$Center.Name),icon=icon)
    }     
    
    nyc_map
    
    
    
  })
  
  output$table4<-renderTable({
    childcare<-subset(childcare,Child.Care.Type==input$childcare)
    childcare<-childcare[childcare[,input$variable]==1,]
    
    addr <- location1()
    
    addr_lon <- street2coordinates(addr)$longitude
    addr_lat <- street2coordinates(addr)$latitude
    if(is.null(addr_lon) & is.null(addr_lat)){
      addr_zip <- location2()
      if (length(zip_df[zip_df$ZIPCODE==addr_zip,]) == 0){
        addr_zip <- NA
      }
      else{
        addr_zip_sf <- shapeData[shapeData$ZIPCODE == addr_zip, "geometry"]
      }
    }
    else{
      addr_zip <- NA
      # get zipcode from address given by the user
      if (is.numeric(addr_lon) & is.numeric(addr_lat)){
        addr_st <- st_point(c(addr_lon, addr_lat))
        idx <- as.integer(st_intersects(addr_st, shapeData$geometry))
        addr_zip <- shapeData$ZIPCODE[idx]
        addr_zip_sf <- shapeData$geometry[idx]
      }
    }
    
    zc=addr_zip
    
    if (!is.na(addr_zip)){
      if(input$nearby){
        childcare<-childcare[childcare$ZipCode==zc,]
      }
      if(nrow(childcare) != 0){
        addr <- subset(childcare, select = c(Building, Street, Borough))
        levels(addr$Borough)<-c(levels(addr$Borough), "NEW YORK")
        addr[addr$Borough == "MANHATTAN", 3] <- "NEW YORK"
        addr$address <- paste(addr$Building, addr$Street, addr$Borough,"NEW YORK", sep = ",")
        od<-childcare[,c(2,8,11)]
        od$address<-addr$address
        od<-distinct(od)
        od
      }
    }}
    
  )
}

