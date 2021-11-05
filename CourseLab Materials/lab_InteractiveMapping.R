#install.packages("RPostgres")
#require(RPostgres)
#
#conn <- dbConnect(RPostgres::Postgres(), dbname = 'advanced_gis', 
#                  host = 'localhost', # i.e. 'your computer'
#                  port = 5432, # or any other port specified by your DBA
#                  user = 'postgres',
#                  password = 'jeremy')
#
#
#options(digits = 10)
#
#oc_sale_data <- dbFetch(dbSendQuery(conn, "SELECT * FROM lab4.prop_val_sample_oc"))
#oc_prop_flood <- dbFetch(dbSendQuery(conn, "SELECT * FROM lab6.mhhw_slosh_sample"))
#oc_assessor <- dbFetch(dbSendQuery(conn, "SELECT * FROM lab7.oc_assessor"))

require(data.table)
oc_sale_data <- fread("prop_val_sample_oc.csv", stringsAsFactors = F, data.table=F)
oc_prop_flood <- fread("mhhw_slosh_sample.csv", stringsAsFactors = F, data.table=F)
oc_assessor <- fread("oc_assessor_data.csv", stringsAsFactors = F, data.table=F)



cols.num <- c("pr_sq_ft","saleyear","x","y","samp_pr05")
oc_sale_data[cols.num] <- sapply(oc_sale_data[cols.num],as.numeric)
cols.num2 <- c("arealotacres","areabuilding","x","y","bedroomscount","yearbuilt",
               "unitscount","beachprox","near_golf","singfam")
oc_assessor[cols.num2] <- sapply(oc_assessor[cols.num2],as.numeric)
oc_full<-cbind(oc_sale_data,oc_prop_flood)
names(oc_full)
head(oc_full)
summary(oc_full)

oc_full[oc_full==99.9]<-0
oc_full$samp_pr05 <- ifelse(is.na(oc_full$samp_pr05),
                            190.76477, oc_full$samp_pr05)
names(oc_full)
summary(oc_full)

names(oc_assessor)
oc_join <- merge(oc_full, oc_assessor, by="fsid")
oc_join$geom<-NULL
oc_join$id<-NULL

summary(oc_join)
names(oc_join)
oc_join$lat <- oc_join$y.x
oc_join$long <- oc_join$x.x
oc_join$year <- as.numeric(oc_join$saleyear)

elev_yr_effect <- 0.03426 #From Lab8's model

oc_join$yr_vs_start <- oc_join$saleyear - 2005
oc_join$water_diff <- as.numeric(as.character(oc_join$water_diff))
oc_join$elv_wat_yr <- oc_join$yr_vs_start*oc_join$water_diff


oc_join$elev_effect <- (elev_yr_effect*oc_join$water_diff)*oc_join$yr_vs_start
summary(oc_join$elev_effect)
hist(oc_join$elev_effect)

oc_join$slr_loss <- ifelse(oc_join$elev_effect < 0, 1, 0)
summary(oc_join$slr_loss)

oc_join$slr_loss_neg <- ifelse(oc_join$elev_effect >= 0, 0, oc_join$elev_effect)


#install.packages("sp")
require(sp)
coordinates(oc_join)=~long+lat
proj4string(oc_join)=CRS("+init=epsg:4269")
plot(oc_join)


#install.packages("leaflet")
require(leaflet)

names(oc_join)
map1 <- leaflet(data=oc_join) %>%
  addTiles() %>%
  addMarkers(~x.y, ~y.y, 
             popup = paste("SLR $Lost (sqft): $", oc_join$slr_loss_neg, "<br>",
                           "Elev to High Tide inches:", oc_join$water_diff, "<br>",
                           "C1 Surge in feet:", oc_join$c1_high, "<br>",
                           "C2 Surge in feet:", oc_join$c2_high))
map1

map2 <- leaflet(data=oc_join) %>%
  addTiles() %>%
  addMarkers(~x.y, ~y.y, 
             popup = paste("SLR $Lost (sqft): $", oc_join$slr_loss_neg, "<br>",
                           "Elev to High Tide inches:", oc_join$water_diff, "<br>",
                           "C1 Surge in feet:", oc_join$c1_high, "<br>",
                           "C2 Surge in feet:", oc_join$c2_high),
             clusterOptions = markerClusterOptions())
map2

oc_join_loss <- subset(oc_join, slr_loss_neg < 0)

map3 <- leaflet(data=oc_join_loss) %>%
  addTiles() %>%
  addMarkers(~x.y, ~y.y, 
             popup = paste("SLR $Lost (sqft): $", oc_join_loss$slr_loss_neg, "<br>",
                           "Elev to High Tide inches:", oc_join_loss$water_diff, "<br>",
                           "C1 Surge in feet:", oc_join_loss$c1_high, "<br>",
                           "C2 Surge in feet:", oc_join_loss$c2_high),
             clusterOptions = markerClusterOptions())
map3

map3 %>% addProviderTiles(providers$CartoDB.Positron)
map3 %>% addProviderTiles(providers$Stamen.Toner)
map3 %>% addProviderTiles(providers$Esri.NatGeoWorldMap)
map3 %>% addProviderTiles(providers$MtbMap)
map3 %>% addProviderTiles(providers$Stamen.TerrainBackground)
map3 %>% addProviderTiles(providers$Esri.WorldImagery)
map3 %>% addProviderTiles(providers$Stamen.Watercolor)
map3 %>% addProviderTiles(providers$MtbMap) %>%
  addProviderTiles(providers$Stamen.TonerLines,
                   options = providerTileOptions(opacity = 0.40)) %>%
  addProviderTiles(providers$Stamen.TonerLabels)



map4 <- leaflet(data=oc_join_loss) %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  addMarkers(~x.y, ~y.y, 
             popup = paste("SLR $Lost (sqft): $", oc_join_loss$slr_loss_neg, "<br>",
                           "Elev to High Tide inches:", oc_join_loss$water_diff, "<br>",
                           "C1 Surge in feet:", oc_join_loss$c1_high, "<br>",
                           "C2 Surge in feet:", oc_join_loss$c2_high),
             clusterOptions = markerClusterOptions())
map4


install.packages("shiny")
#.rs.restartR()
require(leaflet)
require(shiny)


shinyApp(ui = fluidPage(titlePanel("Sales of Sea Level Rise Affected Homes, 2005-2016"),
                        sliderInput("Slider", 
                                    "Adjust Years Here", 
                                    min = 2005, 
                                    max = 2016, 
                                    value = 1, 
                                    sep=""),
                        mainPanel(textOutput("MainPanel"))),
         server = function(input, output){
           output$MainPanel = renderText(input$slider)
         }
)

install.packages("bit64")
oc_join_df <- as.data.frame(oc_join_loss@data)

ui = fluidPage(titlePanel("Sales of Sea Level Rise Affected Homes, 2005-2016"),
                        sliderInput(inputId = "Slider",
                                    label = "Adjust Years Here", 
                                    min = min(oc_join_df$saleyear), 
                                    max = max(oc_join_df$saleyear), 
                                    value = 1, 
                                    sep=""), 
               leafletOutput("map5"))
                        

server = function(input, output){
           
          output$map5 <- renderLeaflet({
             leaflet(oc_join_loss) %>%
               addProviderTiles(providers$Esri.WorldImagery) %>%
               setView(lat = 38.334095, lng = -75.085471, zoom = 13)
             
           })
           
           observe({
             
             leafletProxy(mapId = "map5", data = oc_join_loss) %>% 
               clearMarkers() %>%
               addMarkers(lng = oc_join_loss$x.x, lat = oc_join_loss$y.x, 
                          popup = paste("SLR $Lost (sqft): $", oc_join_loss$slr_loss_neg, "<br>",
                                        "Elev to High Tide inches:", oc_join_loss$elev_water, "<br>",
                                        "C1 Surge in feet:", oc_join_loss$c1_high, "<br>",
                                        "C2 Surge in feet:", oc_join_loss$c2_high),
                          clusterOptions = markerClusterOptions())
           })
}
           

shinyApp(ui, server)



server = function(input, output){
  
  data_filtered <- reactive ({
    oc_join_df[oc_join_df$saleyear <= input$Slider, ]
    
  })
  
  
  output$map5 <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      setView(lat = 38.334095, lng = -75.085471, zoom = 13)
    
  })
  
  observe({
    
    leafletProxy(mapId = "map5", data = data_filtered()) %>% 
      clearMarkers() %>%
      addMarkers(lng = data_filtered()$x.x, lat = data_filtered()$y.x, 
                 popup = paste("SLR $Lost (sqft): $", data_filtered()$slr_loss_neg, "<br>",
                               "Elev to High Tide inches:", data_filtered()$elev_water, "<br>",
                               "C1 Surge in feet:", data_filtered()$c1_high, "<br>",
                               "C2 Surge in feet:", data_filtered()$c2_high),
                 clusterOptions = markerClusterOptions())
  })
}


shinyApp(ui, server)




#install.packages("htmlwidgets")
#require(htmlwidgets)
##
#saveWidget(map4, file="spatio_termporal.html")
#



server = function(input, output){
  
  data_filtered <- reactive ({
    oc_join_df[oc_join_df$saleyear <= input$Slider, ]
    
  })
  
  
  output$map5 <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      setView(lat = 38.334095, lng = -75.085471, zoom = 11)
    
  })
  
  observe({
    
    leafletProxy(mapId = "map5", data = data_filtered()) %>% 
      clearMarkers() %>%
      addMarkers(lng = data_filtered()$x.x, lat = data_filtered()$y.x, 
                 popup = paste("SLR $Lost (sqft): $", data_filtered()$slr_loss_neg, "<br>",
                               "Elev to High Tide inches:", data_filtered()$elev_water, "<br>",
                               "C1 Surge in feet:", data_filtered()$c1_high, "<br>",
                               "C2 Surge in feet:", data_filtered()$c2_high))
  })
}


shinyApp(ui, server)





ui = fluidPage(titlePanel("Sales of Sea Level Rise Affected Homes, 2006-2018"),
               sliderInput(inputId = "Slider",
                           label = "Adjust Years Here", 
                           min = min(oc_join_df$saleyear), 
                           max = max(oc_join_df$saleyear), 
                           value = 1, 
                           sep="",
                           animate = animationOptions(interval = 500, 
                                                      loop = TRUE)), 
               leafletOutput("map5"))


server = function(input, output){
  
  data_filtered <- reactive ({
    oc_join_df[oc_join_df$saleyear <= input$Slider, ]
    
  })
  
  
  output$map5 <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      setView(lat = 38.334095, lng = -75.085471, zoom = 11)
    
  })
  
  observe({
    
    leafletProxy(mapId = "map5", data = data_filtered()) %>% 
      clearMarkers() %>%
      addMarkers(lng = data_filtered()$x.x, lat = data_filtered()$y.x, 
                 popup = paste("SLR $Lost (sqft): $", data_filtered()$slr_loss_neg, "<br>",
                               "Elev to High Tide inches:", data_filtered()$elev_water, "<br>",
                               "C1 Surge in feet:", data_filtered()$c1_high, "<br>",
                               "C2 Surge in feet:", data_filtered()$c2_high))
  })
}


shinyApp(ui, server)
