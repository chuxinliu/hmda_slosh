require(spdep)
require(maptools)
require(rgdal)

getwd()
TRI_poly<-readOGR(dsn="C:/Users/Jeremy/Desktop/spring2019/Adv_GIS/week11",layer="zonal_stats_TRI")
names(TRI_poly)
plot(TRI_poly)

TRI_poly <- spTransform(TRI_poly, CRS("+init=epsg:4326"))
TRI_poly$overall <- (TRI_poly$LengthLife +
                       TRI_poly$Quali_Life + TRI_poly$Soc_Econ + TRI_poly$healthbeh +
                       TRI_poly$Phy_env + TRI_poly$Clini_care) / 6
summary(TRI_poly$overall)

TRI_poly$LengthLife_1 <- ifelse(TRI_poly$LengthLife<=0,"Below Avg.","Above Avg.")
TRI_poly$healthbeh_1 <- ifelse(TRI_poly$healthbeh<=0,"Below Avg.","Above Avg.")
TRI_poly$Clini_care_1 <- ifelse(TRI_poly$Clini_care<=0,"Below Avg.","Above Avg.")
TRI_poly$Soc_Econ_1 <- ifelse(TRI_poly$Soc_Econ<=0,"Below Avg.","Above Avg.")
TRI_poly$Phy_env_1 <- ifelse(TRI_poly$Phy_env<=0,"Below Avg.","Above Avg.")
TRI_poly$Quali_Life_1 <- ifelse(TRI_poly$Quali_Life<=0,"Below Avg.","Above Avg.")


binpal <- colorBin("YlOrRd", domain=TRI_poly$overall, 5)


map6 <- leaflet(TRI_poly) %>%
  setView(-89.814,32.794, 6) %>%
  addTiles()

map6 

map6 %>% addPolygons()
map6 <- map6 %>% addPolygons(fillColor = ~binpal(overall),
                             weight = 2,
                             color = 'black',
                             fillOpacity = 0.5,
                             popup = paste("County:", TRI_poly$NAME, "<br>",
                                           "Quality of Life:", TRI_poly$Quali_Life_1, "<br>",
                                           "Length of Life:", TRI_poly$LengthLife_1, "<br>",
                                           "Pyhsical Environment", TRI_poly$Phy_env_1, "<br>",
                                           "Health Behaviors:", TRI_poly$healthbeh_1, "<br>",
                                           "Clinical Care:", TRI_poly$Clini_care_1, "<br>",
                                           "Socioeconomic Char.:", TRI_poly$Soc_Econ_1, "<br>"))

require(htmlwidgets)
getwd()
saveWidget(map6, file="MS_Health.html")