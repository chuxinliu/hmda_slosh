require(RPostgres)

conn <- dbConnect(RPostgres::Postgres(), dbname = 'advanced_gis', 
                  host = 'localhost', # i.e. 'your computer'
                  port = 5432, # or any other port specified by your DBA
                  user = 'postgres',
                  password = 'jeremy')


options(digits = 10)

oc_sale_data <- dbFetch(dbSendQuery(conn, "SELECT * FROM lab4.prop_val_sample_oc"))

oc_prop_flood <- dbFetch(dbSendQuery(conn, "SELECT * FROM lab6.mhhw_slosh_sample"))

oc_assessor <- dbFetch(dbSendQuery(conn, "SELECT * FROM lab7.oc_assessor"))

require(data.table)
setwd("C:/Users/Jeremy/Desktop/Spring2020(remote)/Advanced_GIS/Lab7_Instructions/Lab 7")
oc_sale_data <- fread("prop_val_sample_oc.csv", stringsAsFactors = F, data.table = F)
oc_prop_flood <- fread("mhhw_slosh_sample.csv", stringsAsFactors = F, data.table = F)
oc_assessor <- fread("oc_assessor_data.csv", stringsAsFactors = F, data.table = F)

summary(oc_prop_flood)

summary(oc_sale_data)
cols.num <- c("pr_sq_ft","saleyear","x","y","samp_pr05")
oc_sale_data[cols.num] <- sapply(oc_sale_data[cols.num],as.numeric)

summary(oc_assessor)
cols.num2 <- c("arealotacres","areabuilding","x","y","bedroomscount","yearbuilt",
               "unitscount","beachprox","near_golf","singfam")
oc_assessor[cols.num2] <- sapply(oc_assessor[cols.num2],as.numeric)

summary(oc_prop_flood)
summary(oc_sale_data)
summary(oc_assessor)


head(oc_prop_flood$id)
head(oc_sale_data$id)

oc_full<-cbind(oc_sale_data,oc_prop_flood)
head(oc_full)
summary(oc_full)

oc_full[oc_full==99.9]<-0
summary(oc_full)
oc_full$samp_pr05 <- ifelse(is.na(oc_full$samp_pr05),
                            188.59, 
                            oc_full$samp_pr05)
summary(oc_full)


require(sp)
coordinates(oc_full)=~x+y
proj4string(oc_full)=CRS("+init=epsg:4269")
plot(oc_full)

coordinates(oc_assessor)=~x+y
proj4string(oc_assessor)=CRS("+init=epsg:4269")
plot(oc_assessor)


oc_join <- merge(oc_full@data, oc_assessor@data, by="fsid")
head(oc_join)


######GWR model
install.packages("spgwr")
library(spgwr)

names(oc_join)

lm_homechar <- lm(pr_sq_ft ~ 
                    arealotacres + 
                    areabuilding +
                    yearbuilt +
                    unitscount + 
                    singfam, 
                  data=oc_join)
summary(lm_homechar)



lm_value <- lm(pr_sq_ft ~ 
                 arealotacres + 
                 areabuilding +
                 yearbuilt + 
                 unitscount + 
                 singfam + 
                 samp_pr05 + 
                 factor(tractfp), 
               data=oc_join)
summary(lm_value)



lm_flood <- lm(pr_sq_ft ~ 
                 arealotacres + 
                 areabuilding +
                 yearbuilt + 
                 unitscount + 
                 singfam + 
                 samp_pr05 + 
                 factor(tractfp) + 
                 factor(fld_zone) + 
                 water_diff, 
               data=oc_join)
summary(lm_flood)



oc_join$yr_vs_start <- oc_join$saleyear-2005

lm_floodtime <- lm(pr_sq_ft ~ 
                     arealotacres + 
                     areabuilding +
                     yearbuilt + 
                     unitscount + 
                     samp_pr05 + 
                     factor(tractfp) + 
                     factor(fld_zone) + 
                     water_diff +
                     yr_vs_start + 
                     water_diff:yr_vs_start, 
                   data=oc_join)
summary(lm_floodtime)

names(oc_join)

oc_xy <- oc_sale_data[,c("fsid","x","y")]
oc_xy <- subset(oc_xy,!duplicated(fsid))
oc_join <- merge(oc_join,oc_xy,by="fsid")


coordinates(oc_join)=~x+y
proj4string(oc_join)=CRS("+init=epsg:4269")
plot(oc_join)


bwG <- gwr.sel(pr_sq_ft ~ 
                 arealotacres + 
                 areabuilding +
                 yearbuilt + 
                 unitscount + 
                 samp_pr05 + 
                 factor(tractfp) +
                 factor(fld_zone) + 
                 water_diff + 
                 yr_vs_start + 
                 water_diff:yr_vs_start, 
               data=oc_join, 
               gweight=gwr.Gauss, 
               verbose=TRUE)


oc_join$elv_wat_yr <- oc_join$water_diff*oc_join$yr_vs_start

start <- Sys.time()
gwrG <- gwr(pr_sq_ft ~ 
              arealotacres + 
              areabuilding + 
              yearbuilt + 
              unitscount + 
              samp_pr05 + 
              factor(tractfp) +
              factor(fld_zone) + 
              water_diff + 
              yr_vs_start + 
              elv_wat_yr, 
            data=oc_join, 
            bandwidth=bwG, 
            gweight=gwr.Gauss)
end <- Sys.time()
print(end-start)

gwrG

names(gwrG)

names (gwrG$SDF)
head(gwrG$SDF)

spplot(gwrG$SDF, "localR2")

spplot(gwrG$SDF, "gwr.e")

spplot(gwrG$SDF, "water_diff")

spplot(gwrG$SDF, "elv_wat_yr")





require(ggplot2)
gwr_results <- as.data.frame(gwrG$SDF)


map <- ggplot() + 
  geom_point(data=gwr_results, 
             aes(x=x, y=y), 
             color="red")
map

map2 <- ggplot() + 
  geom_point(data=gwr_results, aes(x=x, y=y, color=localR2)) +
  labs(x="", y="", title = "Variation in Local R-Square") +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), 
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), 
        plot.title = element_text(face="bold", hjust=0.5, vjust=-132, size=12)) +
  scale_colour_gradientn("Model\nFit", 
                         colours=c( "#f9f3c2","#660000")) +
  theme(legend.position = c(0.90,0.23),
        legend.text=element_text(size=7),
        legend.title=element_text(size=7))

map2


map3 <- ggplot() + 
  geom_point(data=gwr_results, aes(x=x, y=y, color=water_diff)) +
  labs(x="", y="", title = "Variation in Effect of Elevation") +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), 
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), 
        plot.title = element_text(face="bold", hjust=0.5, vjust=-132, size=12)) +
  scale_colour_gradientn("Elevation\nEffect", 
                         colours=c( "#f9f3c2","#660000")) +
  theme(legend.position = c(0.90,0.23),
        legend.text=element_text(size=7),
        legend.title=element_text(size=7))

map3


map4 <- ggplot() + 
  geom_point(data=gwr_results, aes(x=x, y=y, color=elv_wat_yr)) +
  labs(x="", y="", title = "Variation in Elevation Effect by Time") +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), 
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), 
        plot.title = element_text(face="bold", hjust=0.5, vjust=-132, size=12)) +
  scale_colour_gradientn("Elevation\nOver Time", 
                         colours=c( "#f9f3c2","#660000")) +
  theme(legend.position = c(0.90,0.23),
        legend.text=element_text(size=7),
        legend.title=element_text(size=7))

map4


map5 <- ggplot() + 
  geom_point(data=gwr_results, aes(x=x, y=y, color=gwr.e)) +
  labs(x="", y="", title = "GWR Residuals") +
  theme(axis.ticks.y = element_blank(),axis.text.y = element_blank(), 
        axis.ticks.x = element_blank(),axis.text.x = element_blank(), 
        plot.title = element_text(face="bold", hjust=0.5, vjust=-132, size=12)) +
  scale_colour_gradientn("GWR\nResiduals", 
                         colours=c( "#f9f3c2","#660000")) +
  theme(legend.position = c(0.90,0.23),
        legend.text=element_text(size=7),
        legend.title=element_text(size=7))

map5

#install.packages("ggpubr")
require(ggpubr)

multi <- ggarrange(map2, map5, map3, map4, ncol=2, nrow=2)
multi <- annotate_figure(multi, fig.lab = "Figure 1. GWR Results",
                         fig.lab.pos = "top.left", fig.lab.size = 16,
                         fig.lab.face = "bold")
multi
