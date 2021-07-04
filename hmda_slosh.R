library(haven)
library(lifecycle)
require(data.table)
require(sp)
require(spdep)
require(gstat)

setwd("F:/GitHub/hmda_slosh")
data <- read_dta('hmda_slosh_cleaned.dta')
centroids_slosh <- read_dta("centroids_slosh/centroids_slosh.dta")
df_xy <- read_dta("FL_census_tract/ct_00_10.dta")
head(df_xy)
df_xy$x <- df_xy$intptlon
df_xy$y <- df_xy$intptlat
coordinates(df_xy)=~x+y
proj4string(df_xy)=CRS("+init=epsg:4269")

coords <- coordinates(df_xy)
IDs <- row.names(df_xy)
neigh_close <- knn2nb(knearneigh(coords, k=1),row.names=IDs)
dists <- unlist(nbdists(neigh_close, coords))
dists
max_min <- max(dists)
max_min
summary(dists)

vrg_y1 <- variogram(loan_ed~1, data=data)
plot(vrg_y1)
vrg_y1
vrg_y1_fit <- fit.variogram(prop_cor, vgm(c("Sph","Exp","Mat")))
vrg_y1_fit
vrg_y1_fit$range[2]
