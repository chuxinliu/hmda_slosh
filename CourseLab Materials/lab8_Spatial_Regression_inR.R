setwd("F:/GitHub/hmda_slosh/Lab8")
require(data.table)
require(sp)
require(spdep)
require(gstat)

#read hmda_slosh
hmda_slosh <- fread("hmda_slosh.csv", 
                    stringsAsFactors = F, 
                    data.table = F)

summary(hmda_slosh)
hmda_slosh$x <- hmda_slosh$intptlon
hmda_slosh$y <- hmda_slosh$intptlat
coordinates(hmda_slosh)=~x+y
proj4string(hmda_slosh)=CRS("+init=epsg:4269")
plot(hmda_slosh)

coords <- coordinates(hmda_slosh)
IDs <- row.names(hmda_slosh)
neigh_close <- knn2nb(knearneigh(coords, k=1),row.names=IDs)
dists <- unlist(nbdists(neigh_close, coords))
dists
max_min <- max(dists)
max_min
summary(dists)

#variogram
loan_cor <- variogram(loan_ed~x+y, data=hmda_slosh)
plot(loan_cor)
loan_cor
loan_cor_fit <- fit.variogram(loan_cor, vgm(c("Sph","Exp","Mat")))
plot<-plot(loan_cor, loan_cor_fit, 
           main="Variogram:\n Distance Threshold for Spatial Effect on Loan Approval")
plot 
range_mi <- prop_cor_fit$range[2]/69
range_mi 
max_min
dist_nb_mm <- dnearneigh(coords, d1=0, d2=max_min*1.1, row.names=IDs)
summary(dist_nb_mm)
fl_nbq_w_mm<-nb2listw(dist_nb_mm)

#lag term
hmda_slosh$lag_loan_ed<-lag.listw(fl_nbq_w_mm,hmda_slosh$loan_ed)
summary(hmda_slosh$loan_ed)
summary(hmda_slosh$lag_loan_ed)
lm <- lm(lag_loan_ed ~ loan_ed, data=hmda_slosh)
smoothScatter(hmda_slosh$loan_ed, hmda_slosh$lag_loan_ed)
abline(lm)

hmda_slosh_data<-as.data.frame(hmda_slosh)
head(hmda_slosh_data)

moran.test(hmda_slosh$loan_ed, listw=fl_nbq_w_mm)
moran.plot(as.vector(hmda_slosh$loan_ed), listw=fl_nbq_w_mm,labels=F, 
           xlim=c(0,0.8),ylim=c(0.3,0.5),
           main="Moran's I = 0.062, p-value < 0.001", 
           xlab="Loan Approval Rate",ylab="Spatial Lag Loan Approval Rate",pch=1)

locI <- localmoran(hmda_slosh$loan_ed, fl_nbq_w_mm)
head(locI)

summary(hmda_slosh_data)
fl.lm <- lm(loan_ed ~ ln_loan_amount
            + ln_applicant_income
            + c1_high0
            , data=hmda_slosh_data)
summary(fl.lm)

fl.lagrange <- lm.LMtests(fl.lm,fl_nbq_w_mm, test=c("LMerr","RLMerr","LMlag","RLMlag","SARMA"))
print(fl.lagrange)

fl.sp_durbin <- lagsarlm(loan_ed ~ ln_loan_amount
                         + ln_applicant_income
                         + c1_high0
                         , data=hmda_slosh_data
                         , fl_nbq_w_mm, 
                         type = "mixed")
summary(fl.sp_durbin)

hmda_slosh_data$lm_resid <- resid(fl.lm)
hmda_slosh_data$sp_resid <- resid(fl.sp_durbin)



moran.test(hmda_slosh_data$lm_resid, listw=fl_nbq_w_mm)
moran.test(hmda_slosh_data$sp_resid, listw=fl_nbq_w_mm)

