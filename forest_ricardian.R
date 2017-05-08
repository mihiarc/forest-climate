# ricardian function: net return to forest land and climate

library(plyr)
library(stargazer)
library(margins)
library(RSQLite)
library(lfe)
library(mfx)
library(margins)
rm(list=ls())

setwd('C:/Users/mihiarc/Google Drive')
conn <- dbConnect(SQLite(), 'Dissertation/database/main.db')
dbListTables(conn)

clm <- dbReadTable(conn, 'forest_climate_30yrmean')
nr <- dbReadTable(conn, 'nr_forestland_v2')
nr.old <- dbReadTable(conn, 'nr_forestland_per_acre_mean') #from earlier version
georef <- dbReadTable(conn, 'fia_regions_mapping')
sitecl <- dbReadTable(conn, 'fia_sitecl')

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

dat.out <- dat
dat.out$nr_base_volwt <- remove_outliers(dat.out$nr_base_volwt)
dat.out <- subset(dat.out, !is.na(nr_base_volwt))


results$nr_pct_change_rcp45 <- remove_outliers(results$nr_pct_change_rcp45)
results <- subset(results, !is.na(nr_pct_change_rcp45))

##
# Cross-sectional estimation
##

## 30-year normal (1981-2011) of annual climate variables


#merge nr, climate, site class

dat <- merge(nr, clm, by='fips')
dat <- merge(dat, sitecl, by='fips')
dat <- merge(dat, georef, by='fips')

dat.old <- merge(nr.old, clm, by='fips')
dat.old <- merge(dat.old, sitecl, by='fips')
dat.old <- merge(dat.old, georef, by='fips')

# dbWriteTable(conn, 'forest_ricardian_estimation_v3', dat)
# dat <- dbReadTable(conn, 'forest_ricardian_estimation_v3')
# 
# dat.west <- dat[dat$mer100=='west',]
# dat.east <- dat[dat$mer100=='east',]

# data2 <- merge(nr, clm, by.x=c('GEOID','year'), by.y = c('fips','year'))
# data2 <- merge(data2, sitecl, by='GEOID')
# data2 <- merge(data2, sitecl_prop, by='GEOID')
# data2$tmean <- sapply(data2, function(x) data2$tmin+data2$tmax/2)[,9]

#ricardian estimation

# # robust check on functional form
# 
# fit0 <- lm(dat, formula = nr_per_acre_forest ~ tmean + ppt)
# 
# fit1 <- lm(dat, formula = nr_per_acre_forest ~ tmean + I(tmean^2) + ppt + I(ppt^2))
# 
# fit2 <- lm(dat, formula = nr_per_acre_forest ~ tmean + I(tmean^2) + ppt + I(ppt^2) + tmean*ppt)
# 
# fit3 <- lm(data = dat, formula = nr_per_acre_forest ~ tmean + I(tmean^2) + ppt + I(ppt^2)+ tmean*ppt+
#              sitecl1 + sitecl2 + sitecl3 + sitecl4 +
#               sitecl5 + sitecl6)
# 
# stargazer(fit0,fit1,fit2,fit3,type='text')

#spatial fixed effects

fit.pref <- lm(data = dat.old, formula = nr_per_acre_forest ~ tmean + I(tmean^2) + ppt + I(ppt^2) + (tmean*ppt) +
                 sitecl1 + sitecl2 + sitecl3 + sitecl4 + sitecl5 + sitecl6 + factor(region))

fit.old <- lm(data = dat.old, formula = nr_per_acre_forest ~ tmean + I(tmean^2) + ppt + I(ppt^2) + (tmean*ppt) +
                 sitecl1 + sitecl2 + sitecl3 + sitecl4 + sitecl5 + sitecl6 + factor(region))
fit.base <- lm(data = dat, formula = nr_base_volwt ~ tmean + I(tmean^2) + ppt + I(ppt^2) + (tmean*ppt) +
                 sitecl1 + sitecl2 + sitecl3 + sitecl4 + sitecl5 + sitecl6 + factor(region))
fit.cc <- lm(data = dat, formula = nr_cc_volwt ~ tmean + I(tmean^2) + ppt + I(ppt^2) + (tmean*ppt) +
                 sitecl1 + sitecl2 + sitecl3 + sitecl4 + sitecl5 + sitecl6 + factor(region))

fit.old <- lm(data = dat.old, formula = nr_per_acre_forest ~ tmean + I(tmean^2) + ppt + I(ppt^2) + (tmean*ppt) +
                pdt + factor(region))
  
fit.base <- lm(data = dat, formula = nr_base_volwt ~ tmean + I(tmean^2) + ppt + I(ppt^2) + (tmean*ppt) +
                 pdt + factor(region))
  
fit.cc <- lm(data = dat, formula = nr_cc_volwt ~ tmean + I(tmean^2) + ppt + I(ppt^2) + (tmean*ppt) + 
               pdt + factor(region))

################

fit.old <- lm(data = dat.old, formula = nr_per_acre_forest ~ tmean + I(tmean^2) + ppt + I(ppt^2) +
                pdt)

fit.base <- lm(data = dat, formula = nr_base_volwt ~ tmean + I(tmean^2) + ppt + I(ppt^2) +
                 pdt)

fit.cc <- lm(data = dat, formula = nr_cc_volwt ~ tmean + I(tmean^2) + ppt + I(ppt^2) +
               pdt)

#####################

fit.old <- lm(data = dat.old, formula = nr_per_acre_forest ~ tmean + I(tmean^2) + ppt + I(ppt^2))

fit.base <- lm(data = dat, formula = nr_base_volwt ~ tmean + I(tmean^2) + ppt + I(ppt^2))

fit.cc <- lm(data = dat, formula = nr_cc_volwt ~ tmean + I(tmean^2) + ppt + I(ppt^2))

#######################

fit.old <- lm(data = dat.old, formula = nr_per_acre_forest ~ tmean + ppt)

fit.base <- lm(data = dat, formula = nr_base_volwt ~ tmean + ppt)

fit.cc <- lm(data = dat, formula = nr_cc_volwt ~ tmean + ppt)


stargazer(fit.pref, type='text')
stargazer(fit.pref,fit.old, type='text')

mfx.pref <- margins(fit.pref)
summary(fit.pref)
mfx.old <- margins(fit.old)
mfx.base <- margins(fit.base)
mfx.cc <- margins(fit.cc)
summary(mfx.old)
summary(mfx.base)
summary(mfx.cc)


ggplot() + 
  geom_histogram(data = dat.old, aes(x=nr_per_acre_forest), fill = 'blue', alpha = 0.2, binwidth = 20)+
  geom_histogram(data = dat, aes(x=nr_base_volwt), fill = "red", alpha = 0.2, binwidth=20)
  
  
  geom_histogram(data = dat.out, aes(x=nr_base_volwt), fill = "green", alpha = 0.2, binwidth=20)
  
  
  
  +
  


# fit5 <- lm(data = dat, formula = nr_per_acre_forest ~ tmean + I(tmean^2) + ppt + I(ppt^2) + I(tmean*ppt) +
#              sitecl1 + sitecl2 + sitecl3 + sitecl4 + sitecl5 + sitecl6 + factor(subregion))
# 
# fit6 <- lm(data = dat, formula = nr_per_acre_forest ~ tmean + I(tmean^2) + ppt + I(ppt^2) + I(tmean*ppt) +
#              sitecl1 + sitecl2 + sitecl3 + sitecl4 + sitecl5 + sitecl6 + factor(state_fips))


# site class proportions versus site class contiuous (pdt variable)

fit.pref <- lm(data = dat, formula = nr_per_acre_forest ~ tmean + I(tmean^2) + ppt + I(ppt^2) + (tmean*ppt) +
                 sitecl1 + sitecl2 + sitecl3 + sitecl4 + sitecl5 + sitecl6 + factor(region))

fit.sitecl.cont <- lm(data = dat, formula = nr_per_acre_forest ~ tmean + I(tmean^2) + ppt + I(ppt^2) + (tmean*ppt) +
                 pdt + factor(region))

#west seperately

fit7 <- lm(data = dat.west, formula = nr_per_acre_forest ~ tmean + I(tmean^2) + ppt + I(ppt^2) + I(tmean*ppt) +
             sitecl1 + sitecl2 + sitecl3 + sitecl4 + sitecl5 + sitecl6 + factor(region))

fit8 <- lm(data = dat.west, formula = nr_per_acre_forest ~ tmean + I(tmean^2) + ppt + I(ppt^2) + I(tmean*ppt) +
             sitecl1 + sitecl2 + sitecl3 + sitecl4 + sitecl5 + sitecl6 + factor(subregion))

stargazer(fit4,fit7,fit8, type='text')

#east seperately

fit9 <- lm(data = dat.east, formula = nr_per_acre_forest ~ tmean + I(tmean^2) + ppt + I(ppt^2) + I(tmean*ppt) +
             sitecl1 + sitecl2 + sitecl3 + sitecl4 + sitecl5 + sitecl6 + factor(region))

fit10 <- lm(data = dat.east, formula = nr_per_acre_forest ~ tmean + I(tmean^2) + ppt + I(ppt^2) + I(tmean*ppt) +
             sitecl1 + sitecl2 + sitecl3 + sitecl4 + sitecl5 + sitecl6 + factor(subregion))


stargazer(fit4,fit9,fit10, type='text')

#east and west versus full model with region fixed effects

stargazer(fit4,fit7,fit9,type='text')

#east and west versus full model without fixed effects

fit11 <- lm(data = dat.west, formula = nr_per_acre_forest ~ tmean + I(tmean^2) + ppt + I(ppt^2) + I(tmean*ppt) +
             sitecl1 + sitecl2 + sitecl3 + sitecl4 + sitecl5 + sitecl6)

fit12 <- lm(data = dat.east, formula = nr_per_acre_forest ~ tmean + I(tmean^2) + ppt + I(ppt^2) + I(tmean*ppt) +
             sitecl1 + sitecl2 + sitecl3 + sitecl4 + sitecl5 + sitecl6)

stargazer(fit3,fit11,fit12,type='text')

# pretty table

stargazer(fit.pref, title="Forest Ricardian Estimation", align=TRUE,
          dep.var.labels='Net Returns to Forestland',
          covariate.labels=c('Mean Annual Temperature','Mean Annual Temperature Squared','Annual Precipitation',
                             'Annual Precipitation Squared','Temperature-Precipitation Interaction'),
          keep.stat = c('n','adj.rsq'),
          omit = c('sitecl1','sitecl2','sitecl3','sitecl4','sitecl5','sitecl6','region'))

stargazer(fit.pref, title="Forest Ricardian Estimation", align=TRUE,
          dep.var.labels='Net Returns to Forestland',
          covariate.labels=c('Mean Annual Temperature','Mean Annual Temperature Squared','Annual Precipitation',
                              'Annual Precipitation Squared','Temperature-Precipitation Interaction'),
          keep.stat = c('n','adj.rsq'),
          style='qje',
          omit = c('sitecl1','sitecl2','sitecl3','sitecl4','sitecl5','sitecl6','region'))

stargazer(fit4.1,fit.lfe.subregion, title="Forest Ricardian Estimation", align=TRUE,
          dep.var.labels='Net Returns to Forestland (Entire U.S.)',
          column.labels = c('No Fixed Effects','Regional FE'),
          covariate.labels=c('Mean Annual Temperature','Mean Annual Temperature Squared','Annual Precipitation',
                             'Annual Precipitation Squared','Temperature-Precipitation Interaction','Productivity Class 1 (Best Quality)',
                             'Productivity Class 2','Productivity Class 3','Productivity Class 4','Productivity Class 5',
                             'Productivity Class 6'),
          keep.stat = c('n','adj.rsq'),
          style='qje')


# reproducable example
# stargazer(linear.1, linear.2, probit.model, title="Regression Results", align=TRUE, 
#           dep.var.labels=c("Overall Rating","High Rating"), 
#           covariate.labels=c("Handling of Complaints","No Special Privileges", 
#                              "Opportunity to Learn","Performance-Based Raises","Too Critical","Advancement"),
#           omit.stat=c("LL","ser","f"), 
#           no.space=TRUE)


mfx_temp <- fit.pref$coefficients[2] + 2*fit.pref$coefficients[3]*dat$tmean +
  fit.pref$coefficients[15]*dat$ppt

amfx_temp <- fit.pref$coefficients[2] + 2*fit.pref$coefficients[3]*mean(dat$tmean) +
  fit.pref$coefficients[15]*mean(dat$ppt)

amfx_precip <- fit.pref$coefficients[4] + 2*fit.pref$coefficients[5]*mean(dat$ppt) +
  fit.pref$coefficients[15]*mean(dat$tmean)

t <- seq(-10,30,1)
p <- seq(0,1928,48)

mfx.temp.smooth <- fit.pref$coefficients[2] + 2*fit.pref$coefficients[3]*t +
  fit.pref$coefficients[15]*p

ggplot()+
  geom_histogram(aes(dat$tmean), binwidth = 1)+
  geom_line(aes(t,mfx.temp.smooth))+
  geom_hline(yintercept = 0, linetype = "dashed")


plot(t,mfx.temp.smooth, type='line')
plot(dat$tmean,mfx_temp)

mfx_ppt <- fit.pref$coefficients[4] + 2*fit.pref$coefficients[5]*mean(dat$ppt) +
  fit.pref$coefficients[6]*mean(dat$tmean)

mfx.temp.seq <- fit.pref$coefficients[2] + 2*fit.pref$coefficients[3]*temp.seq +
  fit.pref$coefficients[6]*ppt.seq

dat$mfx_temp <- fit4.1$coefficients[2] + 2*fit4.1$coefficients[3]*dat$tmean +
                fit4.1$coefficients[6]*dat$ppt

dat$mfx_ppt <- fit4.1$coefficients[4] + 2*fit4.1$coefficients[5]*dat$ppt +
              fit4.1$coefficients[6]*dat$tmean

#tables and figures

library(interplot)



#this plot shows that as precipitation increases, temperature's effect on net returns
#increaes
interplot(m = fit.pref, var1 = "tmean", var2 = "ppt") + 
  xlab("Annual Precipitation (millimeters)") +
  ylab("Estimated Coefficient for\nTemperature") +
  theme_bw() +
  ggtitle("Estimated Coefficient of Temperature \non Net Returns by Precipitation") +
  theme(plot.title = element_text(face="bold"))

# we can also see that as temperature increases, the effect of precipitation on net returns
# increases. This suggests that temperature and precip are compliments; that net returns will be
# higher in places that are generally warmer and wetter. In the southwest for example, temperatures
# averages are high suggesting that net returns should also be high, however because precip is
# low there the high temperature's affect is muted. Similarly, in the Great North Woods of northeastern U.S.
# has significant precipitation which should be good for net returns, but the low temperatures
# keep net returns relatively low. These conditional effects are critical when estimating future net returns
# because the GCMs used for climate predictions sometimes vary widely across models and scenarios.
# While most agree that the U.S. will warm across the entire landscape, there is great disagreement about
# the level of precipitation change and the spatial pattern.

interplot(m=fit.pref, var1="ppt", var2 = "tmean")

interplot(m = fit.pref, var1 = "ppt", var2 = "tmean") + 
  # Add labels for X and Y axes
  xlab("Average Annual Temperature (Celcius)") +
  ylab("Estimated Coefficient for\nPrecipitation") +
  # Change the background
  theme_bw() +
  # Add the title
  ggtitle("Estimated Coefficient of Precipitation \non Net Returns by Temperature") +
  theme(plot.title = element_text(face="bold"))



interplot(m = fit.test, var1 = "tmean", var2 = "tmean", hist = T) + 
  xlab('Average Annual Temperature (Celcius)') +
  ylab("Estimated Coefficient for\nTemperature") +
  theme_bw() +
  ggtitle("Estimated Coefficient of Temperature \non Net Returns by Precipitation") +
  theme(plot.title = element_text(face="bold"))+
  geom_hline(yintercept = 0, linetype = "dashed")

interplot(m = fit.pref, var1 = "tmean",var2 = 'nr_per_acre_for', hist = T) + 
  xlab('Average Annual Temperature (Celcius)') +
  ylab("Estimated Coefficient for\nTemperature") +
  theme_bw() +
  ggtitle("Estimated Coefficient of Temperature \non Net Returns by Precipitation") +
  theme(plot.title = element_text(face="bold"))+
  geom_hline(yintercept = 0, linetype = "dashed")


library(effects)
# 
# test <- allEffects(fit.test)
# 
# test <- effect('ppt',fit.test)
# plot(test)
# test <- effect('ppt:tmean',fit.test)

library(mfx)
library(margins)

fit.test <- lm(data=est.dat, nr_per_acre_forest ~ tmean + I(tmean^2) + ppt + I(ppt^2) + 
                 tmean*ppt + pdt + factor(region))

stargazer(fit.test,fit.pref,type='text')

fit.pref.mfx <- margins(fit.pref)
fit.pdt.mfx <- margins(fit.sitecl.cont)
summary(fit.pref.mfx)
summary(fit.pdt.mfx)


cplot(test)
plot.margins(fit.pref)
plot(test$tmean,test$dydx_tmean)
plot(test$ppt, test$dydx_ppt)

mfx1 <- margins(fit.pref)
mfx2 <- margins(fit2)
mfx3 <- margins(fit3)
mfx4.1 <- margins(fit4.1)


summary(mfx1)
summary(mfx2)
summary(mfx4.1)

cplot(fit4.1,'tmean', main = "Predicted Value of Temperature")
cplot(fit4.1,'ppt')

cplot(fit4, "tmean", what = "effect", main = "Average Marginal Effect of Temperature \n (Model 2)")
cplot(fit2, "ppt", what = "effect", main = "Average Marginal Effect of Precipitation \n (Model 2)")

# cplot(fit1, "tmean", what = "effect", main = "Average Marginal Effect of Temperature \n (Model 1)")
# cplot(fit1, "ppt", what = "effect", main = "Average Marginal Effect of Precipitation \n (Model 1)")


# cplot(fit1,'tmean')
# cplot(fit1,'ppt')

# plot(mfx1[[1]])

# (5.4227-5.3139)/((0.6581^2/1900+0.6581^2/1900))^(1/2)
# (0.0308-0.0194)/((0.0162^2/1900+0.0157^2/1900))^(1/2)

stargazer(fit2, type='text')

#predictions

dat$nr_predicted <- predict.lm(fit4.1)

# #use 30-year normal (1981-2011) of Jan,April,Jul,Oct climate variables (temp,precip)
# 
# clm$month <- as.numeric(substring(clm[,2],5,6))
# clm_cs <- subset(clm, year<=2011)
# clm_cs <- clm_cs[,2:9]
# clm_cs <- rename(clm_cs, c('tmean.month'='tmean'))
# clm_cs <- ddply(clm_cs, .(fips,st,month), summarize,
#                 ppt=mean(ppt), tmax=mean(tmax), tmin=mean(tmin), tmean=mean(tmean))
# 
# nr_cs <- subset(nr, year==2012)
# 
# data_cs <- merge(nr_cs, clm_cs, by.x='GEOID', by.y='fips')
# data_cs <- merge(data_cs, sitecl, by='GEOID')
# data_cs <- subset(data_cs, month==1 | month==4 | month==7 | month==10)
# data_cs$month[data_cs$month==1] <- 'jan'
# data_cs$month[data_cs$month==4] <- 'apr'
# data_cs$month[data_cs$month==7] <- 'jul'
# data_cs$month[data_cs$month==10] <- 'oct'
# 
# #reshape to get seasonal clim variables
# 
# seas_clm <- data_cs[,c(1,6:10)]
# seas_clm <- melt(seas_clm, id.vars = c('GEOID','month'))
# seas_clm <- dcast(seas_clm, GEOID ~ month + variable)
# 
# data_cs <- merge(data_cs, seas_clm, by="GEOID")
# 
# fit_cs1 <- lm(data = data_cs, formula = mean_nr ~ jan_ppt + apr_ppt + jul_ppt + oct_ppt +
#                 jan_tmean + apr_tmean + jul_tmean + oct_tmean)
# 
# fit_cs2 <- lm(data = data_cs, formula = mean_nr ~ ppt + tmean)
# 
# stargazer(fit_cs1, fit_cs2, type='text', title = 'Cross-sectional estimation of Climate effects on Forest Returns (2012)')
# 
# data <- merge(nr, clm, by=c('GEOID','year'))
# data <- merge(data, sitecl, by='GEOID', all.x=T)
# 
# data <- data[-c(3,5,10:11)]
# data$tmean_prcp <- data$tmean * data$prcp
# 
# #OLS
# 
# fit1 <- lm(mean_nr ~ tmean + tmean_sq + prcp + prcp_sq + tmean_prcp + sitecl1 +
#              sitecl2 + sitecl3 + sitecl4 + sitecl5 + sitecl6 + sitecl7, data = data)
# 
# fit1.2 <- lm(mean_nr ~ tmean + tmean_sq + prcp + prcp_sq + sitecl1 +
#                sitecl2 + sitecl3 + sitecl4 + sitecl5 + sitecl6 + sitecl7, data = data)
# 
# #OLS with year fixed effects
# 
# fit2 <- lm(mean_nr ~ tmean + tmean_sq + prcp + prcp_sq + tmean_prcp + sitecl1 +
#              sitecl2 + sitecl3 + sitecl4 + sitecl5 + sitecl6 + sitecl7 + factor(year), data=data)
# 
# stargazer(fit2,type = 'text', omit = 'year',
#           omit.labels = 'Year Fixed Effects')
# 
# #OLS with county and year fixed effects
# 
# fit3 <- lm(mean_nr ~ tmean + tmean_sq + prcp + prcp_sq + tmean_prcp + sitecl1 +
#              sitecl2 + sitecl3 + sitecl4 + sitecl5 + sitecl6 + sitecl7 + factor(GEOID) + factor(year),
#            data=data)
# 
# #within estimator
# 
# fit4 <- plm(mean_nr ~ tmean + tmean_sq + prcp + prcp_sq + tmean_prcp + sitecl1 +
#               sitecl2 + sitecl3 + sitecl4 + sitecl5 + sitecl6 + sitecl7 + factor(year), data=data, index='GEOID',
#             model='within')
# 
# #year fixed effects with weights
# #forestland weights
# #net return weights
# 
# stargazer(data, type='text', omit = c(1:2,5,7:15))
# stargazer(fit1, fit2, fit3, type = 'text')
# 
# stargazer(fit1,fit2,fit3,omit=c('year','GEOID'),
#           type = 'text')
# 
# stargazer(fit1, fit2, fit3, omit = c('year','GEOID'),
#           omit.labels = c('Year Fixed Effects','County Fixed Effects'), type='text',
#           title = 'Ricardian Estimation',
#           dep.var.caption='Dependant Variable: Net Returns to Forest',
#           column.sep.width = '0.5pt',
#           font.size = 'tiny')
# 
# stargazer(fit1, fit2, fit3, fit5, type='text',
#           title            = "Ricardian Estimation",
#           covariate.labels = c("Temperature", "Wind speed", "Rain (inches)",
#                                "2nd quarter", "3rd quarter", "Fourth quarter"),
#           dep.var.caption  = "A better caption",
#           dep.var.labels   = "Flight delay (in minutes)")
# 
