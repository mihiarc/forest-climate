# process faustmann output


#bind and merge output from matlab script 'faustmann-solution' and save to main.db database

setwd('C:/Users/mihiarc/Google Drive/Lab Files')
soln1 <- read.csv('soln1.csv', header = F)
soln2 <- read.csv('soln2.csv', header = F)
soln3 <- read.csv('soln3.csv', header = F)
soln4 <- read.csv('soln4.csv', header = F)
soln5 <- read.csv('soln5.csv', header = F)
soln6 <- read.csv('soln6.csv', header = F)
soln7 <- read.csv('soln7.csv', header = F)
soln8 <- read.csv('test3.csv', header = F)
soln8 <- soln8[soln8$V2!=0,]
solnb <- rbind(soln1,soln2,soln3,soln4,soln5,soln6,soln7,soln8)
soln9 <- read.csv('nr_test.csv', header = F)
soln9 <- soln9[soln9$V2!=0,]
soln10 <- read.csv('optage_test.csv', header = F)
soln10 <- soln10[soln10$V2!=0,]
soln9b <- read.csv('nr_test2.csv', header = F)
soln9b <- soln9b[soln9b$V2!=0,]
soln10b <- read.csv('optage_test2.csv', header = F)
soln10b <- soln10b[soln10b$V2!=0,]
soln9 <- rbind(soln9,soln9b)
soln10 <- rbind(soln10,soln10b)
soln <- merge(soln9,soln10,by='V1')
colnames(soln) <- c('V1','V4','V5','V2','V3')
soln <- rbind(soln,solnb)
colnames(soln) <- c('id','nr_base','nr_cc','age_base','age_cc')
soln$id <- formatC(soln$id, format = 'd', width = 7, flag='0')

library(RSQLite)
conn <- dbConnect(SQLite(), 'C:/Users/mihiarc/Google Drive/Dissertation/database/main.db')
dbListTables(conn)
dbWriteTable(conn, 'faustmann_solution_canesm2rcp45', soln)

rm(list = ls())

###################################
# explore data
###################################

conn <- dbConnect(SQLite(), 'C:/Users/mihiarc/Google Drive/Dissertation/database/main.db')
dbListTables(conn)
dat <- dbReadTable(conn, 'faustmann_solution_canesm2rcp45')

library(stargazer)

stargazer(dat, type='text')
hist(dat$nr_base)
hist(dat$age_base)

library(ggplot2)

# how does the age distribution change under future climate scenario canesm2rcp45?

ggplot(dat) +
  geom_density(colour="blue", adjust=4, aes(x=age_base))+
  geom_density(colour="red", adjust=4, aes(x=age_cc))+
  xlim(20,30)

# how does net return change?

ggplot(dat) +
  geom_density(colour="blue", adjust=4, aes(x=nr_base))+
  geom_density(colour="red", adjust=4, aes(x=nr_cc))+
  xlim(-1,5)

ggplot(dat) +
  geom_histogram(fill='blue',alpha=.5,aes(x=nr_base),binwidth = .05)+
  geom_histogram(fill='red',alpha=.2,aes(x=nr_cc), binwidth = .05)+
  xlim(-1,5)

# map percentage change in age and nr

# prepare data

dat$fips <- substr(dat$id,1,5)
dat$spgrpcd <- substr(dat$id,6,7)

# load growing stock volume data to weight the variables

library(plyr)
gsv <- dbReadTable(conn, "gsvol_weight_spgrpcd_spcd")
gsv <- ddply(gsv, .(fips,SPGRPCD), summarize, gsvolcf=sum(gsvolcf,na.rm = T))
             
             
             
###################################################################################
### scale net return values:
### faustmann solution is the net return to a patch of land capable of supporting
### the full rotation of a tree in a particular species group.
### To convert net return from the patch level to the acre level,
### we scale the net return value by the average trees per acre in a county
### according to the tree's forest type group.
##################################################################################

acres <- dbReadTable(conn, 't003_timber_acres')
trees <- dbReadTable(conn, 't008_number_of_trees')
gsv <- dbReadTable(conn, "t018_timber_volume")

#drop non-private land and average over all evaluation groups

acres <- ddply(acres, .(STATECD,COUNTYCD,OWNGRPCD,FORTYPCD), summarize, acres=mean(T003_Area.of.timberland.acres))
acres <- acres[acres$OWNGRPCD==40,]
trees <- ddply(trees, .(STATECD,COUNTYCD,OWNGRPCD,SPGRPCD,FORTYPCD), summarize,
               trees=mean(trees))
trees <- trees[trees$OWNGRPCD==40,]
trees$fips <- paste(formatC(trees$STATECD, format='d', width = 2, flag = '0'),
                    formatC(trees$COUNTYCD, format = 'd', width = 3, flag='0'), sep='')
trees$spgrpcd <- formatC(trees$SPGRPCD, format = 'd', width = 2, flag='0')

#load fortyp-spgrp key

key <- dbReadTable(conn, "key_forest_species")

acres <- merge(acres,key,by.x='FORTYPCD',by.y='fortypcd')
acres$fips <- paste(formatC(acres$STATECD, format='d', width = 2, flag = '0'),
                    formatC(acres$COUNTYCD, format = 'd', width = 3, flag='0'), sep='')
acres <- ddply(acres, .(fips,spgrpcd), summarize, acres=mean(acres))
acres$spgrpcd <- formatC(acres$spgrpcd, format = 'd', width = 2, flag='0')

trees.acres <- merge(trees,acres, by = c('fips','spgrpcd'))

trees.acres$tpa <- trees.acres$trees / trees.acres$acres

tpa <- trees.acres[c(1,2,7:10)]

# merge tpa with net return

nr <- merge(dat, tpa, by=c('fips','spgrpcd'))
nr$nr_base <- nr$nr_base * nr$tpa
nr$nr_cc <- nr$nr_cc * nr$tpa

#################################
## net returns to all timberland species

#compute weighted average net return and non-weighted nr

nr.volwt <- merge(nr,gsv,by.x=c('fips','spgrpcd'),by.y=c('fips','SPGRPCD'))
nr.volwt <- ddply(nr.volwt, .(fips), summarize, nr_base_volwt=weighted.mean(nr_base, w=gsvolcf),
                     nr_cc_volwt=weighted.mean(nr_cc, w=gsvolcf))

# weighting by growing stock volume is the prefered method becuase there is significant variation
# at the spgrp level

# nr.acwt <- merge(dat,acres,by=c('fips','spgrpcd'))
# nr.acwt <- ddply(nr.acwt, .(fips), summarize, nr_base_acwt=weighted.mean(nr_base, w=acres),
#                  nr_cc_acwt=weighted.mean(nr_cc, w=acres)) 

# nr.acwt does not work because there is no varitation in acreage at the spgrpcd level
# so the result comes out the same as the non-weighted case

#nr <- ddply(dat, .(fips), summarize, nr_base=mean(nr_base), nr_cc=mean(nr_cc))

# calculate percent change in nr

#nr$chg <- (nr$nr_cc - nr$nr_base) / nr$nr_base
nr.volwt$chg <- (nr.volwt$nr_cc_volwt - nr.volwt$nr_base_volwt) / nr.volwt$nr_base_volwt
dbWriteTable(conn,'nr_forestland_v2',nr.volwt, overwrite=T)
#nr.acwt$chg <- (nr.acwt$nr_cc_acwt - nr.acwt$nr_base_acwt) / nr.acwt$nr_base_acwt
nr.volwt <- dbReadTable(conn, 'nr_forestland_v2')
# load map packages

library(ggplot2)
library(maptools)
library(ggmap)

#load u.s. county shapefile and conus outline

county.shp <- readShapePoly('C:/Users/mihiarc/Google Drive/GIS Files/boundary shapefiles/conus_county.shp')
counties <- fortify(county.shp, region='GEOID')
conus.shp <- readShapePoly('C:/Users/mihiarc/Google Drive/GIS Files/boundary shapefiles/conus_outline.shp')
conus <- fortify(conus.shp, region='Id')
rm(county.shp, conus.shp)

# join county layer with data and set order

map <- merge(counties, nr.volwt, by.x='id', by.y='fips', all.x=T, sort=F)
map <- map[order(map$order),]

##################################################################################

ggplot()+
  geom_polygon(data=map, aes(x=long, y=lat, group=group, map_id=id,
                             fill=chg))+
  geom_polygon(data=conus, aes(x=long, y=lat, map_id=id, group=group),
               fill=NA, color="grey", size=0.005)+
  scale_fill_gradient2()+
  ggtitle("Percent Change in NR \n(CanESM2 RCP 4.5)") +
  coord_map(project='polyconic')+
  theme(plot.margin=unit(c(0,0,0,0),"mm"))+
  theme_nothing(legend = T)
##################################################################################


acres <- ddply(acres, .(fips), summarize, acres=sum(acres))
nr <- merge(nr.volwt, acres, by='fips')
nr$val_base <- nr$nr_base_volwt * nr$acres
nr$val_cc <- nr$nr_cc_volwt * nr$acres

val_chgb <- weighted.mean(nr$chg, w=nr$acres)
val_chg_nowt <- mean(nr$chg)

val_base <- sum(nr$nr_base_volwt * nr$acres)
val_cc <- sum(nr$nr_cc_volwt * nr$acres)
val_chg <- (val_cc - val_base) / val_base

# by 2050 total timberland value increases by approximately 7.3% 
# relative to a baseline with no climate change

# plot value of timberland

# join county layer with data and set order

map <- merge(counties, nr, by.x='id', by.y='fips', all.x=T, sort=F)
map <- map[order(map$order),]


ggplot()+
  geom_polygon(data=map, aes(x=long, y=lat, group=group, map_id=id,
                             fill=val_base))+
  geom_polygon(data=conus, aes(x=long, y=lat, map_id=id, group=group),
               fill=NA, color="grey", size=0.005)+
  scale_fill_gradient2()+
  ggtitle("Value of Timberland \nUnder CanESM2 RCP 4.5") +
  coord_map(project='polyconic')+
  theme(plot.margin=unit(c(0,0,0,0),"mm"))+
  theme_nothing(legend = T)
  
# write output to main.db

dbWriteTable(conn, "nr_forestland_v2", nr)
