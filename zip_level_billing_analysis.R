require(dplyr)
require(lubridate)
require(stringr)
require(lme4)

# pathname with all source data on Sharefile
path = "S:/Shared Folders/CADEO/05_Data/CA PUC Energy Division/Billing" 

# aggregated billing data by zip code - stack into single data frame
zipcols <- c("zipcode","month","year","custclass","comb","ncust","kWh","avgkwh")
y15q1 <- read.csv(paste(path, "PGE_2015_Q1_ElectricUsageByZip.csv",sep="/"), col.names=zipcols)
y15q2 <- read.csv(paste(path, "PGE_2015_Q2_ElectricUsageByZip.csv",sep="/"), col.names=zipcols)
y15q3 <- read.csv(paste(path, "PGE_2015_Q3_ElectricUsageByZip.csv",sep="/"), col.names=zipcols)
y15q4 <- read.csv(paste(path, "PGE_2015_Q4_ElectricUsageByZip.csv",sep="/"), col.names=zipcols)
y16q1 <- read.csv(paste(path, "PGE_2016_Q1_ElectricUsageByZip.csv",sep="/"), col.names=zipcols)
y16q2 <- read.csv(paste(path, "PGE_2016_Q2_ElectricUsageByZip.csv",sep="/"), col.names=zipcols)
y16q3 <- read.csv(paste(path, "PGE_2016_Q3_ElectricUsageByZip.csv",sep="/"), col.names=zipcols)
y16q4 <- read.csv(paste(path, "PGE_2016_Q4_ElectricUsageByZip.csv",sep="/"), col.names=zipcols)
y17q1 <- read.csv(paste(path, "PGE_2017_Q1_ElectricUsageByZip.csv",sep="/"), col.names=zipcols)
y17q2 <- read.csv(paste(path, "PGE_2017_Q2_ElectricUsageByZip.csv",sep="/"), col.names=zipcols)
y17q3 <- read.csv(paste(path, "PGE_2017_Q3_ElectricUsageByZip.csv",sep="/"), col.names=zipcols)
y17q4 <- read.csv(paste(path, "PGE_2017_Q4_ElectricUsageByZip.csv",sep="/"), col.names=zipcols)
cons <- rbind(y15q1,y15q2,y15q3,y15q4,y16q1,y16q2,y16q3,y16q4,y17q1,y17q2,y17q3,y17q4)

# consumption values treated as factors by R and have thousands commas. Need to convert to numeric
cons$ncust <- as.numeric(gsub(",","",as.character(cons$ncust)))
cons$kWh <- as.numeric(gsub(",","",as.character(cons$kWh)))
cons$avgkwh <- as.numeric(gsub(",","",as.character(cons$avgkwh)))

# clear out temporary data frames
rm(y15q1,y15q2,y15q3,y15q4,y16q1,y16q2,y16q3,y16q4,y17q1,y17q2,y17q3,y17q4)


# climate zone by zip code
cz <- read.csv(paste(path,"BuildingClimateZonesByZIPCode.csv",sep="/"),col.names=c("zipcode","cz","junk"))
cz <- dplyr::select(cz,zipcode,cz)

# inner join climate zone reference table with billing data 
cz_cons <- dplyr::inner_join(cons, cz, by="zipcode") %>%
  dplyr::filter(., cz %in% c(1,2,3,4,5,11,12,13)) %>%
  dplyr::filter(., custclass %in% c("Elec- Residential")) %>%
  dplyr::filter(., ncust > 100) %>%
  dplyr::arrange(.,custclass, zipcode, year, month)


# read NOAA actual weather data, add columns from month and day 
w1 <- read.csv(paste(path,"1335874.csv",sep="/"))
w2 <- read.csv(paste(path,"1335875.csv",sep="/"))
w3 <- read.csv(paste(path,"1335876.csv",sep="/"))
weather <- rbind(w1,w2,w3)
weather$datetime <- as.POSIXct(weather$DATE, format="%Y-%m-%d %H:%M" )
weather$month <- month(weather$datetime)
weather$day <- day(weather$datetime)
weather$year <- year(weather$datetime)

# clear out temporary data frames
rm(w1,w2,w3)

# subset to the daily weather rows, map each station to a climate zone
weather_day <- weather %>%
  dplyr::select(.,STATION,STATION_NAME,LATITUDE,LONGITUDE,DATE,month,day,year,REPORTTPYE,DAILYHeatingDegreeDays,DAILYCoolingDegreeDays,DAILYAverageDryBulbTemp) %>%
  dplyr::filter(.,REPORTTPYE=="SOD") %>%
  dplyr::mutate(.,cz = case_when( 
    STATION_NAME =="EUREKA WEATHER FORECAST OFFICE WOODLEY ISLAND CA US" ~ 1,
    STATION_NAME =="NAPA CO AIRPORT CA US" ~ 2, 
    STATION_NAME =="SAN FRANCISCO INTERNATIONAL AIRPORT CA US" ~ 3,
    STATION_NAME =="SAN JOSE CA US" ~ 4,  
    STATION_NAME =="SANTA MARIA PUBLIC AIRPORT CA US" ~ 5,
    STATION_NAME =="RED BLUFF MUNICIPAL AIRPORT CA US" ~ 11,
    STATION_NAME =="STOCKTON METROPOLITAN AIRPORT CA US" ~ 12,
    STATION_NAME =="FRESNO YOSEMITE INTERNATIONAL CA US" ~ 13))


# need to get to monthly weather HDD, CDD
weather_month <- weather_day %>%
  dplyr::group_by(., cz, year, month) %>%
  dplyr::summarise(.,hdd = sum(DAILYHeatingDegreeDays, na.rm=TRUE), cdd = sum(DAILYCoolingDegreeDays, na.rm=TRUE))
weather_month$cz = as.integer(weather_month$cz)

# ACS data
acs <- read.csv(paste(path,"pacge_acs_data.csv",sep="/"),na.strings="#N/A") 
acs$medianIncomeHH <- as.numeric(gsub(",","",as.character(acs$medianIncomeHH)))
cz_cons_acs <- dplyr::inner_join(cz_cons, acs, by="zipcode") 
cz_cons_acs_wx <- dplyr::inner_join(cz_cons_acs, weather_month, by=c("cz","year","month"))


# run lm by zip code
fits <- lmList(avgkwh~hdd+cdd | zipcode, data=cz_cons_acs_wx)

# combine r-squared with coefficients from lm, save as a data frame
sumfun <- function(x) c(coef(x),summary(x)$r.squared)
a_kwh_coeff <- t(sapply(fits,sumfun)) 
kwh_coeff <- as.data.frame(a_kwh_coeff) %>%
  tibble::rownames_to_column(.,"zipcode")
kwh_coeff$zipcode <- as.numeric(kwh_coeff$zipcode)

# avg consumption - 2015-2017
avg_cons <- cz_cons %>% 
  dplyr::group_by(.,zipcode, cz) %>%
  dplyr::summarise(.,avg_cons=mean(kWh,na.rm=TRUE), avg_cust=mean(ncust,na.rm=TRUE)) %>%
  dplyr::mutate(avg_cust_cons = 12*avg_cons/avg_cust)

# final data - join ACS demographics w/ consumption model results
finalData <- dplyr::inner_join(acs, kwh_coeff, by="zipcode") %>%
  dplyr::inner_join(., avg_cons, by="zipcode") %>%
  dplyr::select(., - avg_cons) %>%
  dplyr::rename(., rsqd = V4, intcpt=`(Intercept)`)
write.table(finalData,file="S:/Shared Folders/CADEO/05_Data/CA PUC Energy Division/Billing/pge.csv",
            append=FALSE, 
            quote=FALSE, 
            row.names=FALSE, 
            col.names=c("zipcode","gini","medianIncomeHH","pctGasHeat","pctElecHeat","pctOwn","pctUnemp","reg_intercep","hdd_slope","cdd_slope","r_sqd","cz","num_cust","avg_upc"),
            sep=",")

#tmp <- cz_cons_acs_wx %>%
#  dplyr::filter(.,zipcode %in% c(93230, 95113))
#write.table(tmp,
#            file="S:/Personal Folders/good zip codes.csv",
#            append=FALSE, 
#            quote=FALSE, 
#            row.names=FALSE, 
#            sep=",")
#
#tmp2 <- cz_cons_acs_wx %>%
#  dplyr::filter(.,zipcode %in% c(93230))
#fits_tmp2 <- lm(avgkwh~hdd+cdd, data=tmp2)
