# Compairing DEQ and PurpleAir PM2.5 data for the year 2020
# Required packages
library(openair)
library(lubridate)
library(ggplot2)
library(dplyr)
library(ggpubr)

# Read in DEQ daily avg pm2.5 for 2020 and format date
DEQ <- read.csv("DEQ_msci.csv", header=T)
DEQ$date <-as.POSIXct(DEQ$date,format="%m/%d/%y",tz=Sys.timezone())

# Read in full sensor data for MSiC
Full_msci <- read.csv("PA_MSiC.csv", header=T)

# Convert date and create year column
Full_msci$date <- as.POSIXct(Full_msci$date,format="%Y-%m-%d %H:%M",tz=Sys.timezone())
Full_msci$year <- year(Full_msci$date)

# drop extra rows
Full_msci <- subset(Full_msci, select = -c(X, time_stamp, sensor_index, rssi, 
                                          uptime, pa_latency,memory,pressure))
# Create 3 data frames, atm, and cf
atm_msci <- subset(Full_msci, select = -c(pm2.5_cf_1_a, pm2.5_cf_1_b, pm2.5_alt_a, pm2.5_alt_b))
cf_msci <- subset(Full_msci, select = -c(pm2.5_atm_a, pm2.5_atm_b, pm2.5_alt_a, pm2.5_alt_b))
alt_msci <- subset(Full_msci, select = -c(pm2.5_cf_1_a, pm2.5_cf_1_b,pm2.5_atm_a, pm2.5_atm_b))

# Create 2 columns, one for difference and one for % error
#atm
atm_msci$diff <- atm_msci$pm2.5_atm_a - atm_msci$pm2.5_atm_b
atm_msci$diff <- abs(atm_msci$diff)
atm_msci$perc <- atm_msci$diff * 2 / (atm_msci$pm2.5_atm_a + atm_msci$pm2.5_atm_b)*100
atm_msci$perc <- abs(atm_msci$perc)

# SD = 23.04, 2SD = 46.08
sd(atm_msci$perc, na.rm = T)

# cf_1
cf_msci$diff <- cf_msci$pm2.5_cf_1_a - cf_msci$pm2.5_cf_1_b
cf_msci$diff <- abs(cf_msci$diff)
cf_msci$perc <- cf_msci$diff * 2 / (cf_msci$pm2.5_cf_1_a + cf_msci$pm2.5_cf_1_b)*100
cf_msci$perc <- abs(cf_msci$perc)

# SD = 23.5, 2SD = 47
sd(cf_msci$perc, na.rm = T)

#alt
alt_msci$diff <- alt_msci$pm2.5_alt_a - alt_msci$pm2.5_alt_b
alt_msci$diff <- abs(alt_msci$diff)
alt_msci$perc <- alt_msci$diff * 2 / (alt_msci$pm2.5_alt_a + alt_msci$pm2.5_alt_b)*100
alt_msci$perc <- abs(alt_msci$perc)

# SD = 17.91, 2SD = 35.82
sd(alt_msci$perc, na.rm = T)

# Subset by year (2020 for DEQ comparison)
atm_msci_20 <-atm_msci[atm_msci$year=="2020",]
cf_msci_20 <-cf_msci[cf_msci$year=="2020",]
alt_msci_20 <-alt_msci[alt_msci$year=="2020",]

# Cleaning data, QA
clean_atm_msci_20 <-subset(atm_msci_20, diff < 5 | perc < 46 )
clean_cf_msci_20 <-subset(cf_msci_20, diff < 5 | perc < 47)
clean_alt_msci_20 <-subset(alt_msci_20, diff < 5 | perc < 36)

# Relative humidity correction - cf only
clean_cf_msci_20$pm2.5_RH_a <- (0.524 *clean_cf_msci_20$pm2.5_cf_1_a - 0.0862 * clean_cf_msci_20$humidity + 5.75 )
clean_cf_msci_20$pm2.5_RH_b <- (0.524 *clean_cf_msci_20$pm2.5_cf_1_b - 0.0862 * clean_cf_msci_20$humidity + 5.75)

# Average two channels 
clean_atm_msci_20$avg_atm <- (clean_atm_msci_20$pm2.5_atm_a + clean_atm_msci_20$pm2.5_atm_b)/2
clean_cf_msci_20$avg_cf <- (clean_cf_msci_20$pm2.5_cf_1_a + clean_cf_msci_20$pm2.5_cf_1_b)/2
clean_cf_msci_20$avg_cf_RH <- (clean_cf_msci_20$pm2.5_RH_a + clean_cf_msci_20$pm2.5_RH_b)/2
clean_alt_msci_20$avg_alt <- (clean_alt_msci_20$pm2.5_alt_a + clean_alt_msci_20$pm2.5_alt_b)/2

# select only avg and date 
atm_avg <-subset(clean_atm_msci_20, select = c(date, avg_atm))
cf_avg<-subset(clean_cf_msci_20, select = c(date, avg_cf))
cf_RH_avg <- subset(clean_cf_msci_20, select = c(date, avg_cf_RH))
alt_avg <- subset(clean_alt_msci_20, select = c(date, avg_alt))
  
# calculate daily avg - clean
atm_daily <- timeAverage(atm_avg, avg.time = "day")
cf_daily <- timeAverage(cf_avg, avg.time = "day")
cf_RH_daily <- timeAverage(cf_RH_avg, avg.time = "day")
alt_daily <- timeAverage(alt_avg, avg.time = "day")
DEQ_daily <-timeAverage(DEQ, avg.time = "day")
  
# drop HMS - clean
atm_daily$date <-strftime(atm_daily$date, format="%Y-%m-%d")
atm_daily$date <-as.POSIXct(atm_daily$date,format="%Y-%m-%d",tz=Sys.timezone())

cf_daily$date <-strftime(cf_daily$date, format="%Y-%m-%d")
cf_daily$date <-as.POSIXct(cf_daily$date,format="%Y-%m-%d",tz=Sys.timezone())

cf_RH_daily$date <-strftime(cf_RH_daily$date, format="%Y-%m-%d")
cf_RH_daily$date <-as.POSIXct(cf_RH_daily$date,format="%Y-%m-%d",tz=Sys.timezone())
cf_RH_daily <-subset(cf_RH_daily, avg_cf_RH > 0)

alt_daily$date <-strftime(alt_daily$date, format="%Y-%m-%d")
alt_daily$date <-as.POSIXct(alt_daily$date,format="%Y-%m-%d",tz=Sys.timezone())

DEQ_daily$date <-strftime(DEQ_daily$date, format="%Y-%m-%d")
DEQ_daily$date <-as.POSIXct(DEQ_daily$date,format="%Y-%m-%d",tz=Sys.timezone())

# merge with DEQ - clean
DEQ_PA_3 <-left_join(DEQ_daily,atm_daily , 
                      by=c("date"))

DEQ_PA_3 <-left_join(DEQ_PA_3,cf_daily , 
                     by=c("date"))

DEQ_PA_3 <-left_join(DEQ_PA_3,cf_RH_daily , 
                     by=c("date"))

DEQ_PA_3 <-left_join(DEQ_PA_3,alt_daily , 
                     by=c("date"))

# Plots for QA data vs DEQ
DEQ_atm_qa <- ggplot(DEQ_PA_3,aes( x = avg_atm, y= pm2.5))+
  geom_point()+
  geom_abline (slope=1, linetype = "dashed", color="Red")+
  theme_bw() +
  labs (y = "DEQ PM2.5", x = " PA ATM PM2.5")+
  geom_smooth(method = "lm")+
  stat_regline_equation(label.x = 0, label.y = 17, size = 2.5)

DEQ_cf_qa <- ggplot(DEQ_PA_3,aes( x = avg_cf, y= pm2.5))+
  geom_point()+
  geom_abline (slope=1, linetype = "dashed", color="Red")+
  theme_bw()+
  labs (y = "DEQ PM2.5", x = " PA CF1 PM2.5")+
  geom_smooth(method = "lm")+
  stat_regline_equation(label.x = 0, label.y = 17, size = 2.5)

DEQ_alt_qa <- ggplot(DEQ_PA_3,aes( x = avg_alt, y= pm2.5))+
  geom_point()+
  geom_abline (slope=1, linetype = "dashed", color="Red")+
  theme_bw()+
  labs (y = "DEQ PM2.5", x = " PA ALT PM2.5")+
  geom_smooth(method = "lm")+
  stat_regline_equation(label.x = 0, label.y = 17, size = 2.5)

# Convert negative value to 0, RH correction
DEQ_PA_3[285,5]<-0

# RH correction
DEQ_cf_RH <- ggplot(DEQ_PA_3,aes( x = avg_cf_RH, y= pm2.5))+
  geom_point()+
  geom_abline (slope=1, linetype = "dashed", color="Red")+
  theme_bw()+
  labs (y = "DEQ PM2.5", x = " PA CF1 RH PM2.5")+
  geom_smooth(method = "lm")+
  stat_regline_equation(label.x = 0, label.y = 17, size = 2.5)

# atm vs. DEQ - almost always overestimating 
ggplot() + 
  geom_line(data=DEQ_PA_3, aes(x=date, y=pm2.5), color='black', size=1) + 
  geom_line(data=DEQ_PA_3, aes(x=date, y=avg_atm), color='red', size=1) +
  theme_bw()

# Arranging plots
PAvsDEQ <- (DEQ_atm_qa | DEQ_cf_qa | DEQ_alt_qa |DEQ_cf_RH ) 
PAvsDEQ <- PAvsDEQ + plot_annotation(tag_levels = 'A')

# Save figures
ggsave("MathSci DEQ and PA.png", plot = PAvsDEQ, device = "png", path = NULL, 
       scale = 1, width = 8.6, height = 4.7, units = "in", dpi = 800)

ggsave("MathSci DEQ and PA_RH.png", plot = DEQ_cf_RH, device = "png", path = NULL, 
       scale = 1, width = 5.88, height = 3.93, units = "in", dpi = 800)
