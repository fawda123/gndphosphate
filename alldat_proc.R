######
# nutrient data processing
# first event - heavy rain - April 2005
# second event - hurricane Isaac - August 2012
# for nuts: PO4H, NO23, NH4, CHLA
# timeframes: E1A Event 1 Acute, E1C Event 1 Chronic, NI Non-Input Years, 
#   E2A Event 2 Acute, E2C Event 2 Chronic

# load libraries
# install.packages(c('dplyr', 'tidyr', 'ggplot2', 'agricolae'))
library(dplyr)
library(tidyr)
library(ggplot2)
library(agricolae)
library(SWMPr)
library(reshape2)

# import
raw_data <- read.csv('PO4modified2005-Aug2015grabs.csv', stringsAsFactors = F)
timeframes <- c('E1A', 'E1C', 'NI1', 'E2A', 'E2C', 'NI2')

# format the data
nut_dat <- select(raw_data, StationCode, Date, TimeFrame, PO4F, NH4F, NO23F, CHLA_N) %>% 
  mutate(
    Date = as.Date(Date, format= "%m/%d/%Y"),
    PO4F = pmax(0.01, PO4F),
    NH4F = pmax(0.01, NH4F),
    NO23F = pmax(0.01, NO23F),
    StationCode = toupper(gsub('gnd|nut| *', '', StationCode)),
    TimeFrame = factor(TimeFrame, levels = timeframes, labels = timeframes)
  ) %>% 
  rename(date = Date) %>% 
  filter(!StationCode %in% c('CR', 'BH') & !is.na(TimeFrame)) %>% 
  group_by(StationCode, TimeFrame, date) %>% 
  summarise_each(funs(mean(., na.rm = TRUE))) %>% 
  gather('nutrient', 'value', PO4F:CHLA_N) %>% 
  unite(stat_nut, StationCode, nutrient, sep = ' ', remove = F) %>% 
  mutate(logvalue = log(value, 10)) %>% 
  data.frame(.)

save(nut_dat, file = 'nut_dat.RData')

######
# wq data processing
# gndwq is an r object that was created by importing local data from zips
# see gndwa_explore

# stats <- c('bl', 'bc', 'bh', 'pc')
# stats <- paste0('gnd', stats, 'wq')
# 
# dat_in <- vector('list', length(stats))
# names(dat_in) <- stats
# 
# # import data
# for(stat in stats){
# 
#   dat <- import_local('C:/Users/mbeck/Desktop/939702.zip', stat)
#   
#   dat_in[[stat]] <- dat
#   
# }
# 
# # save raw, unprocessed data in R format
# gndwq <- dat_in
# save(gndwq, file = 'gndwq_unpro.RData')

load('gndwq_unproc.RData')
dat_in <- gndwq

# process data
wq_dat <- lapply(dat_in, 
  function(x) {
    
    out <- qaqc(x, qaqc_keep = c(0, 4, 5))  
    
    # add in all ph for period in second ts plot if station is bl
    if(attr(x, 'station') == 'gndblwq'){
      
      keepph <- subset(x, select = 'ph') 
      subs <- c('2012-08-20 0:0', '2012-10-01 0:0')
      subs <- as.POSIXct(subs, format = '%Y-%m-%d', tz = 'America/Regina')
      subs <- out$datetimestamp >= subs[1] & out$datetimestamp <= subs[2]
      out[subs, 'ph'] <- keepph[subs, 'ph']
      
    }
      
#     out <- subset(out, subset = '2015-01-01 00:00', operator = '<') %>% 
#       select(datetimestamp, sal, ph, depth) 
    out <-  select(out, datetimestamp, sal, ph, depth) 
    
    return(out)
    
  })

# combine stations, format for plotting
wq_dat <- melt(wq_dat, id.vars = names(wq_dat[[1]])) %>% 
  mutate(StationCode = toupper(gsub('^gnd|wq$', '', L1)))
wq_dat$L1 <- NULL
brks <- c('07-01-2006 0:0', '03-01-2008 0:0', '09-01-2012 0:0', '02-01-2014 0:0', '12-01-2014 0:0') %>% 
  as.POSIXct(format = '%m-%d-%Y %H:%M', tz = 'America/Regina') %>% 
  c(-Inf, ., Inf)
labs <- c('E1A', 'E1C', 'NI1', 'E2A', 'E2C', 'NI2')
wq_dat$TimeFrame <- cut(as.numeric(wq_dat$datetimestamp), breaks = brks, labels = labs, 
  right = FALSE) # open on right

save(wq_dat, file = 'wq_dat.RData')

######
# met proc

# # import from zipped folder
# met_dat <- import_local('C:/Users/mbeck/Desktop/956024.zip', 'gndcrmet')
#  
# # save raw, unprocessed data in R format
# gndmet <- met_dat
# save(gndmet, file = 'gndmet_unproc.RData')

load('gndmet_unproc.RData')

met_dat <- qaqc(gndmet, qaqc_keep = c(0, 4, 5)) %>% 
      # subset(subset = '2015-01-01 00:00', operator = '<') %>% 
      select(datetimestamp, totprcp) 

brks <- c('07-01-2006 0:0', '03-01-2008 0:0', '09-01-2012 0:0', '02-01-2014 0:0', '12-01-2014 0:0') %>% 
  as.POSIXct(format = '%m-%d-%Y %H:%M', tz = 'America/Regina') %>% 
  c(-Inf, ., Inf)
labs <- c('E1A', 'E1C', 'NI1', 'E2A', 'E2C', 'NI2')
met_dat$TimeFrame <- cut(as.numeric(met_dat$datetimestamp), breaks = brks, labels = labs, 
  right = FALSE) # open on right

save(met_dat, file = 'met_dat.RData')

##
# some minor processing of supplementary precip data from KPQL
# fill hole in 2005-2006 from Katrina for first time series plot
# data from wunderground
met_supp <- read.table('ignore/KPQL_wxsupp.txt', header = T, sep = ',')
met_supp <- select(met_supp, CDT, PrecipitationIn) %>% 
  mutate(
    CDT = as.Date(CDT, format = '%Y-%m-%d'), 
    PrecipitationIn = PrecipitationIn * 25.4
  ) %>% 
  rename(
    date = CDT,
    totprcp = PrecipitationIn
  )
brks <- c('07-01-2006 0:0', '03-01-2008 0:0', '09-01-2012 0:0', '02-01-2014 0:0', '12-01-2014 0:0') %>% 
  as.POSIXct(format = '%m-%d-%Y %H:%M', tz = 'America/Regina') %>% 
  c(-Inf, ., Inf)
labs <- c('E1A', 'E1C', 'NI1', 'E2A', 'E2C', 'NI2')
met_supp$TimeFrame <- cut(as.numeric(met_supp$date), breaks = brks, labels = labs, 
  right = FALSE) # open on right
met_supp <- met_supp[, c(1, 3, 2)]

save(met_supp, file = 'met_supp.RData')

##
# some minor processing of supplementary precip data from KPQL
# fill hole in precip for first event for second time series plot
# data from wunderground
met_supp_e1 <- read.table('ignore/KPQL_event1.txt', header = T, sep = ',')
met_supp_e1 <- select(met_supp_e1, CDT, PrecipitationIn) %>% 
  mutate(
    CDT = as.POSIXct(CDT, format = '%Y-%m-%d', tz = 'America/Regina'), 
    PrecipitationIn = PrecipitationIn * 2.54 # in to cm
  ) %>% 
  rename(
    datetimestamp = CDT,
    totprcp = PrecipitationIn
  )
brks <- c('07-01-2006 0:0', '03-01-2008 0:0', '09-01-2012 0:0', '02-01-2014 0:0', '12-01-2014 0:0') %>% 
  as.POSIXct(format = '%m-%d-%Y %H:%M', tz = 'America/Regina') %>% 
  c(-Inf, ., Inf)
labs <- c('E1A', 'E1C', 'NI1', 'E2A', 'E2C', 'NI2')
met_supp_e1$TimeFrame <- cut(as.numeric(met_supp_e1$datetimestamp), breaks = brks, labels = labs, 
  right = FALSE) # open on right
met_supp_e1 <- met_supp_e1[, c(1, 3, 2)]

save(met_supp_e1, file = 'met_supp_e1.RData')

##
# some minor processing of supplementary precip data from KPQL
# fill hole in precip for second event for second time series plot
# data from wunderground
met_supp_e2 <- read.table('ignore/KPQL_event2.txt', header = T, sep = ',')
met_supp_e2 <- select(met_supp_e2, CDT, PrecipitationIn) %>% 
  mutate(
    CDT = as.POSIXct(CDT, format = '%Y-%m-%d', tz = 'America/Regina'), 
    PrecipitationIn = PrecipitationIn * 2.54 # in to cm
  ) %>% 
  rename(
    datetimestamp = CDT,
    totprcp = PrecipitationIn
  )
brks <- c('07-01-2006 0:0', '03-01-2008 0:0', '09-01-2012 0:0', '02-01-2014 0:0', '12-01-2014 0:0') %>% 
  as.POSIXct(format = '%m-%d-%Y %H:%M', tz = 'America/Regina') %>% 
  c(-Inf, ., Inf)
labs <- c('E1A', 'E1C', 'NI1', 'E2A', 'E2C', 'NI2')
met_supp_e2$TimeFrame <- cut(as.numeric(met_supp_e2$datetimestamp), breaks = brks, labels = labs, 
  right = FALSE) # open on right
met_supp_e2 <- met_supp_e2[, c(1, 3, 2)]

save(met_supp_e2, file = 'met_supp_e2.RData')

