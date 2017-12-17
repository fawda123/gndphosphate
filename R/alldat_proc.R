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
library(lubridate)

# import
raw_data <- read.csv('ignore/PO4modified2005-Aug2015grabs.csv', stringsAsFactors = F)
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

# chl data up to May 2007 is bonk, just remove E1A and E1C
torm <- with(nut_dat, TimeFrame %in% c('E1A', 'E1C') & nutrient == 'CHLA_N')
nut_dat[torm, 'value'] <- NaN
nut_dat[torm, 'logvalue'] <- NaN

# save
save(nut_dat, file = 'data/nut_dat.RData')

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
#   dat <- import_local('C:/Users/mbeck/Desktop/972699.zip', stat)
# 
#   dat_in[[stat]] <- dat
# 
# }
# 
# # save raw, unprocessed data in R format
# gndwq <- dat_in
# save(gndwq, file = 'data/gndwq_unproc.RData')

data(gndwq_unproc)
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
      
      # remove bogus data (Kim's email 9/30)
      # for sal - 2015-07-09 15:00 and 2015-07-23 09:00
      # for ph - 2015-07-17 19:15 to 2015-07-23 09:00, 2015-07-09 15:00
      remsl <- as.POSIXct(c('2015-07-09 15:00', '2015-07-23 09:00'), tz = 'America/Regina')
      remph <- as.POSIXct(c('2015-07-17 19:15', '2015-07-23 09:00', '2015-07-09 15:00'), tz = 'America/Regina')
      out[with(out, datetimestamp %in% remsl), 'sal'] <- NA
      out[with(out, datetimestamp >= remph[1] & datetimestamp <= remph[2] | datetimestamp == remph[3]), 'ph'] <- NA
      
    }
      
#     out <- subset(out, subset = '2015-01-01 00:00', operator = '<') %>% 
#       select(datetimestamp, sal, ph, depth) 
    out <-  select(out, datetimestamp, do_pct, sal, ph, ph, depth, depth, turb, turb) 
    
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

save(wq_dat, file = 'data/wq_dat.RData')

######
# met proc

# # import from zipped folder
# met_dat <- import_local('C:/Users/mbeck/Desktop/956024.zip', 'gndcrmet')
#  
# # save raw, unprocessed data in R format
# gndmet <- met_dat
# save(gndmet, file = 'data/gndmet_unproc.RData')

data(gndmet_unproc)

met_dat <- qaqc(gndmet, qaqc_keep = c(0, 4, 5)) %>% 
      # subset(subset = '2015-01-01 00:00', operator = '<') %>% 
      select(datetimestamp, totprcp) 

brks <- c('07-01-2006 0:0', '03-01-2008 0:0', '09-01-2012 0:0', '02-01-2014 0:0', '12-01-2014 0:0') %>% 
  as.POSIXct(format = '%m-%d-%Y %H:%M', tz = 'America/Regina') %>% 
  c(-Inf, ., Inf)
labs <- c('E1A', 'E1C', 'NI1', 'E2A', 'E2C', 'NI2')
met_dat$TimeFrame <- cut(as.numeric(met_dat$datetimestamp), breaks = brks, labels = labs, 
  right = FALSE) # open on right

save(met_dat, file = 'data/met_dat.RData')

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

save(met_supp, file = 'data/met_supp.RData')

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

save(met_supp_e1, file = 'data/met_supp_e1.RData')

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

save(met_supp_e2, file = 'data/met_supp_e2.RData')

######
# ISCO data for fig 5

isco <- read.csv('ignore/BC_WQandNUT_2012.csv')
save(isco, file = 'data/isco.RData')

######
# multiple comparisons within sites (insites) and within time frames (intimes)

data(nut_dat)

# remove stat_nut, E1A/E1C time frames from chla
nut_dat <- nut_dat %>% 
  dplyr::select(-stat_nut) %>% 
  filter(!(nutrient == 'CHLA_N' & TimeFrame %in% c('E1A', 'E1C')))

# multiple comparisons of time frames within sites, all nutrients
insites <- nut_dat %>% 
  group_by(StationCode, nutrient) %>% 
  nest %>% 
  mutate(
    ests = map(data, function(x){
 
      tocmp <- x 
      
      # pairwise comparisons with mann-whitney (wilcox)
      grps <- unique(tocmp$TimeFrame)
      grps <- combn(grps, 2)
      pval <- rep(NA, ncol(grps))
      for(col in 1:ncol(grps)){
        grp <- tocmp$TimeFrame %in% grps[, col, drop = TRUE]
        res <- wilcox.test(value ~ TimeFrame, data = tocmp[grp, ], exact = FALSE, 
          alternative = 'two.sided')
        pval[col] <- res$p.value
      }
      
      # adjust p-values using holm sequential bonferroni 
      pval <- p.adjust(pval, method = 'holm')
      
      # pval as t/f using bonferroni correction
      vecs <- rep(FALSE, ncol(grps))
      vecs[pval < 0.05] <- TRUE
      names(vecs) <- paste(grps[1, ], grps[2, ], sep = '-')
      
      # group membership based on multiple comparisons
      lets <- multcompLetters(vecs)$Letters
      
      # standard summary stats
      sums <- group_by(x, TimeFrame) %>% 
        summarise(
          length = length(na.omit(value)),
          medval = median(value, na.rm = TRUE),
          minval = min(value, na.rm = TRUE),
          maxval = max(value, na.rm = TRUE)
          )
      
      data.frame(lets, sums, stringsAsFactors = FALSE)
      
    })
  ) %>% 
  dplyr::select(-data) %>% 
  unnest

# multiple comparisons of sites within time frames, all nutrients
intimes<- nut_dat %>% 
  group_by(TimeFrame, nutrient) %>% 
  nest %>% 
  mutate(
    ests = map(data, function(x){
 
      tocmp <- x 
      
      # pairwise comparisons with mann-whitney (wilcox)
      grps <- unique(tocmp$StationCode)
      grps <- combn(grps, 2)
      pval <- rep(NA, ncol(grps))
      for(col in 1:ncol(grps)){
        grp <- tocmp$StationCode %in% grps[, col, drop = TRUE]
        res <- wilcox.test(value ~ StationCode, data = tocmp[grp, ], exact = FALSE, 
          alternative = 'two.sided')
        pval[col] <- res$p.value
      }
      
      # adjust p-values using holm sequential bonferroni 
      pval <- p.adjust(pval, method = 'holm')
      
      # pval as t/f using bonferroni correction
      vecs <- rep(FALSE, ncol(grps))
      vecs[pval < 0.05] <- TRUE
      names(vecs) <- paste(grps[1, ], grps[2, ], sep = '-')
      
      # group membership based on multiple comparisons
      lets <- multcompLetters(vecs, reversed = T)$Letters
      
      # standard summary stats
      sums <- group_by(x, StationCode) %>% 
        summarise(
          length = length(na.omit(value)),
          medval = median(value, na.rm = TRUE),
          minval = min(value, na.rm = TRUE),
          maxval = max(value, na.rm = TRUE)
          )
      
      data.frame(lets, sums, stringsAsFactors = FALSE)
      
    })
  ) %>% 
  dplyr::select(-data) %>% 
  unnest

save(insites, file = 'data/insites.RData', compress = 'xz')
save(intimes, file = 'data/intimes.RData', compress = 'xz')
