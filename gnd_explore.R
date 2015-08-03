######
# some exploratory stuff for Grand Bay wq
# May 2015, M. Beck

# first event - heavy rain - April 2005
# second event - hurricane Isaac - August 2012
# for nuts: PO4H, NO23, NH4, CHLA
# timeframes: E1A Event 1 Acute, E1C Event 1 Chronic, NI Non-Input Years, E2A Event 2 Acute, E2C Event 2 Chronic

# load libraries
# install.packages(c('dplyr', 'tidyr', 'ggplot2', 'agricolae'))
library(dplyr)
library(tidyr)
library(ggplot2)
library(agricolae)

# import
raw_data <- read.csv('PO4modified2005-2014grabs.csv', stringsAsFactors = F)
timeframes <- c('E1A', 'E1C', 'NI', 'E2A', 'E2C')

# format the data with dplyr....
# select columns of interest
# format date columns, create year column, floor by det limit for PO4, NO23, NH4
# cleanup station code, set factor levels for TimeFrame for correct plotting order,
# remove CR station
# make the nutrient variables long format with tidyr
# combine station, nutrient column for later analysis
# combine duplicate days, create logvalue
nut_data <- select(raw_data, StationCode, Date, TimeFrame, PO4F, NH4F, NO23F, CHLA_N) %>% 
  mutate(
    Date = as.Date(Date, format= "%m/%d/%Y"),
    Year = as.numeric(format(Date, "%Y")), 
    PO4F = pmax(0.01, PO4F),
    NH4F = pmax(0.01, NH4F),
    NO23F = pmax(0.01, NO23F),
    StationCode = toupper(gsub('gnd|nut| *', '', StationCode)),
    TimeFrame = factor(TimeFrame, levels = timeframes, labels = timeframes)
  ) %>% 
  filter(StationCode != 'CR' & !is.na(TimeFrame)) %>% 
  gather('nutrient', 'value', PO4F:CHLA_N) %>% 
  unite(stat_nut, StationCode, nutrient, sep = ' ', remove = F) %>% 
  group_by(stat_nut, StationCode, TimeFrame, Year, Date, nutrient) %>% 
  summarize(value = mean(value, na.rm = TRUE)) %>% 
  mutate(logvalue = log(value, 10))

##
# boxplots
p1 <- ggplot(nut_data, aes(x = TimeFrame, y = value)) +
  geom_boxplot() + 
  facet_grid(nutrient ~ StationCode, scales = 'free_y') +
  scale_y_log10() +
  theme_bw()
p1

##
# line plots
p2 <- ggplot(nut_data, aes(x = Date, y = value)) +
  geom_line() + 
  facet_grid(nutrient ~ StationCode, scales = 'free_y') +
  scale_y_log10() + 
  theme_bw()
p2 

######
# some analyses

# split data by unique station/nutrient variable
sep_data <- split(nut_data, nut_data$stat_nut)

# run a Tukey multiple comparison for each station, nutrient variable combo
res <- lapply(sep_data, function(x){

  mod <- aov(logvalue ~ TimeFrame, data = x)
  tuk_mod <- HSD.test(mod, 'TimeFrame', group = T)
  grps <- tuk_mod$groups
  grps$trt <- gsub('[[:space:]].*$', '', grps$trt)
  grps
  
  })

# combine results for plotting
res <- do.call('rbind', res) %>% 
  mutate(
    stat_nut = gsub('\\.[0-9]', '', row.names(.)),
    TimeFrame = trt
  ) %>% 
  separate(stat_nut, c('StationCode', 'nutrient'), sep = ' ')
ylocs <- data.frame(nutrient = c('PO4F', 'NH4F', 'NO23F', 'CHLA_N'), ylims = c(10, 10, 1, 100))
res <- left_join(res, ylocs, by = 'nutrient')
res$TimeFrame <- factor(res$trt, levels = timeframes, labels = timeframes)

# the boxplots but with letters above each box indicating different groups
pdf('boxcomp.pdf', width = 10, height = 8, family = 'serif')
p1 + geom_text(data = res, aes(x = TimeFrame, y = ylims, label = M))
dev.off()

######
# survival regression for censored data by each timeframe
library(survival)

to_eval <- filter(nut_data, nutrient == 'PO4F' & StationCode == 'BN')

to_eval <- split(to_eval, to_eval$TimeFrame)

res <- lapply(to_eval, function(x){

  # setup detection limit column
  x <- na.omit(x)
  x$notcens <- TRUE
  x$notcens[x$logvalue <= -2] <- FALSE
  
  mod <- survreg(Surv(logvalue, notcens, type = "left") ~ Date,
    data = x, dist="gaus", control = list(iter.max = 1000)) 
  
  preddat <- seq.Date(min(x$Date), max(x$Date), by = 'day')
  preddat <- data.frame(Date = preddat)
  
  out <- predict(mod, newdata = preddat)
  out <- data.frame(Date = preddat, pred = out)
  
  out
  
  })
res <- do.call('rbind', res)
res <- left_join(do.call('rbind', to_eval), res, by = 'Date')

pdf('gnd_censreg.pdf', width = 8, height = 3, family = 'serif')
ggplot(res, aes(x = Date, y = logvalue, group = TimeFrame, colour = TimeFrame)) + 
  geom_point() + 
  geom_line(aes(y = pred)) + 
  theme_bw() + 
  scale_y_continuous(limits = c(-2, max(res$logvalue)))
dev.off()

##
# changepoint analysis
library(changepoint)

to_eval <- filter(nut_data, nutrient == 'PO4F' & StationCode == 'BN') %>% 
  select(Date, logvalue)

# fill missing values
interped <- approx(y = to_eval$logvalue, x = to_eval$Date, xout = to_eval$Date)
to_eval$interp <- interped$y 
res <- cpt.mean(to_eval$interp, Q = 5, method = 'BinSeg')
plot(res)
chngs <- to_eval$Date[cpts(res)]

pdf('gnd_chngpt.pdf', width = 8, height = 3, family = 'serif')
ggplot(to_eval, aes(x = Date, y = logvalue, group = TimeFrame, colour = TimeFrame)) + 
  geom_point() +
  geom_vline(xintercept = as.numeric(chngs)) + 
  theme_bw()
dev.off()

######
# changepoint analysis for ph, first event
library(changepoint)
load(file = 'wq_dat.RData')

# subset +- one month
subdts <- c('2005-04-01 0:0', '2005-05-01 0:0')
subdts <- as.POSIXct(subdts, format = '%Y-%m-%d %H:%M', tz = 'America/Regina')

to_eval <- filter(wq_dat, StationCode == 'BL') %>% 
  select(datetimestamp, ph) %>% 
  filter(datetimestamp >= subdts[1] & datetimestamp <= subdts[2])

# fill missing values
interped <- approx(y = to_eval$ph, x = to_eval$datetimestamp, xout = to_eval$datetimestamp)
to_eval$interp <- interped$y 
res <- cpt.mean(to_eval$interp, Q = 200, method = 'BinSeg')
plot(res)
chngs <- to_eval$datetimestamp[cpts(res)]

# pdf('gnd_chngpt.pdf', width = 8, height = 3, family = 'serif')
ggplot(to_eval, aes(x = datetimestamp, y = ph)) + 
  geom_line() +
  geom_vline(xintercept = as.numeric(chngs), colour = 'darkgreen') + 
  theme_bw()
# dev.off()