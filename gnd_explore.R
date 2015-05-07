######
# some exploratory stuff for Grand Bay wq
# May 2015, M. Beck

# first event - heavy rain - April 2005
# second event - hurricane Isaac - August 2012
# for nuts: PO4H, NO23, NH4, CHLA

# load libraries
# install.packages(c('dplyr', 'tidyr', 'ggplot2', 'agricolae'))
library(dplyr)
library(tidyr)
library(ggplot2)
library(agricolae)

# import
nut_data <- read.csv('PO4modified2005-2013grabs.csv', stringsAsFactors = F)
epochs <- c('E1A', 'E1C', 'NIY', 'E2A')

# format the data with dplyr....
# select columns of interest
# format date columns, create year column, floor by det limit for PO4, NO23, NH4
# cleanup station code, set factor levels for Epoch for correct plotting order,
# remove CR station
# make the nutrient variables long format with tidyr
# log transform variables
# combine station, nutrient column for later analysis
nut_data <- select(nut_data, StationCode, Date, Epoch, PO4F, NH4F, NO23F, CHLA_N) %>% 
  mutate(
    Date = as.Date(Date, format= "%m/%d/%Y"),
    Year = as.numeric(format(Date, "%Y")), 
    PO4F = pmax(0.01, PO4F),
    NH4F = pmax(0.01, NH4F),
    NO23F = pmax(0.01, NO23F),
    StationCode = toupper(gsub('gnd|nut| *', '', StationCode)),
    Epoch = factor(Epoch, levels = epochs, labels = epochs)
  ) %>% 
  filter(StationCode != 'CR') %>% 
  gather('nutrient', 'value', PO4F:CHLA_N) %>% 
  mutate(logvalue = log(value, exp(10))) %>% 
  unite(stat_nut, StationCode, nutrient, sep = ' ', remove = F)

##
# boxplots
p1 <- ggplot(nut_data, aes(x = Epoch, y = value)) +
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

##
# barplots with 95 CI, these are ugly and not very informative
# have to create a new dataset

# confidence interval funciton
ci_fun <- function(x, alpha = 0.05){
  me <- sd(x, na.rm = T)/sqrt(length(x))
  quant <- qt(1 - alpha/2, length(x) - 1)
  me * quant
}

# create dataset with dplyr
bar_data <- group_by(nut_data, StationCode, Epoch, nutrient) %>% 
  summarize(
    meanvalue = mean(value, na.rm = T),
    meanlogvalue = mean(logvalue, na.rm = T),
    uppervalue = ci_fun(value) + meanvalue,
    lowervalue = ci_fun(value) - meanvalue,
    upperlogvalue = ci_fun(logvalue) + meanlogvalue,
    lowerlogvalue = ci_fun(logvalue) - meanlogvalue
)

p3 <- ggplot(bar_data, aes(x = Epoch, y = meanvalue)) + 
  geom_bar(stat = 'identity') +
  geom_errorbar(stat = 'identity', aes(ymin = lowervalue, ymax = uppervalue), width = 0) +
  facet_grid(nutrient ~ StationCode, scales = 'free_y') +
  theme_bw()
p3

######
# some analyses

# split data by unique station/nutrient variable
sep_data <- split(nut_data, nut_data$stat_nut)

# run a Tukey multiple comparison for each station, nutrient variable combo
res <- lapply(sep_data, function(x){

  mod <- aov(logvalue ~ Epoch, data = x)
  tuk_mod <- HSD.test(mod, 'Epoch', group = T)
  grps <- tuk_mod$groups
  grps
  
  })

# combine results for plotting
res <- do.call('rbind', res) %>% 
  mutate(
    stat_nut = gsub('\\.[0-9]', '', row.names(.)),
    Epoch = trt
  ) %>% 
  separate(stat_nut, c('StationCode', 'nutrient'), sep = ' ')
ylocs <- data.frame(nutrient = c('PO4F', 'NH4F', 'NO23F', 'CHLA_N'), ylims = c(10, 10, 1, 100))
res <- left_join(res, ylocs, by = 'nutrient')

# the boxplots but with letters above each box indicating different groups
pdf('boxcomp.pdf', width = 10, height = 8, family = 'serif')
p1 + geom_text(data = res, aes(x = Epoch, y = ylims, label = M))
dev.off()

#####

# some nonsense
tmp <- filter(nut_data, StationCode == 'BN' & nutrient == 'PO4F')
tmp$lags <- c(NA, diff(tmp$logvalue))
mod <- lm(logvalue ~ Date + lags, data = tmp)
