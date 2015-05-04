# first event - heavy rain - April 2005
# second event - hurricane Isaac - August 2012
# for nuts: PO4H, NO23, NH4, CHLA

# load libraries to use, mostly for cleanup
library(dplyr)
library(tidyr)
library(ggplot2)
library(agricolae)

nut_data <- read.csv('PO4modified2005-2013grabs.csv', stringsAsFactors = F)

# format the data with dplyr
# select columns of interest then...
# format date columns, create year column, floor by det limit for PO4, NO23, NH4, log-transform
# cleanup station code, set factor levels for Epoch for correct plotting order,
# remove CR station
# make the nutrient variables long formaty with tidyr
nut_data <- select(nut_data, StationCode, Date, Epoch, PO4F, NH4F, NO23F, CHLA_N) %>% 
  mutate(
    Date = as.Date(Date, format= "%m/%d/%Y"),
    Year = as.numeric(format(Date, "%Y")), 
    PO4F = pmax(0.01, PO4F),
    NH4F = pmax(0.01, NH4F),
    NO23F = pmax(0.01, NO23F),
    StationCode = toupper(gsub('gnd|nut| *', '', StationCode)),
    Epoch = factor(Epoch, levels = c('', 'ME1', 'PE1', 'QY', 'ME2', 'PE2'))
  ) %>% 
  filter(StationCode != 'CR') %>% 
  gather('nutrient', 'value', PO4F:CHLA_N) %>% 
  mutate(logvalue = log(value, exp(10))) %>% 
  unite(stat_nut, StationCode, nutrient, sep = ' ', remove = F)

# combine the empty variable with ME1
levels(nut_data$Epoch) <- c('ME1', 'ME1', 'PE1', 'QY', 'ME2', 'PE2')

# boxplots
p1 <- ggplot(nut_data, aes(x = Epoch, y = value)) +
  geom_boxplot() + 
  facet_grid(nutrient ~ StationCode, scales = 'free_y') +
  scale_y_log10() +
  theme_bw()
p1

# line plots
p2 <- ggplot(nut_data, aes(x = Date, y = value)) +
  geom_line() + 
  facet_grid(nutrient ~ StationCode, scales = 'free_y') +
  scale_y_log10() + 
  theme_bw()
p2 

##
# barplots with 95 CI
# have to create a new dataset
ci_fun <- function(x, alpha = 0.05){
  me <- sd(x, na.rm = T)/sqrt(length(x))
  quant <- qt(1 - alpha/2, length(x) - 1)
  me * quant
}
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
sep_data <- split(nut_data, nut_data$stat_nut)

res <- lapply(sep_data, function(x){

  mod <- aov(logvalue ~ Epoch, data = x)
  tuk_mod <- HSD.test(mod, 'Epoch', group = T)
  grps <- tuk_mod$groups
  levels(grps$trt) <- c('ME1', 'PE1', 'QY', 'ME2', 'PE2')
  grps[order(grps$trt), ]
  
  })

res <- do.call('rbind', res) %>% 
  mutate(
    stat_nut = gsub('\\.[0-9]', '', row.names(.)),
    Epoch = trt
  ) %>% 
  separate(stat_nut, c('StationCode', 'nutrient'), sep = ' ')
ylocs <- data.frame(nutrient = c('PO4F', 'NH4F', 'NO23F', 'CHLA_N'), ylims = c(10, 10, 1, 100))
res <- left_join(res, ylocs, by = 'nutrient')

p1 + geom_text(data = res, aes(x = Epoch, y = ylims, label = M))

