library(tidyverse)
library(lubridate)
library(SWMPr)

data(nut_dat)

timeframes <- c('E1A', 'E1C', 'NI1', 'E2A', 'E2C', 'NI2')
dts <- c('7/1/2006', '2/1/2008', '9/1/2012', '1/1/2014', '12/1/2014') %>% 
  mdy

dat <- nut_dat %>% 
  group_by(stat_nut) %>% 
  nest %>% 
  mutate(
    sets = map(data, function(x){
      
      x <- data.frame(x, stringsAsFactors = F)
      
      dc <- decomp_cj(x, param = 'value', date_col = 'date', vals_out = T) %>%
        mutate(
          TimeFrame = cut(
            as.numeric(Time),
            breaks = c(-Inf, as.numeric(dts), Inf),
            labels = timeframes,
            right = F
            ),
          vals = original - seasonal
        )
      
      
      mod <- lm(vals ~ Time, dc)
      resid(mod)
      # durbinWatsonTest(mod, max.lag = 1)
 
    })
  )
# dat$sets

pdf('autocor.pdf', family = 'serif', height = 12, width = 12)
par(mfrow = c(4, 4))
for(i in 1:nrow(dat)){

  mn <- dat$stat_nut[i]
  acf(na.omit(dat$sets[[i]]), main = mn)

}
dev.off()
