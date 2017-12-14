library(tidyverse)
library(lubridate)

data(nut_dat)

dat <- nut_dat %>% 
  group_by(stat_nut) %>% 
  nest %>% 
  mutate(
    sets = map(data, function(x){
      
      x <- x %>% 
        mutate(
          yr = year(date),
          qrt = quarter(date)
          ) %>%
        unite('yrqrt', yr, qrt, sep = '-') %>% 
        group_by(yrqrt, TimeFrame) %>% 
        summarise(
          logvalue = median(logvalue, na.rm = T)
        )
      
      mod <- lm(logvalue ~ TimeFrame, x)
      resid(mod)
 
    })
  )

pdf('autocor.pdf', family = 'serif', height = 12, width = 12)
par(mfrow = c(4, 4))
for(i in 1:nrow(dat)){
  
  mn <- dat$stat_nut[i]
  acf(na.omit(dat$sets[[i]]), main = mn)

}
dev.off()
