# select BL, BC, PC, timeframes, summarize by day to reduce
wq_proc <- filter(wq_dat, StationCode %in% c('BL', 'PC', 'BC') & TimeFrame %in% c('E1A', 'NI', 'E2A')) %>% 
  mutate(datetimestamp = as.Date(datetimestamp)) %>% 
  rename(date = datetimestamp) %>% 
  group_by(date, StationCode, TimeFrame) %>% 
  summarise_each(funs(mean(., na.rm = TRUE))) %>% 
  rename(Station = StationCode) %>% 
  droplevels(.)

ggplot(wq_proc, aes(x = sal, y = ph, group = Station, colour = Station)) + 
  geom_point(alpha = 0.4) + 
  facet_wrap(~ TimeFrame) +
  stat_smooth(method = 'lm') + 
  theme_bw()

# mod 1

# find leading/trailing NA, remove, then fill
library(nlme)

pdf('C:/Users/mbeck/Desktop/ph_mods.pdf', height = 6, width = 6, family = 'serif')
for(tf in unique(wq_proc$TimeFrame)){

  tomod <- wq_proc[wq_proc$TimeFrame == tf, ]

  # find leading/trailing NA, remove, then fill
  notna <- which(!is.na(tomod$ph))
  notna <- c(notna[1], notna[length(notna)])
  tomod <- tomod[notna[1]:notna[2], ]
  notna <- which(!is.na(tomod$sal))
  notna <- c(notna[1], notna[length(notna)])
  tomod <- tomod[notna[1]:notna[2], ]
  
  tomod$ph <-zoo::na.approx(tomod$ph, maxgap = 1e6)
  tomod$sal <- zoo::na.approx(tomod$sal, maxgap = 1e6)
  
  mod <- lm(ph ~ sal * Station + date, data = tomod)

  salrng <- range(tomod$sal, na.rm = T)
  phrng <- range(tomod$ph, na.rm = T)
  plot(salrng, phrng, type="n",xlab="sal", ylab="pH")
  uni_stat <- unique(tomod$Station)
  for(i in seq_along(uni_stat)){
    
    salrngi <- range(tomod$sal[tomod$Station == uni_stat[i]], na.rm = T)
    newdata <- data.frame(
      sal = seq(salrngi[1], salrngi[2], length = 30), 
      date = mean(tomod$date),
      Station = uni_stat[i]
      )
    lines(newdata$sal, predict(mod, newdata), lty = i)
  
  }

  legend("topleft", inset = .01, lty = 1:3,legend=uni_stat)
  title(tf)
  
}
dev.off()