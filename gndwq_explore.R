library(SWMPr)
library(dplyr)
library(reshape)
library(tidyr)
library(ggplot2)

stats <- c('bl', 'bc', 'bh', 'pc')
stats <- paste0('gnd', stats, 'wq')

dat_in <- vector('list', length(stats))
names(dat_in) <- stats

# import data
for(stat in stats){

  dat <- import_local('C:/Users/mbeck/Desktop/939702.zip', stat)
  
  dat_in[[stat]] <- dat
  
}

# save raw, unprocessed data in R format
gndwq <- dat_in
save(gndwq, file = 'gndwq.RData')

# process data
dat <- lapply(dat_in, 
  function(x) {
    
    x <- qaqc(x, qaqc_keep = c(0, 4, 5)) %>% 
      # subset(subset = '2015-01-01 00:00', operator = '<') %>% 
      aggreswmp(., by = 'days', params = c('sal', 'ph', 'do_mgl')) 
    
    return(x)
    
  })

# combine stations, format for plotting
dat <- melt(dat, id.vars = names(dat[[1]])) %>% 
  mutate(stat = gsub('^gnd|wq$', '', L1))
dat$L1 <- NULL
dat <- gather(dat, 'variable', 'value', sal:ph)

events <- c('2005-04-01', '2012-08-01')
events <- as.Date(events)

pdf('gndwq.pdf', width = 12, height = 6, family = 'serif')
ggplot(dat, aes(x = datetimestamp, group = stat, colour = stat, y = value)) + 
  geom_line() +
  geom_vline(xintercept = as.numeric(events)) +
  facet_wrap(~ variable, ncol = 1, scales = 'free_y') + 
  theme_bw()
dev.off()