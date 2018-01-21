library(tidyverse)

source('scrape_page.R')

source('scrape_metrohist.R')

permits = scrape_metrohist(1900, 1942)

saveRDS(object = permits, file = 'data/permits_1900_1942.Rds')

permits_2 = scrape_metrohist(1943, 1986)

saveRDS(object = permits_2, file = 'data/permits_1943_1986.Rds')

# There are way fewer records from 1943-86 than expected. Check on that later.

permits_all = rbind(permits, permits_2)

saveRDS(object = permits_all, file = 'data/permits_1900_1986.Rds')

# Make csv for sharing.
write_csv(x = permits_all, path = 'data/permits_1900_1986.csv')
