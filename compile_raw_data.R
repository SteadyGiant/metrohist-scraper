library(tidyverse)

source('scrape_page.R')

source('scrape_metrohist.R')

permits = scrape_metrohist(1900, 1942)

saveRDS(object = permits, file = 'data/permits_1900_1942.Rds')

permits_2 = scrape_metrohist(1943, 1986)

saveRDS(object = permits_2, file = 'data/permits_1943_1986.Rds')
