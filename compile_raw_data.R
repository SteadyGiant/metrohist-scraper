library(tidyverse)

source('scrape_page.R')

source('scrape_metrohist.R')

permits = scrape_metrohist(1900, 1986)
