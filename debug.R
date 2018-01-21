library(magrittr)
library(rvest)
library(tidyverse)


########################

permits_all %>% 
  group_by(year) %>% 
  summarize(n()) %>% 
  View()



########################

current_page = rvest::html_session(
  'http://www.metrohistory.com/dbpages/NBresults.lasso?-MaxRecords=10&-SkipRecords=180&-token.Action=Search%20Records&-Op=bw&year=1947'
) 

source('scrape_page.R')

test1 = scrape_page(current_page)

source('scrape_metrohist.R')

test2 = scrape_metrohist(1900, 1900)



########################

current_page = rvest::html_session(
  'http://www.metrohistory.com/dbpages/NBresults.lasso?-MaxRecords=10&-SkipRecords=490&-token.Action=Search%20Records&-Op=bw&year=1900'
) 

# DOB NB#'s
nums = current_page %>%
  rvest::html_nodes(css = 'td:nth-child(2)') %>%
  rvest::html_text() %>%
  as_tibble() %>%
  mutate(value = stringr::str_trim(value))

for (i in 2:nrow(nums)) {
  if (
    grepl(pattern = 'DOB NB', x = nums[(i-1), 1]) &
    nums[i, 1] == ''
  ) {
    nums[i, 1] = 'blank'
  }
}

# Remove all entries which aren't numeric or 'blank'
nums %<>%
  filter(
    value != '',
    !grepl(
      pattern = '\\(o\\)|\\(a\\)|\\(o & a\\)|DOB NB|\\/^[:digit:]|\\/$|\\ / |^\\/', # 
      x = value)
  )
