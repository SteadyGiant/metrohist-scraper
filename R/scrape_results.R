# Early version of scraper that extracted all results for a given year.

library(magrittr)
library(rvest)
library(stringr)
library(tidyverse)

session = rvest::html_session(
  'http://www.metrohistory.com/dbpages/NBsearch.lasso'
)

form = rvest::html_form(session)[[1]] %>%
  rvest::set_values(year = 1900)

current_page = rvest::submit_form(session, form)

end_result = current_page %>%
  rvest::html_nodes(css = 'td div:nth-child(1) font:nth-child(1) b:nth-child(1)') %>%
  rvest::html_text() %>%
  as.numeric()

current_result = current_page %>%
  rvest::html_nodes(css = 'td div:nth-child(1) b:nth-child(3)') %>%
  rvest::html_text() %>%
  as.numeric()

costs_2 = list()

while (TRUE) {
  
  # Costs
  costs = current_page %>%
    rvest::html_nodes(css = 'td:nth-child(3) div font') %>%
    rvest::html_text() %>%
    as_tibble() %>%
    filter(value != 'COST')
  
  costs_2[length(costs_2) + 1] = costs
  
  current_page %<>%
    rvest::follow_link(i = 4, css = 'body > div:nth-child(1) > table:nth-child(4) > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(1) > div:nth-child(1) > a:nth-child(3) > img:nth-child(1)')
  
  current_result = current_page %>%
    rvest::html_nodes(css = 'td div:nth-child(1) b:nth-child(3)') %>%
    rvest::html_text() %>%
    as.numeric()
  
  if (current_result == end_result) {
    costs = current_page %>%
      rvest::html_nodes(css = 'td:nth-child(3) div font') %>%
      rvest::html_text() %>%
      as_tibble() %>%
      filter(value != 'COST')
    
    costs_2[length(costs_2) + 1] = costs
    
    break
  }
  
}

# costs = do.call(what = rbind, args = costs_2) %>%
#   as_tibble()

costs = unlist(costs_2) %>%
  as_tibble()