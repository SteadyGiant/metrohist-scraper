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

current_page %<>%
  rvest::follow_link(i = 2, css = 'body > div:nth-child(1) > table:nth-child(4) > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(1) > div:nth-child(1) > a:nth-child(3) > img:nth-child(1)')

# Costs
costs = current_page %>%
  rvest::html_nodes(css = 'td:nth-child(3) div font') %>%
  rvest::html_text() %>%
  as_tibble() %>%
  filter(value != 'COST')
