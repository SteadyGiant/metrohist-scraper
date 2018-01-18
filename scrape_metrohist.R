# Scrapes the Building Permits Database (1900-86) of the Office for Metropolitan
# History: http://www.metrohistory.com/searchfront.htm
# Searches for every year available & grabs all entries.

scrape_metrohist = function(first_year, last_year) {
  
  for (j in first_year:last_year) {
    
    library(magrittr)
    library(rvest)
    library(stringr)
    library(tidyverse)
    
    session = rvest::html_session(
      'http://www.metrohistory.com/dbpages/NBsearch.lasso'
    ) 
    
    form = rvest::html_form(session)[[1]] %>%
      rvest::set_values(year = 1905)
    
    current_page = rvest::submit_form(session, form)
    
    
    ##############
    ### Scrape ###
    ##############
    
    # Costs
    costs = current_page %>%
      rvest::html_nodes(css = 'td:nth-child(3) div font') %>%
      rvest::html_text() %>%
      as_tibble() %>%
      filter(value != 'COST')
    
    # Building Addresses
    bldg_adds = current_page %>%
      rvest::html_nodes(css = 'td:nth-child(4)') %>%
      rvest::html_text() %>%
      as_tibble()
    
    # Some of the blank cells are just extra crap we don't need, but some are 
    # actual building addresses left blank. We want to keep those. They always 
    # appear right under an instance of 'BUILDING ADDRESS'.
    for (i in 1:nrow(bldg_adds)) {
      if (
        i > 1 & 
        bldg_adds[i, 1] == '' & 
        grepl(pattern = 'BUILDING ADDRESS', x = bldg_adds[(i-1), 1])
      ) {
        bldg_adds[i, 1] = 'blank'
      } 
    }
    
    bldg_adds %<>%
      filter(
        value != '',
        !grepl(pattern = 'BUILDING ADDRESS', x = value)
      )
    
    # DOB NB#'s
    nums = current_page %>%
      rvest::html_nodes(css = 'td:nth-child(2)') %>%
      rvest::html_text() %>%
      as_tibble() %>%
      mutate(value = stringr::str_trim(value))
    
    for (i in 1:nrow(nums)) {
      if (
        i > 1 & 
        grepl(pattern = 'DOB NB', x = nums[(i-1), 1]) &
        nums[i, 1] == ''
      ) {
        nums[i, 1] = 'blank'
      }
    }
    
    # Remove all entries which aren't numeric or 'blank'
    nums %<>%
      mutate(
        value = ifelse(
          grepl(pattern = '^[\\(o\\)|\\(a\\)|DOB NB]', x = value), 
          value, 
          value %>% as.numeric() %>% as.character
        )
      ) %>%
      filter(
        !is.na(value),
        !grepl(pattern = '\\(o\\)|\\(a\\)|DOB NB', x = value)
      )
    
    # Owners / Owner Addresses
    # Architects / Architect Addresses
    # These can only be scraped together, due to the site layout.
    owns_archs = current_page %>%
      rvest::html_nodes(css = 'td:nth-child(2)') %>%
      rvest::html_text() %>%
      as_tibble()
    
    owns_archs %<>%
      mutate(value = stringr::str_trim(value)) %>%
      filter(
        value != '',
        is.na(as.numeric(value)),
        !grepl(pattern = 'DOB NB|<\\?\\?>', x = value)
      ) 
    
    owners = list()
    
    architects = list()
    
    for (i in 1:nrow(owns_archs)) {
      if (i %% 2 == 0) {
        architects[i] = owns_archs[i, 1]
      } else {
        owners[i] = owns_archs[i, 1]
      }
    }
    
    owners = do.call(rbind, owners) %>%
      as_tibble()
    
    architects = do.call(rbind, architects) %>%
      as_tibble()
    
    # Descriptions
    # Comments
    desc_comm = current_page %>%
      rvest::html_nodes(css = 'td:nth-child(3) , td:nth-child(5)') %>%
      rvest::html_text() %>%
      as_tibble() %>%
      mutate(value = stringr::str_trim(value)) %>%
      filter(
        !grepl(pattern = 'COST|\\$', x = value)
      )
    
    for (i in 1:nrow(desc_comm)) {
      if (
        i > 1 & 
        stringr::str_trim(desc_comm[i, 1]) == '' & 
        grepl(pattern = 'DESCRIPTION|COMMENTS', x = desc_comm[(i-1), 1])
      ) {
        desc_comm[i, 1] = 'blank'
      } 
    }
    
    desc_comm %<>%
      mutate(value = stringr::str_trim(value)) %>%
      filter(
        value != '',
        !grepl(pattern = 'DESCRIPTION|COMMENTS', x = value)
      )
    
    descripts = list()
    
    comments = list()
    
    for (i in 1:nrow(desc_comm)) {
      if (i %% 2 == 0) {
        comments[i] = desc_comm[i, 1] # Every even row (2+).
      } else {
        descripts[i] = desc_comm[i, 1] # Every odd row (1+).
      }
    }
    
    descripts = do.call(rbind, descripts) %>%
      as_tibble()
    
    comments = do.call(rbind, comments) %>%
      as_tibble()
    
    
    ###############
    ### Combine ###
    ###############
    
    combined = cbind(
      nums, bldg_adds, owners, architects, costs, descripts, comments
    ) %>%
      set_colnames(
        c('DOB NB#', 'building_address', 'owner_address', 
          'architect_address', 'cost', 'description', 'comments')
      ) %>%
      mutate(year = j)
    
    if (exists('metrohist_scraped')) {
      metrohist_scraped[[length(metrohist_scraped)+1]] = combined
    } else {
      metrohist_scraped = list()
      metrohist_scraped[[1]] = combined
    }
    
  }
  
  metrohist_scraped = do.call(rbind, metrohist_scraped)
  
  return(metrohist_scraped)
  
}


# https://stat4701.github.io/edav/2015/04/02/rvest_tutorial/