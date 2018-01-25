# Calls scrape_page()

scrape_metrohist = function(first_year, last_year) {
  
  ##############
  ### Prelim ###
  ##############
  
  library(magrittr)
  library(rvest)
  library(tidyverse)
  
  # Loop through years.
  for (j in first_year:last_year) {
    
    # Navigate to homepage.
    session = rvest::html_session(
      'http://www.metrohistory.com/dbpages/NBsearch.lasso'
    ) 
    
    # Navigate to page 1 of search results.
    form = rvest::html_form(session)[[1]] %>%
      rvest::set_values(year = j)
    
    # Initialize current page tracker.
    current_page = rvest::submit_form(session, form)
    
    # Get # of results.
    end_result = current_page %>%
      rvest::html_nodes(css = 'td div:nth-child(1) font:nth-child(1) b:nth-child(1)') %>%
      rvest::html_text() %>%
      as.numeric()
    
    # Initialize current result # tracker.
    current_result = current_page %>%
      rvest::html_nodes(css = 'td div:nth-child(1) b:nth-child(3)') %>%
      rvest::html_text() %>%
      as.numeric()
    
    # Initialize list to hold tibbles of scraped data from each result page.
    results_list = list()
    
    
    ##############
    ### Scrape ###
    ##############
    
    # Loop through the result pages.
    while (TRUE) {
      
      # 1. Scrape data on current page and add to list.
      results_list[[length(results_list) + 1]] = scrape_page(current_page) %>%
        mutate(year = j)
      
      # 2. Navigate to next page.
      current_page %<>%
        rvest::follow_link(i = 4, css = 'body > div:nth-child(1) > table:nth-child(4) > tbody:nth-child(1) > tr:nth-child(3) > td:nth-child(1) > div:nth-child(1) > a:nth-child(3) > img:nth-child(1)')
      
      # 3. Store which result # we're currently at.
      current_result = current_page %>%
        rvest::html_nodes(css = 'td div:nth-child(1) b:nth-child(3)') %>%
        rvest::html_text() %>%
        as.numeric()
      
      # An infinite loop w/ a conditional break was added bcs the very last page
      # of results were being ignored otherwise.
      if (current_result == end_result) {
        
        # Scrape last page of results.
        results_list[[length(results_list) + 1]] = scrape_page(current_page) %>%
          mutate(year = j)
        
        # End loop.
        break
        
      }
      
    }
    
    # We have all the results for one year.
    # We want to add it to a list of results for each year,
    # one that doesn't get overwritten in each for-loop iteration.
    if (exists('years_list')) {
      years_list[[length(years_list) + 1]] = do.call(
        what = rbind, 
        args = results_list
      )
    } else {
      years_list = list()
      years_list[[1]] = do.call(
        what = rbind, 
        args = results_list
      )
    }
    
  }
  
  # We have all results for all years.
  # We want to combine these tibbles into one big dataset.
  master = do.call(what = rbind, args = years_list)
  
  return(master)
  
}