scrape_page = function(page) {
  
  # Initialize current page tracker.
  current_page = page
  
  
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
    filter(
      value != '',
      !grepl(pattern = '\\(o\\)|\\(a\\)|DOB NB|\\/', x = value)
    )
  
  # Owners / Owner Addresses
  # Architects / Architect Addresses
  # These can only be scraped together, due to the site layout.
  owns_archs = current_page %>%
    rvest::html_nodes(css = 'td:nth-child(2)') %>%
    rvest::html_text() %>%
    as_tibble()
  
  for (i in 2:nrow(owns_archs)) {
    if (
      grepl(pattern = 'DOB NB', x = owns_archs[(i-1), 1])
    ) {
      owns_archs[i, 1] = ''
    }
  }
  
  owns_archs %<>%
    mutate(value = stringr::str_trim(value)) %>%
    filter(grepl(pattern = '\\(o\\)|\\(a\\)|\\/', x = value))
  
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
      !grepl(pattern = 'COST', x = value),
      !(grepl(pattern = '\\$', x = value) & !grepl(pattern = '[:alpha:]', x = value))
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
    )
  
  return(combined)
  
}
