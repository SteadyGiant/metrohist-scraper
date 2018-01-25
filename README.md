# metrohist-scraper

## Scraping The Building Permits Database, 1900-1986

I used these scripts to extract all data on all new building applications filed 
in Manhattan from 1900-1986. The [Office for Metropolitan History](http://www.metrohistory.com/) 
maintains this database. 

The data is available in the `data` folder, in .csv and .Rds formats. Consult 
the [database site](http://www.metrohistory.com/searchfront.htm) for issues with 
the data.

See `R/compile_raw_data.R` for the code that generated this data. It calls the
`scrape_metrohist()` function.