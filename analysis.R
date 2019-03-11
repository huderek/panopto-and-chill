library("tidyr")
library("dplyr")
library("httr")
library("jsonlite")
library("dplyr")
library("markdown")
library("knitr")

#x <- read.csv("populationdata.csv" , stringsAsFactors = FALSE)
y <- read.csv("gdpdata.csv" , stringsAsFactors = FALSE)
options(scipen = 999)

x <- read.csv("populationdata.csv.csv" , stringsAsFactors = FALSE)
#y <- read.csv("gdpdata.csv" , stringsAsFactors = FALSE)

#####################data cleaning#########################################
countries <- read.csv("all.csv", stringsAsFactors = FALSE) 

countries <- countries$country.code
c <- unname(unlist(x[1,]))
colnames(x) <-  c
colnames(x)[colnames(x) == ''] <- "Region"
population_data_by_country <- x %>% subset(`Region/Country/Area` %in% countries) %>% filter(Year == "2015" | Year == "2010")%>% select(Region, Year, Series, Value) %>% spread(Series, Value)


c2 <- unname(unlist(y[1,]))
colnames(y) <-  c2
colnames(y)[colnames(y) == ''] <- "Region"
gdp_data_by_country <- y %>% subset(`Region/Country/Area` %in% countries) %>% filter(Year == "2010" | Year == "2015") %>% select(Region, Year, Series, Value) %>% spread(Series, Value)


gather_pop <- population_data_by_country %>% gather("trend" ,"value" , 
                                                    `Infant mortality for both sexes (per 1,000 live births)`: `Total fertility rate (children per women)`) %>%
  spread("Year" , "value") 

colnames(gather_pop) <- c("region", "trend", "yr2010" , "yr2015")
gather_pop <- gather_pop %>% mutate(change = as.numeric(yr2015 , na.rm = T) - as.numeric(yr2010, na.rm = T))
## joins data tables by country.
all_data <- full_join(population_data_by_country , gdp_data_by_country, by = NULL, type = "full" , match = "all")

iso_countries <- unique(gather_pop['region'])
  
  iso_countries <- iso_countries  %>% mutate(codes = iso.alpha(region,3))

write.csv(gather_pop,"gather_pop.csv")

#################################plotting####################################################



