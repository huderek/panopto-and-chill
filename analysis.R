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

new_frame <- select(countries, name, alpha.3)
new_frame <- mutate(new_frame, iso_test = iso.alpha(name, 3))

countries <- countries$country.code
  c <- unname(unlist(x[1,]))
colnames(x) <-  c
colnames(x)[colnames(x) == ''] <- "Region"
population_data_by_country <- x %>% subset(`Region/Country/Area` %in% countries) %>% filter(Year == "2015" | Year == "2010")%>% select(Region, Year, Series, Value) %>% spread(Series, Value)


c2 <- unname(unlist(y[1,]))
colnames(y) <-  c2
colnames(y)[colnames(y) == ''] <- "Region"

gdp_data_by_country <- y %>% subset(`Region/Country/Area` %in% countries) %>% filter(Year == "2010" | Year == "2015") %>% select(Region, Year, Series, Value) %>% spread(Series, Value)

# fix incorrect naming conventions
population_data_by_country[59:60,"Region"] <- "Virgin Islands"
population_data_by_country[69:70,"Region"] <- "Cape Verde"
population_data_by_country[95:96,"Region"] <- "Republic of Congo"
population_data_by_country[101:102,"Region"] <- "Ivory Coast"
population_data_by_country[107:108,"Region"] <- "Curacao"
population_data_by_country[111:112,"Region"] <- "Czech Republic"
population_data_by_country[113:114,"Region"] <- "North Korea"
population_data_by_country[115:116,"Region"] <- "Democratic Republic of the Congo"
population_data_by_country[185:186,"Region"] <- "Vatican"
population_data_by_country[225:226, "Region"] <- "Laos"
population_data_by_country[333:334, "Region"] <- "South Korea"
population_data_by_country[335:336, "Region"] <- "Moldova"
population_data_by_country[337:338, "Region"] <- "Reunion"
population_data_by_country[391:392, "Region"] <- "Palestine"
population_data_by_country[407:408, "Region"] <- "Macedonia"
population_data_by_country[437:438, "Region"] <- "UK"
population_data_by_country[439:440, "Region"] <- "Tanzania"
population_data_by_country[441:442, "Region"] <- "USA"
population_data_by_country[453:454, "Region"] <- "Vietnam"

# make pop data long
gather_pop <- population_data_by_country %>% gather("trend" ,"value" , 
                                                    `Infant mortality for both sexes (per 1,000 live births)`: `Total fertility rate (children per women)`) %>%
  spread("Year" , "value") 

colnames(gather_pop) <- c("region", "trend", "yr2010" , "yr2015")
gather_pop <- gather_pop %>% mutate(change = as.numeric(yr2015 , na.rm = T) - as.numeric(yr2010, na.rm = T))

## joins data tables by country.
combined_df <- full_join(population_data_by_country , gdp_data_by_country, by = NULL, type = "full" , match = "all")

# fix incorrect naming conventions
combined_df[59:60,"Region"] <- "Virgin Islands"
combined_df[69:70,"Region"] <- "Cape Verde"
combined_df[95:96,"Region"] <- "Republic of Congo"
combined_df[101:102, "Region"] <- "Ivory Coast"
combined_df[107:108, "Region"] <- "Curacao"
combined_df[111:112, "Region"] <- "Czech Republic"
combined_df[113:114, "Region"] <- "North Korea"
combined_df[115:116, "Region"] <- "Democratic Republic of the Congo"
combined_df[185:186, "Region"] <- "Vatican"
combined_df[225:226, "Region"] <- "Laos"
combined_df[333:334, "Region"] <- "South Korea"
combined_df[335:336, "Region"] <- "Moldova"
combined_df[337:338, "Region"] <- "Reunion"
combined_df[391:392, "Region"] <- "Palestine"
combined_df[407:408, "Region"] <- "Macedonia"
combined_df[437:438, "Region"] <- "UK"
combined_df[439:440, "Region"] <- "Tanzania"
combined_df[441:442, "Region"] <- "USA"
combined_df[453:454, "Region"] <- "Vietnam"

iso_countries <- unique(gather_pop['region'])
  iso_countries <- iso_countries  %>% mutate(codes = iso.alpha(region,3))
  
write.csv(gather_pop,"gather_pop.csv")

#################################plotting####################################################


bin_values_color <- function(a){
  if(a == "GDP_millions_of_USD"){
    bin_values <-  c(0, 995, 3900, 12055, Inf)
  }
  else{
    bin_values <- c(0, 32, 5000, 40000, Inf )
  }
}

