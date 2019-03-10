library("tidyr")
library("dplyr")
library("httr")
library("jsonlite")
library("dplyr")
library("markdown")
library("knitr")

x <- read.csv("populationdata.csv.csv" , stringsAsFactors = FALSE)
y <- read.csv("gdpdata.csv" , stringsAsFactors = FALSE)


#### dplyr work ###########

#c <- unname(unlist(x[1,]))
#colnames(x) <-  c
#colnames(x)[colnames(x) == ''] <- "Region"
#population_with_regions <- x %>% filter(Year == "2015" | Year == "2010")%>% select(Region, Year, Series, Value) %>% spread(Series, Value) 

#c2 <- unname(unlist(y[1,]))
#colnames(y) <-  c2
#colnames(y)[colnames(y) == ''] <- "Region"
#gdp_data_with_regions <- y %>%  filter(Year == "2010" | Year == "2015") %>% select(Region, Year, Series, Value) %>% spread(Series, Value) 


#all_data_with_regions <- full_join(population_with_regions , gdp_data_with_regions, by = NULL, type = "full" , match = "all")

#all_countries <- "Total, all countries or areas"

#main_regions <- c( "Africa","Asia","Europe", "Latin America & the Caribbean","Northern America", "Oceania")

#africa_subregions <- c("Northern Africa", "Sub-Saharan Africa", "Eastern Africa", "Middle Africa", "Southern Africa", "Western Africa")

#latin america_and_the_caribbean_subregions <- c("Caribbean", "Central America", "South America")

#asia_subregions <- c("Central Asia", "Eastern Asia", "South-central Asia", "South-eastern Asia", "Southern Asia", "Western Asia")

#europe_subreagions <- c("Eastern Europe", "Northern Europe", "Southern Europe", "Western Europe")

#oceania_subregions <- c("Australia and New Zealand", "Melanesia", "Micronesia", "Polynesia")


####trying to ling region and subregion########

countries <- c("United States", "Canada", "Afghanistan", "Albania", 
               "Algeria", "American Samoa", "Andorra", "Angola", "Anguilla", 
               "Antarctica", "Antigua and/or Barbuda", "Argentina", "Armenia", "Aruba", 
               "Australia", "Austria", "Azerbaijan", "Bahamas", "Bahrain", "Bangladesh", 
               "Barbados", "Belarus", "Belgium", "Belize", "Benin", "Bermuda", "Bhutan", 
               "Bolivia", "Bosnia and Herzegovina", "Botswana", "Bouvet Island", "Brazil", 
               "British lndian Ocean Territory", "Brunei Darussalam", "Bulgaria", "Burkina Faso", 
               "Burundi", "Cambodia", "Cameroon", "Cape Verde", "Cayman Islands", "Central African Republic", 
               "Chad", "Chile", "China", "Christmas Island", "Cocos (Keeling) Islands", "Colombia", "Comoros", 
               "Congo", "Cook Islands", "Costa Rica", "Croatia (Hrvatska)", "Cuba", "Cyprus", "Czech Republic", 
               "Denmark", "Djibouti", "Dominica", "Dominican Republic", "East Timor", "Ecudaor", "Egypt", "El Salvador",
               "Equatorial Guinea", "Eritrea", "Estonia", "Ethiopia", "Falkland Islands (Malvinas)", 
               "Faroe Islands", "Fiji", "Finland", "France", "France, Metropolitan", "French Guiana", 
               "French Polynesia", "French Southern Territories", "Gabon", "Gambia", "Georgia", "Germany", 
               "Ghana", "Gibraltar", "Greece", "Greenland", "Grenada", "Guadeloupe", "Guam", "Guatemala", 
               "Guinea", "Guinea-Bissau", "Guyana", "Haiti", "Heard and Mc Donald Islands", "Honduras", "Hong Kong", 
               "Hungary", "Iceland", "India", "Indonesia", "Iran (Islamic Republic of)", "Iraq", "Ireland", "Israel", 
               "Italy", "Ivory Coast", "Jamaica", "Japan", "Jordan", "Kazakhstan", "Kenya", "Kiribati", 
               "Korea, Democratic People's Republic of", "Korea, Republic of", "Kuwait", "Kyrgyzstan", 
               "Lao People's Democratic Republic", "Latvia", "Lebanon", "Lesotho", "Liberia", "Libyan Arab Jamahiriya",
               "Liechtenstein", "Lithuania", "Luxembourg", "Macau", "Macedonia", "Madagascar", "Malawi", "Malaysia", 
               "Maldives", "Mali", "Malta", "Marshall Islands", "Martinique", "Mauritania", "Mauritius", "Mayotte", 
               "Mexico", "Micronesia, Federated States of", "Moldova, Republic of", "Monaco", "Mongolia", "Montserrat", 
               "Morocco", "Mozambique", "Myanmar", "Namibia", "Nauru", "Nepal", "Netherlands", "Netherlands Antilles", 
               "New Caledonia", "New Zealand", "Nicaragua", "Niger", "Nigeria", "Niue", "Norfork Island", 
               "Northern Mariana Islands", "Norway", "Oman", "Pakistan", "Palau", "Panama", "Papua New Guinea",
               "Paraguay", "Peru", "Philippines", "Pitcairn", "Poland", "Portugal", "Puerto Rico", "Qatar", "Reunion", 
               "Romania", "Russian Federation", "Rwanda", "Saint Kitts and Nevis", "Saint Lucia", 
               "Saint Vincent and the Grenadines", "Samoa", "San Marino", "Sao Tome and Principe", "Saudi Arabia", 
               "Senegal", "Seychelles", "Sierra Leone", "Singapore", "Slovakia", "Slovenia", "Solomon Islands", 
               "Somalia", "South Africa", "South Georgia South Sandwich Islands", "Spain", "Sri Lanka", "St. Helena",
               "St. Pierre and Miquelon", "Sudan", "Suriname", "Svalbarn and Jan Mayen Islands", "Swaziland", "Sweden", 
               "Switzerland", "Syrian Arab Republic", "Taiwan", "Tajikistan", "Tanzania, United Republic of", "Thailand",
               "Togo", "Tokelau", "Tonga", "Trinidad and Tobago", "Tunisia", "Turkey", "Turkmenistan", 
               "Turks and Caicos Islands", "Tuvalu", "Uganda", "Ukraine", "United Arab Emirates", "United Kingdom", 
               "United States minor outlying islands", "Uruguay", "Uzbekistan", "Vanuatu", "Vatican City State",
               "Venezuela", "Vietnam", "Virigan Islands (British)", "Virgin Islands (U.S.)", "Wallis and Futuna Islands", 
               "Western Sahara", "Yemen", "Yugoslavia", "Zaire", "Zambia", "Zimbabwe") ;
c <- unname(unlist(x[1,]))
colnames(x) <-  c
colnames(x)[colnames(x) == ''] <- "Region"
population_data_by_country <- x %>% subset(Region %in% countries) %>% filter(Year == "2015" | Year == "2010")%>% select(Region,`Region/Country/Area`,Year, Series, Value) %>% spread(Series, Value)


c2 <- unname(unlist(y[1,]))
colnames(y) <-  c2
colnames(y)[colnames(y) == ''] <- "Region"
gdp_data_by_country <- y %>% subset(Region %in% countries) %>% filter(Year == "2010" | Year == "2015") %>% select(Region,`Region/Country/Area`,Year, Series, Value) %>% spread(Series, Value)

## joins data tables by country.
all_data <- full_join(population_data_by_country , gdp_data_by_country, by = NULL, type = "full" , match = "all")
#cleaner_all_data <- as.numeric(all_data$`Region/Country/Area`)
all_data$`Region/Country/Area` <- as.numeric(all_data$`Region/Country/Area`)

all_data$`GDP in constant 2010 prices (millions of US dollars)` <- as.numeric(gsub(",","",all_data$`GDP in constant 2010 prices (millions of US dollars)`))
all_data$`GDP in current prices (millions of US dollars)` <- as.numeric(gsub(",","",all_data$`GDP in current prices (millions of US dollars)`))
all_data$`GDP per capita (US dollars)` <- as.numeric(gsub(",","",all_data$`GDP per capita (US dollars)`))

all_data$`Infant mortality for both sexes (per 1,000 live births)` <- as.numeric(all_data$`Infant mortality for both sexes (per 1,000 live births)`)
all_data$`Life expectancy at birth for both sexes (years)` <- as.numeric(all_data$`Life expectancy at birth for both sexes (years)`)
all_data$`Life expectancy at birth for females (years)` <- as.numeric(all_data$`Life expectancy at birth for females (years)`)
all_data$`Life expectancy at birth for males (years)` <- as.numeric(all_data$`Life expectancy at birth for males (years)`)
all_data$`Maternal mortality ratio (deaths per 100,000 population)` <- as.numeric(all_data$`Maternal mortality ratio (deaths per 100,000 population)`)
all_data$`Population annual rate of increase (percent)` <- as.numeric(all_data$`Population annual rate of increase (percent)`)
all_data$`Total fertility rate (children per women)` <- as.numeric(all_data$`Total fertility rate (children per women)`)
all_data$`GDP real rates of growth (percent)` <- as.numeric(all_data$`GDP real rates of growth (percent)`)


region_and_subregions <- read.csv("data/all.csv") %>% select(name, country.code, region, sub.region)

all_data_with_regions <- left_join(all_data,region_and_subregions, by = c("Region/Country/Area" = "country.code"))

print(all_data)

write.csv(all_data_with_regions, "all_data_with_regions.csv")