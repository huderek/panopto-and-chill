library("tidyr")
library("dplyr")
library("httr")
library("jsonlite")
library("dplyr")
library("markdown")
library("knitr")

#x <- read.csv("populationdata.csv" , stringsAsFactors = FALSE)
y <- read.csv("gdpdata.csv" , stringsAsFactors = FALSE)


x <- read.csv("populationdata.csv.csv" , stringsAsFactors = FALSE)
#y <- read.csv("gdpdata.csv" , stringsAsFactors = FALSE)

#####################data cleaning#########################################

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
population_data_by_country <- x %>% subset(Region %in% countries) %>% filter(Year == "2015" | Year == "2010")%>% select(Region, Year, Series, Value) %>% spread(Series, Value)


c2 <- unname(unlist(y[1,]))
colnames(y) <-  c2
colnames(y)[colnames(y) == ''] <- "Region"
gdp_data_by_country <- y %>% subset(Region %in% countries) %>% filter(Year == "2010" | Year == "2015") %>% select(Region, Year, Series, Value) %>% spread(Series, Value)


gather_pop <- population_data_by_country %>% gather("trend" ,"value" , 
                                                    `Infant mortality for both sexes (per 1,000 live births)`: `Total fertility rate (children per women)`) %>%
  spread("Year" , "value") 

colnames(gather_pop) <- c("region", "trend", "yr2010" , "yr2015")
gather_pop <- gather_pop %>% mutate(change = as.numeric(yr2015 , na.rm = T) - as.numeric(yr2010, na.rm = T))
## joins data tables by country.
all_data <- full_join(population_data_by_country , gdp_data_by_country, by = NULL, type = "full" , match = "all")

#################################plotting####################################################

world_map <- map_data("world") %>%
  mutate(Country.Code = iso.alpha(region , 3))
##filtering based on user input 
data_new <-  gather_pop %>% 
  mutate(Country.Code = iso.alpha(region , 3)) %>%
  filter(trend == "Infant mortality for both sexes (per 1,000 live births)") #%>% 
  #filter(Series.Name == input$type) %>%
  #spread(year, value)
#adding change column
#forest_data_new <-  forest_data_new %>%
  #mutate(change = forest_data_new[, 5] - forest_data_new[, 4])

world_pop_map <- left_join(world_map, data_new, by = "Country.Code") 
##finding the 5 bins based on quantiles 
bin_values <- quantile(world_pop_map$change , probs = c(0, 0.2, 0.4, 0.6, 0.8, 1) , na.rm = T)
bin_values_rounded <-  round(bin_values)
world_pop_map <- world_pop_map %>% 
  mutate(`Percentage change` = cut(change, breaks=bin_values, labels=c(paste(bin_values_rounded[1],"to",bin_values_rounded[2]), 
                                                                       paste(bin_values_rounded[1],"to",bin_values_rounded[2]), 
                                                                       paste(bin_values_rounded[2],"to",bin_values_rounded[3]), 
                                                                       paste(bin_values_rounded[3],"to",bin_values_rounded[4]), 
                                                                       paste(bin_values_rounded[4],"to",bin_values_rounded[5]))))


ggplot(data = world_pop_map) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = `Percentage change`)) +
  scale_fill_brewer(palette = "RdYlGn") +
  #labs(title = paste("Change in" , input$type , "between the years" ,input$Years[1] , "and" ,input$Years[2] ) , x = "", y = "" , fill = "change") +
  coord_quickmap() +
  theme(legend.position = "bottom")

