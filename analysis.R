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

countries <- c("USA", "Canada", "Afghanistan", "Albania", 
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

countries_isod <- iso.alpha(countries, 3)
countries_isod
c <- unname(unlist(x[1,]))
colnames(x) <-  c
colnames(x)[colnames(x) == ''] <- "Region"
population_data_by_country <- x %>% 
  subset(Region %in% countries) %>% 
  filter(Year == "2015" | Year == "2010")%>% 
  select(Region, Year, Series, Value) %>% 
  spread(Series, Value)


c2 <- unname(unlist(y[1,]))
colnames(y) <-  c2
colnames(y)[colnames(y) == ''] <- "Region"
gdp_data_by_country <- y %>% subset(Region %in% countries) %>% 
  filter(Year == "2010" | Year == "2015") %>% 
  select(Region, Year, Series, Value) %>% spread(Series, Value)


gather_pop <- population_data_by_country %>% gather("trend" ,"value" , 
                                                    `Infant mortality for both sexes (per 1,000 live births)`: `Total fertility rate (children per women)`) %>%
  spread("Year" , "value") 

colnames(gather_pop) <- c("region", "trend", "yr2010" , "yr2015")
gather_pop <- gather_pop %>% mutate(change = as.numeric(yr2015 , na.rm = T) - as.numeric(yr2010, na.rm = T))
## joins data tables by country.
all_data <- full_join(population_data_by_country , gdp_data_by_country, by = NULL, type = "full" , match = "all")

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