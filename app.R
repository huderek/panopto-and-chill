# install.packages("shiny")
# install.packages("DT")
# install.packages("rsconnect")
library("rsconnect")
library("shiny")
library("ggplot2")
library("dplyr")
library("tidyr")
library("DT")
library("maps")
rsconnect::setAccountInfo(
  name = "soham-gh", token = "43013155CD55E73DD4A85A0B78A28FDD",
  secret = "J5Ldo7vylZ9eow6mA6rE4sSLjhG4/VOJSqwEZIHP"
)
options(scipen = 999)

gather_pop <- read.csv("gather_pop.csv",stringsAsFactors = FALSE)


complete_data <- read.csv("all_data_with_regions.csv", stringsAsFactors = FALSE)
selected_complete_data <- select(complete_data, -X, -Life.expectancy.at.birth.for.females..years., -Life.expectancy.at.birth.for.males..years.,
  -GDP.real.rates.of.growth..percent., -GDP.in.constant.2010.prices..millions.of.US.dollars., -name, -Region.Country.Area
)

colnames(selected_complete_data) <- c("Region", "Year", "Infant_mortality", "Life_expectancy", "Maternal_mortality_ratio", "Annual_population__rate_of_change", "Fertility_rate", "GDP_millions_of_USD", "GDP_per_capita_USD", "region", "sub_region")

my_server <-  function(input, output){
  output$plot <- renderPlot({
    world_map <- map_data("world") %>%
      mutate(Country.Code = iso.alpha(region , 3))
    ##filtering based on user input 
    data_new <-  gather_pop %>% 
      mutate(Country.Code = iso.alpha(region , 3)) %>%
      filter(trend == input$type1)  
    
    world_pop_map <- left_join(world_map, data_new, by = "Country.Code") 
    
    
    output$sentance <- renderText({
      conversion_list <- list(
        Infant_mortality = "Infant mortality",
        Life_expectancy = "Life expectancy", 
        Maternal_mortality_ratio = "Maternal mortality ratio", 
        Annual_population__rate_of_change = "Annual population rate of change", 
        Fertility_rate = "Fertility rate",
        GDP_millions_of_USD = "GDP millions of USD", 
        GDP_per_capita_USD = "GDP per capita USD"
      )
      paste("You are now viewing", conversion_list[input$select_key2], "VS.", conversion_list[input$radio_key], "for year", input$rb_yr, 
            "which is colored by", conversion_list[input$type3], "and also filtered by the region of", input$type4, ".")
    })
    
    output$header <- renderText({
      conversion_list <- list(
        Infant_mortality = "Infant mortality",
        Life_expectancy = "Life expectancy", 
        Maternal_mortality_ratio = "Maternal mortality ratio", 
        Annual_population__rate_of_change = "Annual population rate of change", 
        Fertility_rate = "Fertility rate",
        GDP_millions_of_USD = "GDP millions of USD", 
        GDP_per_capita_USD = "GDP per capita USD"
      )
      
      paste(conversion_list[input$select_key2], "VS.", conversion_list[input$radio_key], "for year", input$rb_yr, ".")
      
    })
 
    if(input$type2 == "yr2010"){
      column <- as.numeric(world_pop_map$yr2010)


      rakes <- quantile(column , prob = c(0, 0.1, 0.4, 0.6, 0.9, 1), na.rm = T)

      bins = cut(column,breaks = rakes, labels=c(paste(rakes[1],"to",rakes[2]), 
                                                 paste(rakes[2],"to",rakes[3]), 
                                                 paste(rakes[3],"to",rakes[4]), 
                                                 paste(rakes[4],"to",rakes[5]),
                                                 paste(rakes[5],"to",rakes[6])))
      world_pop_map <- mutate(world_pop_map, bins) 

    }
    else if(input$type2 == "yr2015"){
      column <- as.numeric(world_pop_map$yr2015)

      rakes <- quantile(column , prob = c(0, 0.1, 0.4, 0.6, 0.9, 1), na.rm = T)

      bins = cut(column,breaks = rakes, labels=c(paste(rakes[1],"to",rakes[2]), 
                                                 paste(rakes[2],"to",rakes[3]), 
                                                 paste(rakes[3],"to",rakes[4]), 
                                                 paste(rakes[4],"to",rakes[5]),
                                                 paste(rakes[5],"to",rakes[6])))
      world_pop_map <- mutate(world_pop_map, bins) 
    }
    else{
      column <- as.numeric(world_pop_map$change)

      world_pop_map <- mutate(world_pop_map, bins) 

      rakes <- round(quantile(column, prob = c(0, 0.3, 0.6, 0.9, 1), na.rm = T), 4)
      bins <- cut(column, breaks = rakes, labels = c(
        paste(rakes[1], "to", rakes[2]),
        paste(rakes[2], "to", rakes[3]),
        paste(rakes[3], "to", rakes[4]),
        paste(rakes[4], "to", rakes[5])
      ))
      world_pop_map <- mutate(world_pop_map, bins)


    }
    conversion_list2 <- list("Infant mortality for both sexes (per 1,000 live births)"  = "Infant mortality per 1,000 live births" ,
                            "Life expectancy at birth for both sexes (years)"   = "Life expectancy in years",
                            "Life expectancy at birth for females (years)"  = "Life expectancy in years",
                            "Life expectancy at birth for males (years)" = "Life expectancy in years",
                            "Maternal mortality ratio (deaths per 100,000 population)" = "Maternal mortality ratio",
                            "Population annual rate of increase (percent)"  = "Rate of annual population percent change",
                            "Total fertility rate (children per women)"  = "Total fertility rate")
    
    ggplot(data = world_pop_map) +
      geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = bins), color = "black", size = .1)  +
      scale_fill_brewer(palette = "RdYlGn") +
      coord_quickmap() +
      theme(legend.position = "bottom")+
      labs(fill = conversion_list2[input$type1])+
      
      if(input$type2 == "change"){
        labs(title = paste("Change in" , input$type1 , "from 2010 to 2015"))  
      }else{
        labs(title = paste(input$type1, "data from", gsub("yr","", input$type2)))
      }
  })
  
  output$graph <- renderPlot({
 
   by_yr <- filter(selected_complete_data, Year == input$rb_yr)

    if(input$type4 == "All"){
      regioned <- by_yr
    }else{
      regioned <- filter(by_yr, region == input$type4)
    }
   
    if(input$type3 == "GDP_millions_of_USD"){
      column <- regioned$GDP_millions_of_USD
      rakes <- c(0,995,3900,12055, Inf)
      bins = cut(column,breaks = rakes, labels=c(paste0("$",rakes[1]," to $",rakes[2]), 
                                                       paste0("$",rakes[2]," to $",rakes[3]), 
                                                       paste0("$", rakes[3]," to $", rakes[4]), 
                                                       paste0("Greater than $",rakes[4])))
      regioned <- mutate(regioned, bins) 
    }
    else{
      column <- regioned$GDP_per_capita_USD
      rakes <- c(0,32,5000,40000, Inf)
      bins = cut(column, breaks = rakes, labels=c(paste0("$",rakes[1]," to $",rakes[2]), 
                                                  paste0("$",rakes[2]," to $",rakes[3]), 
                                                  paste0("$", rakes[3]," to $", rakes[4]), 
                                                  paste0("Greater than $",rakes[4])))
      regioned <-  mutate(regioned, bins)
    }
    
    thegraph <- ggplot(regioned, na.rm = T) +
      geom_point(mapping = aes_string(x = "Annual_population__rate_of_change" , y = "Fertility_rate", color = bins )) +
      
      if(input$type3 == "GDP_per_capita_USD"){
        labs(colour = "GDP per capita (USD)")  
      }else{
        labs(colour = "GDP (USD in millions)") 
      }
  thegraph
  })
  
output$test <-renderPlot({
  year <- filter(selected_complete_data, Year == input$year3)
  ggplot(year, na.rm = T)+
    geom_point(mapping = aes_string(x = "Annual_population__rate_of_change" , y = "Fertility_rate"))
  
  
})

output$page4 <-renderPlot({
  year <- filter(selected_complete_data, Year == input$year)
  ggplot(year, na.rm = T)+
    geom_point(mapping = aes_string(x = "Annual_population__rate_of_change" , y = "Fertility_rate"))
  
  
})

}



#does the first page of the shiny
page_one <- tabPanel( "World Map",
  sidebarLayout(
    # interaction panel
    sidebarPanel(
      ## select the features to display
      radioButtons(
        inputId = "type2", label = "Data Type", choices =
          c("yr2010", "yr2015", "change")
      ),
      selectInput(
        inputId = "type1", label = "trend",
        unique(gather_pop$trend)
      )
    ),
    # display panel
    mainPanel(

      textOutput("selected_var1"), plotOutput("plot")
    )
  ) 
)
       #textOutput("selected_var1") 
page_two <-  tabPanel( "Graphs",
                       titlePanel("Visualization"),
                       sidebarLayout(  # lay out the passed content into two columns
                         sidebarPanel( # lay out the passed content inside the "sidebar" column
                           radioButtons(inputId = "rb_yr", label = "Pick a year", choices = c(2010,2015 )),
                           
                           selectInput( inputId = "type3", label = "Color by:", choices = c("GDP per capita USD"="GDP_per_capita_USD","GDP millions of USD" = "GDP_millions_of_USD")),
                           selectInput( inputId = "type4", label = "Filter by region:", choices = c("All","Africa", "Americas","Asia", "Europe", "Oceania"))
                           #selectInput( inputId = "type5", label = "Filter by sub region:", choices = unique(filter(selected_complete_data, Year == input$rb_yr, region == input$type4) %>% select(sub_region)))
                         ),
                         mainPanel(    # lay out the passed content inside the "main" column
                           textOutput(outputId = "messagetwo"),
                           span(textOutput("header"),style="font-size:25px"),
                           plotOutput(outputId = "graph"),
                           textOutput("sentance")
                         )
                       )
)



page_four <-  tabPanel( "Source", 
                        titlePanel("Works Cited"), 
gdp_data <- a("GDP Data Source", 
              href ="http://data.un.org/_Docs/SYB/PDFs/SYB60_T03_Population%20Growth,%20Fertility%20and%20Mortality%20Indicators.pdf"),
p(life_exp_url <- a("Population Data Source", 
                    href= "http://data.un.org/_Docs/SYB/PDFs/SYB61_T13_GDP%20and%20GDP%20Per%20Capita.pdf")))

page_three <- tabPanel("Graph5",
                       titlePanel("graph7"), sidebarLayout(
                         sidebarPanel(
                           radioButtons(inputId = "year3", label = "Pick a year", choices = c(2010,2015 ))
                         ),
                         mainPanel(
                           plotOutput(outputId = "test" )
                         )
                       )
  
)

page_five <- tabPanel("page 4",
                       titlePanel("graph6"), sidebarLayout(
                         sidebarPanel(
                           radioButtons(inputId = "year", label = "Pick a year", choices = c(2010,2015 ))
                         ),
                         mainPanel(
                           plotOutput(outputId = "page4" )
                         )
                       )
                       
)



page_zero <- tabPanel(
  "Introduction", titlePanel("Introduction for the webPage:")
)


my_ui <- navbarPage("My application", page_zero ,page_one, page_two,page_three,page_five,page_four)



shinyApp(ui = my_ui, server = my_server)
