#install.packages("shiny")
#install.packages("DT")
#install.packages("rsconnect")
library("rsconnect")
library("shiny")
library("ggplot2")
library("dplyr")
library("tidyr")
library("DT")
library("maps")
rsconnect::setAccountInfo(name='soham-gh', token='43013155CD55E73DD4A85A0B78A28FDD',
                          secret='J5Ldo7vylZ9eow6mA6rE4sSLjhG4/VOJSqwEZIHP')
options(scipen = 999)
gather_pop <- read.csv("gather_pop.csv",stringsAsFactors = FALSE)

complete_data <- read.csv("all_data_with_regions.csv",stringsAsFactors = FALSE)
selected_complete_data <- select(complete_data, -X, -Life.expectancy.at.birth.for.females..years.,-Life.expectancy.at.birth.for.males..years.,
                                 -GDP.real.rates.of.growth..percent., -GDP.in.constant.2010.prices..millions.of.US.dollars.,-name, -Region.Country.Area)

 

colnames(selected_complete_data) <- c("Region", "Year", "Infant_mortality", "Life_expectancy","Maternal_mortality_ratio", "Annual_population__rate_of_change", "Fertility_rate", "GDP_millions_of_USD", "GDP_per_capita_USD", "region","sub_region")




my_server <-  function(input, output){
  output$plot <- renderPlot({
    world_map <- map_data("world") %>%
      mutate(Country.Code = iso.alpha(region , 3))
    ##filtering based on user input 
    data_new <-  gather_pop %>% 
      mutate(Country.Code = iso.alpha(region , 3)) %>%
      filter(trend == input$type1)  
    
    world_pop_map <- left_join(world_map, data_new, by = "Country.Code") 
    ##finding the 5 bins based on quantiles 
    if(input$type2 == "yr2010"){
      column <- as.numeric(world_pop_map$yr2010)
      rakes <- quantile(column , prob = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = T)
      bins = cut(column,breaks = rakes, labels=c(paste(rakes[1],"to",rakes[2]), 
                                                 paste(rakes[2],"to",rakes[3]), 
                                                 paste(rakes[3],"to",rakes[4]), 
                                                 paste(rakes[4],"to",rakes[5]),
                                                 paste(rakes[5],"to",rakes[6])))
      world_pop_map <- mutate(world_pop_map, bins) 
    }
    else if(input$type2 == "yr2015"){
      column <- as.numeric(world_pop_map$yr2015)
      rakes <- quantile(column , prob = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = T)
      bins = cut(column,breaks = rakes, labels=c(paste(rakes[1],"to",rakes[2]), 
                                                 paste(rakes[2],"to",rakes[3]), 
                                                 paste(rakes[3],"to",rakes[4]), 
                                                 paste(rakes[4],"to",rakes[5]),
                                                 paste(rakes[5],"to",rakes[6])))
      world_pop_map <- mutate(world_pop_map, bins) 
    }
    else{
      column <- as.numeric(world_pop_map$change)
      rakes <- quantile(column , prob = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = T)
      bins = cut(column,breaks = rakes, labels=c(paste(rakes[1],"to",rakes[2]), 
                                                 paste(rakes[2],"to",rakes[3]), 
                                                 paste(rakes[3],"to",rakes[4]), 
                                                 paste(rakes[4],"to",rakes[5]),
                                                 paste(rakes[5],"to",rakes[6])))
      world_pop_map <- mutate(world_pop_map, bins) 
    }
   
    ggplot(data = world_pop_map) +
      geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = bins)) +
      scale_fill_brewer(palette = "RdYlGn") +
      #labs(title = paste("Change in" , input$type , "between the years" ,input$Years[1] , "and" ,input$Years[2] ) , x = "", y = "" , fill = "change") +
      coord_quickmap() +
      theme(legend.position = "bottom")
    
    
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
      bins = cut(column,breaks = rakes, labels=c(paste(rakes[1],"to",rakes[2]), 
                                                                    paste(rakes[2],"to",rakes[3]), 
                                                                    paste(rakes[3],"to",rakes[4]), 
                                                                    paste(rakes[4],"to",rakes[5])))
      regioned <- mutate(regioned, bins) 
    }
    else{
      column <- regioned$GDP_per_capita_USD
      rakes <- c(0,32,5000,40000, Inf)
      bins = cut(column, breaks = rakes, labels=c(paste(rakes[1],"to",rakes[2]), 
                                                  paste(rakes[2],"to",rakes[3]), 
                                                  paste(rakes[3],"to",rakes[4]), 
                                                  paste(rakes[4],"to",rakes[5])))
      regioned <-  mutate(regioned, bins)
      
    }
    
    thegraph <- ggplot(regioned, na.rm = T) +
      geom_point(mapping = aes_string(y = input$radio_key , x = input$select_key2, color = bins ))
    
    
  thegraph
  
    
    
  })

}





page_one <- tabPanel( "First Page",
  sidebarLayout(
    # interaction panel
    sidebarPanel(
      ##select the features to display
      radioButtons(inputId = "type2", label = "Data Type",choices = 
                   c("yr2010","yr2015", "change")
      ),
      selectInput(inputId = "type1", label = "trend",
                  unique(gather_pop$trend)
      )), 
    # display panel
    mainPanel(
       textOutput("selected_var1") ,plotOutput("plot"))
      )
    )
  




page_two <-  tabPanel( "Second Page",
                       titlePanel("Vizualization"),
                       sidebarLayout(  # lay out the passed content into two columns
                         sidebarPanel( # lay out the passed content inside the "sidebar" column
                           radioButtons(inputId = "rb_yr", label = "Pick a year", choices = c(2010,2015 )),
                           selectInput( inputId = "select_key2", label = "Choose the independant variable (x-axis)",
                                        choices = c("Infant mortality"="Infant_mortality", "Life expectancy"="Life_expectancy", "Maternal mortality ratio"="Maternal_mortality_ratio", "Annual population rate of change"="Annual_population__rate_of_change", "Fertility rate"="Fertility_rate", "GDP millions of USD" = "GDP_millions_of_USD", "GDP per capita USD"="GDP_per_capita_USD")),
                           radioButtons( inputId = "radio_key", label = "Choose an dependant variable (y-axis)",
                                         choices = c("Infant mortality"="Infant_mortality", "Life expectancy"="Life_expectancy", "Maternal mortality ratio"="Maternal_mortality_ratio", "Annual population rate of change"="Annual_population__rate_of_change", "Fertility rate"="Fertility_rate", "GDP millions of USD" = "GDP_millions_of_USD", "GDP per capita USD"="GDP_per_capita_USD")),
                           selectInput( inputId = "type3", label = "Color by:", choices = c("GDP per capita USD"="GDP_per_capita_USD","GDP millions of USD" = "GDP_millions_of_USD")),
                           selectInput( inputId = "type4", label = "Filter by region:", choices = c("All","Africa", "Americas","Asia", "Europe", "Oceania"))
                           #selectInput( inputId = "type5", label = "Filter by sub region:", choices = unique(filter(selected_complete_data, Year == input$rb_yr, region == input$type4) %>% select(sub_region)))
                           
                         ),
                         mainPanel(    # lay out the passed content inside the "main" column
                           textOutput(outputId = "messagetwo"),
                           plotOutput(outputId = "graph")
                         )
                       )
)





my_ui <- navbarPage("My application", page_one, page_two)




shinyApp(ui = my_ui , server = my_server)






