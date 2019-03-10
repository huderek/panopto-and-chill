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
    #temp <- as.character(input$type2)
    bin_values <- quantile(world_pop_map$change , probs = c(0, 0.2, 0.4, 0.6, 0.8, 1) , na.rm = T)
    bin_values_rounded <-  round(bin_values)

    world_pop_map <-  mutate(world_pop_map, difference = cut(change, breaks=bin_values, labels=c(paste(bin_values_rounded[1],"to",bin_values_rounded[2]), 
                                                                  paste(bin_values_rounded[2],"to",bin_values_rounded[3]), 
                                                                  paste(bin_values_rounded[3],"to",bin_values_rounded[4]), 
                                                                  paste(bin_values_rounded[4],"to",bin_values_rounded[5]), 
                                                                  paste(bin_values_rounded[5],"to",bin_values_rounded[6]))))
    
    


    ggplot(data = world_pop_map) +
      geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = difference), color = "black", size = .1) +
      
      scale_fill_brewer(palette = "RdYlGn") +
      coord_quickmap() +
      theme(legend.position = "bottom")+
      
      if(input$type2 == "change"){
        labs(title = paste("Change in" , input$type1 , "from 2010 to 2015"))  
      }else{
        labs(title = paste(input$type1, "data from", gsub("yr","", input$type2)))
      }
  })
  
  output$graph <- renderPlot({
    by_yr <- filter(selected_complete_data, Year == input$rb_yr)
   
    
    thegraph <- ggplot(by_yr, na.rm = T) +
      geom_point(mapping = aes_string(y = input$radio_key , x = input$select_key2 ))
 
    
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
       #textOutput("selected_var1") 
      plotOutput("plot"))
      )
    )
  


page_two <-  tabPanel( "Second Page",
                       titlePanel("Visualization"),
                       sidebarLayout(  # lay out the passed content into two columns
                         sidebarPanel( # lay out the passed content inside the "sidebar" column
                           radioButtons(inputId = "rb_yr", label = "Pick a year", choices = c(2010,2015 )),
                           selectInput( inputId = "select_key2", label = "Choose the independant variable (x-axis)",
                                        choices = c("Infant mortality"="Infant_mortality", "Life expectancy"="Life_expectancy", "Maternal mortality ratio"="Maternal_mortality_ratio", "Annual population rate of change"="Annual_population__rate_of_change", "Fertility rate"="Fertility_rate", "GDP millions of USD" = "GDP_millions_of_USD", "GDP per capita USD"="GDP_per_capita_USD")),
                           radioButtons( inputId = "radio_key", label = "Choose an dependant variable (y-axis)",
                                         choices = c("Infant mortality"="Infant_mortality", "Life expectancy"="Life_expectancy", "Maternal mortality ratio"="Maternal_mortality_ratio", "Annual population rate of change"="Annual_population__rate_of_change", "Fertility rate"="Fertility_rate", "GDP millions of USD" = "GDP_millions_of_USD", "GDP per capita USD"="GDP_per_capita_USD"))
                           
                         ),
                         mainPanel(    # lay out the passed content inside the "main" column
                           textOutput(outputId = "messagetwo"),
                           plotOutput(outputId = "graph")
                         )
                       )
)


thegraph <- ggplot(selected_complete_data, na.rm = T) +
  geom_point(mapping = aes(y = Life_expectancy, x = Fertility_rate )) 


my_ui <- navbarPage("My application", page_one, page_two)




shinyApp(ui = my_ui , server = my_server)






