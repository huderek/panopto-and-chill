# install.packages("shiny")
# install.packages("DT")
#install.packages("rsconnect")
library("rsconnect")
library("shiny")
library("ggplot2")
library("dplyr")
library("tidyr")
library("DT")
library("maps")
rsconnect::setAccountInfo(name='huderek',
                          token='4CB1BBCC2CED4670AAF793AF88CF297E',
                          secret='G/NJuif5jsXIUVSHWHYw3X6Hq+JhBCqq7ySX7jKb')
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

      rakes <- round(quantile(column, prob = c(0, 0.3, 0.6, 0.9, 1), na.rm = T), 4)
      bins = cut(column, breaks = rakes, labels = c(paste(rakes[1], "to", rakes[2]),
                                                    paste(rakes[2], "to", rakes[3]),
                                                    paste(rakes[3], "to", rakes[4]),
                                                    paste(rakes[4], "to", rakes[5])))
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
      theme(legend.position = "bottom") +
      labs(fill = conversion_list2[input$type1]) +
      
      if(input$type2 == "change"){
        labs(x = "", y = "", title = paste("Change in" , input$type1 , "from 2010 to 2015"))  
      }else{
        labs(x = "", y = "", title = paste(input$type1, "data from", gsub("yr","", input$type2)))
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
      geom_point(mapping = aes_string(x = "Infant_mortality" , y = "Fertility_rate", color = bins )) +
      
      if(input$type3 == "GDP_per_capita_USD"){
        labs(x = "Infant Mortality (per 100,000)", y = "Fertility Rate (%)", colour = "GDP per capita (USD)")  
      }else{
        labs(x = "Infant Mortality (per 100,000)", y = "Fertility Rate (%)", colour = "GDP (USD in millions)") 
      }
  thegraph
  })

  
  
output$q1 <-renderPlot({
  
  
  year <- filter(selected_complete_data, Year == input$year3)
  if(input$region1 == "All"){
    by_region <- year
  }else{
by_region <- filter(year, region == input$region1)
  }
  
  if(input$bygdp == "GDP_millions_of_USD"){
    column <- by_region$GDP_millions_of_USD
    rakes <- c(0,995,3900,12055, Inf)
    bins = cut(column,breaks = rakes, labels=c(paste0("$",rakes[1]," to $",rakes[2]), 
                                               paste0("$",rakes[2]," to $",rakes[3]), 
                                               paste0("$", rakes[3]," to $", rakes[4]), 
                                               paste0("Greater than $",rakes[4])))
    by_region <- mutate(by_region, bins) 
  }
  else{
    column <- by_region$GDP_per_capita_USD
    rakes <- c(0,32,5000,40000, Inf)
    bins = cut(column, breaks = rakes, labels=c(paste0("$",rakes[1]," to $",rakes[2]), 
                                                paste0("$",rakes[2]," to $",rakes[3]), 
                                                paste0("$", rakes[3]," to $", rakes[4]), 
                                                paste0("Greater than $",rakes[4])))
    by_region <-  mutate(by_region, bins)
  }
  ggplot(by_region, na.rm = T)+
    geom_point(mapping = aes_string(x = "Annual_population__rate_of_change" , y = "Fertility_rate", color = bins))+
    if(input$bygdp == "GDP_per_capita_USD"){
      labs(x = "Annual Pop. Rate of Change (%)", y = "Fertility Rate (%)",colour = "GDP per capita (USD)")  
    }else{
      labs(x = "Annual Pop. Rate of Change (%)", y = "Fertility Rate (%)", colour = "GDP (USD in millions)") 
    }
  
  
})

output$question4 <-renderPlot({
  by_yr <- filter(selected_complete_data, Year == input$year)
  if(input$region_type == "All"){
    regioned <- by_yr
  }else{
    regioned <- filter(by_yr, region == input$region_type)
  }
  if(input$gdp_choice == "GDP_millions_of_USD"){
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
  q4_graph <- ggplot(regioned, na.rm = T) +
    geom_point(mapping = aes_string(y = "Annual_population__rate_of_change" , x = "Life_expectancy", color = bins )) +
    
    if(input$type3 == "GDP_per_capita_USD"){
      labs(x = "Life Expectancy (yrs.)", y = "Anual Pop. Rate of Change (%)",colour = "GDP per capita (USD)")  
    }else{
      labs(x = "Life Expectancy (yrs.)", y = "Anual Pop. Rate of Change (%)",colour = "GDP (USD in millions)") 
    }
  q4_graph

})

}


#does the first page of the shiny
page_one <- tabPanel( "Question 1",
                      titlePanel("How have Healthcare Statistics of Countries changed from 2010 to 2015?"), 
  sidebarLayout(
    # interaction panel
    sidebarPanel(
      ## select the features to display
      radioButtons(
        inputId = "type2", label = "Data Type" , choices =
          c("2010"= "yr2010","2015" = "yr2015","change" = "change")
      ),
      selectInput(
        inputId = "type1", label = "trend",
        choices = c("Infant mortality for both sexes (per 1,000 live births)", "Life expectancy at birth for both sexes (years)", 
                    "Maternal mortality ratio (deaths per 100,000 population)") 
      )
    ),
    # display panel
    mainPanel(
        
      
      plotOutput("plot")
      
    )
  ),
  strong("1. How Have Healthcare Parametres of Countries changed from 2010 to 2015?"),
  p("We picked 3 of the many parameters used to evaluate the healthcare index by the WHO. We picked these because we believe they have significant impact on the healthcare index and also the data for these parameters were easily available. 
This visualization helps one determine what the life expectancy, infant mortality rate and maternal mortality rate were in 2015, 2010 as well as the change in these parameters over those years. Since it is a world map one can easily tell which regions performs the best while which regions performed the worst. The map is augmented by bins that are based on quantiles. Red represents the worst performing 10% while dark green represents the best performing 10%. The other break offs are at 40%, 60% and 90%.")
)

page_two <-  tabPanel( "Question 2",
                       titlePanel("What is the Correlation between Fertility Rates and Infant Mortality?"),
                       sidebarLayout(  # lay out the passed content into two columns
                         sidebarPanel( # lay out the passed content inside the "sidebar" column
                           radioButtons(inputId = "rb_yr", label = "Pick a year", choices = c(2010,2015 )),
                           
                           selectInput( inputId = "type3", label = "Color by:", choices = c("GDP per capita USD"="GDP_per_capita_USD","GDP millions of USD" = "GDP_millions_of_USD")),
                           selectInput( inputId = "type4", label = "Filter by region:", choices = c("All","Africa", "Americas","Asia", "Europe", "Oceania"))
                         ),
                         mainPanel(    # lay out the passed content inside the "main" column

                           plotOutput(outputId = "graph")

                         )
                       ),
                       strong("3. Infant Mortality vs. Fertility Rates"),
                       p("When comparing fertility rates to infant mortality, we found that countries with lower GDP per capita saw higher rates of both fertility and infant mortality. 
                         High infant mortality can be attributed to lack of adequate healthcare systems in countries with low GDP per capita. Intuition suggests that lower GDP would result 
                         in lower fertility rates, because it's costly to raise a child. In reality, many countries with lower GDP also can utilize children as a financial asset to do work that requires manual labor. 
                         The opposite is true in countries like the US, where children are seen as a financial costs, not an investment. Although morbid, high fertility can be attributed to high infant mortality rate as well,
                         as parents may look to replace children that die in infancy. Using our region filter widget, we found that European nations generally tend to have the lowest infant mortality and fertility rates.
                         African nations have the highest in both categories.")
)

page_four <-  tabPanel( "Sources", 
                        titlePanel("Works Cited"), 
gdp_data <- a("GDP Data Source", 
              href ="http://data.un.org/_Docs/SYB/PDFs/SYB60_T03_Population%20Growth,%20Fertility%20and%20Mortality%20Indicators.pdf"),
p(life_exp_url <- a("Population Data Source", 
                    href= "http://data.un.org/_Docs/SYB/PDFs/SYB61_T13_GDP%20and%20GDP%20Per%20Capita.pdf")))


page_three <- tabPanel("Question 3",
                       titlePanel("Does a Higher Fertility Rate Necessarily Result in a Higher Annual Rate of Population Growth?"), 
                       sidebarLayout(
                         sidebarPanel(
                           radioButtons(inputId = "year3", label = "Pick a year", choices = c(2010,2015 )),
                           selectInput(inputId = "region1", label = "Pick a region",choices = c("All","Africa", "Americas","Asia", "Europe", "Oceania")),
                           radioButtons( inputId = "bygdp", label = "Color by:", choices = c("GDP per capita USD"="GDP_per_capita_USD","GDP millions of USD" = "GDP_millions_of_USD"))
                         ),
                         mainPanel(
                           plotOutput(outputId = "q1" ) 
                         )
                       ),
                       p("Higher fertility should theoretically correlate with a higher annual rate of population. While there is a linear correlation, the slope of line is much less than 1. This is due to deaths and migration from nations, which wouldn’t allow for a 1:1 ratio of fertility. We also found that countries with higher GDP generally tend to have lower annual population growth and fertility. This may be because wealthier nations don’t have to have children for economic reasons.")
  
)

page_five <- tabPanel("Question 4",
                      titlePanel("What is Correlation between Population Rate of Change and Life Expectancy?"), 
                      sidebarLayout(
                         sidebarPanel(
                           radioButtons(inputId = "year", label = "Pick a year", choices = c(2010,2015 )),
                           
                           selectInput( inputId = "gdp_choice", label = "Color by:", 
                                        choices = c("GDP per capita USD"="GDP_per_capita_USD","GDP millions of USD" = "GDP_millions_of_USD")),
                           selectInput( inputId = "region_type", label = "Filter by region:", 
                                        choices = c("All","Africa", "Americas","Asia", "Europe", "Oceania"))),

                         mainPanel(
                           plotOutput(outputId = "question4" )
                         )
                       ),
                      strong("4. Annual population rate of change vs. life expectancy"),
                      p("We were curious to see if the population has an effect on the life expectancy of a country. It turns out that there might be a weak linear correlation between the two. 
  Although more advanced analysis is needed to confirm this. It is possible to explain this relationship as follows; countries with lower rate of population change have fewer people and thus more access 
  to healthcare facilities per person.")
                    )                   


page_zero <- tabPanel(
  "Introduction", titlePanel("Introduction"),
  p("This application uses GDP and healthcare country data gathered from the United Nations. There are four visualization tabs aimed to answer four questions we had about healthcare and GDP. 
    Question 1 aims to answer how have healthcare statistic changed from 2010 to 2015. The user can toggle between different healthcare stats. 
    Question 2 looks into the relationship between fertility rates and infant mortality. Question 3 studies the correlation between fertility and population growth. 
    Finally, Question 4 studies the the relationship between the population rate of change and life expectancy of countries. All visualizalizations allow the user to toggle between years and regions for further analysis. ")
)


my_ui <- navbarPage("GDP & Healthcare Analysis", page_zero ,page_one, page_two,page_three,page_five,page_four)



shinyApp(ui = my_ui, server = my_server)
