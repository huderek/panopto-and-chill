# install.packages("shiny")
# install.packages("DT")
#install.packages("rsconnect")
#install.packages('plotly')
library("plotly")
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
    #ggplotly(p)
  })
#####################################################################################################################################  
  output$graph <- renderPlotly({
 
   by_yr <- filter(selected_complete_data, Year == input$rb_yr)

    if(input$type4 == "All"){
      regioned <- by_yr
    }else{
      regioned <- filter(by_yr, region == input$type4)
    }
   

    
    thegraph <- ggplot(regioned, na.rm = T) +
      geom_point(mapping = aes_string(x = log(regioned$GDP_per_capita_USD) , y = input$type3, fill = "Region")) + ##log to make trend more visible 
     theme(legend.position="none")
  ggplotly(thegraph)
  })
  
  output$q2text <- renderText({
    by_years <- filter(selected_complete_data, Year == input$rb_yr)
    health_statistic <- select(by_years,Annual_population__rate_of_change,input$type3,region,Region, GDP_per_capita_USD )
    if(input$type4 == "All"){
      by_region_filt <- health_statistic
    }else{
      by_region_filt <- filter(health_statistic, region == input$type4)
    }
    conversion_list5 <- list("Life_expectancy"= "Life expectancy in years" ,"Fertility_rate"="Fertility Rate","Maternal_mortality_ratio"="Maternal mortallity ratio","Infant_mortality"="Infant Mortallity")
    
    average_GDP_new <-  mean(by_region_filt$GDP_per_capita_USD, na.rm = T)
    average_GDP_text <- paste0("The mean GDP per capita is " , average_GDP_new)
    average_health_statistic_new <- mean(by_region_filt[[input$type3]],na.rm = T)
    
    total <- nrow(by_region_filt)
    above_and_above_new <- (nrow(filter(by_region_filt, by_region_filt$GDP_per_capita_USD >= average_GDP_new & (by_region_filt[[input$type3]] >= average_health_statistic_new) )) )
    perc_deviation_ab_new <- ((above_and_above_new/total)* 100) 
    
    total <- nrow(by_region_filt)
    below_and_below_new <- (nrow(filter(by_region_filt, by_region_filt$GDP_per_capita_USD <= average_GDP_new & (by_region_filt[[input$type3]] <= average_health_statistic_new) )) )
    perc_deviation_new <- ((below_and_below_new/total) * 100)
    
    average_health_statistic_text <- paste0("The mean of GDP per capita was " , round(average_GDP_new,1),
                                           "% and the mean of ",  conversion_list5[input$type3] , " was " ,round(average_health_statistic_new,1), "in ", input$rb_yr, " for countries in ", 
                                           input$type4, ". ", perc_deviation_ab_new, "% of contries had a GDP per capita increase above the mean and also had a ",conversion_list5[input$type3], " above the mean ",
                                           conversion_list5[input$type3],". While, ", 
                                           perc_deviation_new, "% of contries have a GDP per capita that is below the average population change and a ",conversion_list5[input$type3], " that is below the average ",
                                           conversion_list5[input$type3]  )

    c1 <- by_region_filt[[input$type3]]
    c2 <- by_region_filt$GDP_per_capita_USD
    correlation <- round(cor(c1, c2, use = "pairwise.complete.obs"), 3)
    analysis <-  paste("The kendall correlation between GDP per capita and", input$type3 , correlation , ". ")
    continued <- "A negative correlation is a relationship between two variables such that as the value of one variable increases, the other decreases."
    final <- paste(analysis, continued, average_GDP_text, average_health_statistic_text)
  })

######################################################################################################################################  
  
output$q1 <-renderPlotly({
  if(input$the_year == "2010"){
    by_region <- filter(selected_complete_data, Year == 2010)
  }
  else{
    by_region <- filter(selected_complete_data, Year == 2015)
  }
  

  if(input$region1 == "All"){
    by_region <- by_region
  }else{
    by_region <- filter(by_region, region == input$region1)
  }
  column <- by_region$GDP_per_capita_USD
  rakes <- c(0,32,5000,40000, Inf)
  bins = cut(column,breaks = rakes, labels=c(paste0("$",rakes[1]," to $",rakes[2]),
                                             paste0("$",rakes[2]," to $",rakes[3]),
                                             paste0("$", rakes[3]," to $", rakes[4]),
                                             paste0("Greater than $",rakes[4])))
  by_region2 <- mutate(by_region, bins = round(GDP_per_capita_USD , -4))


  p <- plot_ly(by_region2, x = ~bins, y = ~Annual_population__rate_of_change , type = "box")


})
  
#####################################################################################
output$question4 <-renderPlotly({
  by_yer <- filter(selected_complete_data, Year == input$year)
  health_stat <- select(by_yer,Annual_population__rate_of_change,input$stat,region, Region )
  if(input$region_type == "All"){
    by_region_filtered <- health_stat
  }else{
    by_region_filtered <- filter(health_stat, region == input$region_type)
  }
  
  
  conversion_list3 <- list("Life_expectancy"= "Life expectancy in years" ,"Fertility_rate"="Fertility Rate","Maternal_mortality_ratio"="Maternal mortallity ratio","Infant_mortality"="Infant Mortallity")
  
  
  q4_graph <- ggplot(by_region_filtered, na.rm = T) +
    geom_point(mapping = aes_string(y = input$stat , x = "Annual_population__rate_of_change", fill = "Region"))+
    theme(legend.position = "none")+
    xlab("Population annual rate of increase (percent)")+
    ylab(input$stat)
  
  
  
  ggplotly(q4_graph)
  
})

  output$gdp_population_txt <- renderText({
    x <- paste("You are now viewing the data for the region of" , input$region1, "for the year of", input$the_year)
  })
  
#############################################################################################################################

output$q4text <- renderText({
  by_yer <- filter(selected_complete_data, Year == input$year)
  health_stat <- select(by_yer,Annual_population__rate_of_change,input$stat,region,Region )
  if(input$region_type == "All"){
    by_region_filtered <- health_stat
  }else{
    by_region_filtered <- filter(health_stat, region == input$region_type)
  }
  conversion_list4 <- list("Life_expectancy"= "Life expectancy in years" ,"Fertility_rate"="Fertility Rate","Maternal_mortality_ratio"="Maternal mortallity ratio","Infant_mortality"="Infant Mortallity")
  
  average_population <-  mean(by_region_filtered$Annual_population__rate_of_change, na.rm = T)
  average_populationtxt <- paste0("The Average pop mean is " , average_population)
  average_health_statistic <- mean(by_region_filtered[[input$stat]],na.rm = T)
  
  
  total <- nrow(by_region_filtered)
  above_and_above <- (nrow(filter(by_region_filtered, by_region_filtered$Annual_population__rate_of_change >= average_population & (by_region_filtered[[input$stat]] >= average_health_statistic) )) )
  perc_deviation_ab <- ((above_and_above/total)* 100) 
  
  total <- nrow(by_region_filtered)
  below_and_below <- (nrow(filter(by_region_filtered, by_region_filtered$Annual_population__rate_of_change <= average_population & (by_region_filtered[[input$stat]] <= average_health_statistic) )) )
  perc_deviation <- ((below_and_below/total) * 100)
  
  average_health_statistic_txt <- paste0("The mean of average population change was " , round(average_population,1), "% and the mean of ",  conversion_list4[input$stat] , " was " ,round(average_health_statistic,1), "in ", input$year, " for countries in ", input$region_type, ". ", perc_deviation_ab, "% of contries had an average population increase above the mean and also had a ",conversion_list4[input$stat], " above the mean ",conversion_list4[input$stat],". While, ", perc_deviation, "% of contries have a average population change that is below the average population change and a ",conversion_list4[input$stat], " that is below the average ",conversion_list4[input$stat]  )
  
  
  
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
This visualization helps one determine what the life expectancy, infant mortality rate and maternal mortality rate were in 2015, 2010 as well as the change in these parameters over those years. Since it is a world map one can easily tell which regions performs the best while which regions performed the worst. 
    The map is augmented by bins that are based on quantiles. Red represents the worst performing 10% while dark green represents the best performing 10%. The other break offs are at 40%, 60% and 90%.
    The purpose of this visual is to understand the spread of these parameters around the world. More detailed analysis of the effect that the GDP per capita has on these parameters 
    is conducted in question 2. We chose GDP per capita in the next analysis because we intuitively believe it would have the strongest effect on these parameters")
)
###############################################################################################################################
page_two <-  tabPanel( "Question 2",
                       titlePanel("What is the Correlation between GDP per capita and Health parameters of a country"),
                       sidebarLayout(  # lay out the passed content into two columns
                         sidebarPanel( # lay out the passed content inside the "sidebar" column
                           radioButtons(inputId = "rb_yr", label = "Pick a year", choices = c(2010,2015 )),
                           
                           selectInput( inputId = "type3", label = "trend", choices =  c("Infant_mortality", 
                                                                                         "Life_expectancy", 
                                                                                  "Maternal_mortality_ratio")),
                           
                           selectInput( inputId = "type4", label = "Filter by region:", choices = c("All","Africa", "Americas","Asia", "Europe", "Oceania"))
                         ),
                         mainPanel(    # lay out the passed content inside the "main" column

                           plotlyOutput(outputId = "graph"), 
                           textOutput(outputId = "q2text")

                         )
                       ),
                       strong("3. Infant Mortality vs. Fertility Rates"),
                       p("When comparing fertility rates to infant mortality, we found that countries with lower GDP per capita saw higher rates of both fertility and infant mortality. 
                         High infant mortality can be attributed to lack of adequate healthcare systems in countries with low GDP per capita. Intuition suggests that lower GDP would result 
                         in lower fertility rates, because it's costly to raise a child. In reality, many countries with lower GDP also can utilize children as a financial asset to do work that requires manual labor. 
                         The opposite is true in countries like the US, where children are seen as a financial costs, not an investment. Although morbid, high fertility can be attributed to high infant mortality rate as well,
                         as parents may look to replace children that die in infancy. Using our region filter widget, we found that European nations generally tend to have the lowest infant mortality and fertility rates.
                         African nations have the highest in both categories.All three health parametres seem to have a very high correlation with GDP per capita thus confirming our hypothesis.
                         The maternal mortality rate and infant mortality rate have very high negative correlations with GDPper capita. This makes sense because individuals with
                         higher average income can spend more on healthcare facilities. GDP per capita is also a determining factor in economic development and infrastructure because it implies low unemployment.
                         As a result, people in these countries can focus more on their helathcare needs."), 
                       p("Life expectancy has a high positive correlation with GDP per capita for the same reasons described above. People with more disposable income spend more on health care, pushing up the life expectancy in the region in the long run."), 
                       p("the purpose of the mean tests are to broadly determine what percentage of the countries violate the expected trend based on the correlation
                         coefficient. The percentage results of the mean tests depict the percentage of points lying in the second and fourth quadrant respectively"),
                       p("In this process, the means of the both dependent and independent variables are taken(in lieu of more advanced statistical methods), essentially dividing
                         the plot into 4 quadrants. In negative linear relationships such as this, one expects all the data to only occupy the first and the third quadrants, the points occupying other quadrants are said to violate the trend.
                         This analysis can be applied to the moratily rates.When viewing a trend with positive correlation as in the case of Life Expectancy, 
                         one expects most points to lie in the second and fourth quadrant, while points in other quadrants are outliers. ")
)
##################################################################################################################################################

page_four <-  tabPanel( "Sources", 
                        titlePanel("Works Cited"), 
gdp_data <- a("GDP Data Source", 
              href ="http://data.un.org/_Docs/SYB/PDFs/SYB60_T03_Population%20Growth,%20Fertility%20and%20Mortality%20Indicators.pdf"),
p(life_exp_url <- a("Population Data Source", 
                    href= "http://data.un.org/_Docs/SYB/PDFs/SYB61_T13_GDP%20and%20GDP%20Per%20Capita.pdf")))

##################################################################################
page_three <- tabPanel("Question 3",
                       titlePanel("Does GDP effect the Annual population rate of Change?"), 
                       sidebarLayout(
                         sidebarPanel(
                           selectInput(inputId = "region1", label = "Pick a region",choices = c("All","Africa", "Americas","Asia", "Europe", "Oceania")), 
                           selectInput(inputId = "the_year", label = "Pick a year",choices = c("2010","2015"))
                         ),
                         mainPanel(
                           plotlyOutput(outputId = "q1" ),
                           textOutput(outputId = "gdp_population_txt")
                         )
                       ),
                       strong("3. GDP VS, Annual Population Rate Of Change"),
                       p("Based on the box plots, GDP and population rate don't show clear correlations in some regions. For example, there is no correlation
                         in Asia because there are many diffrent countries and there are more factors that cause it to corrlate besides the GDP per capita. Countries like India and China for example 
                         have very different demographic aspects when compared to for example, the middle east. On the other had,
                         The box plot shows a corrlation in Africa because it has a lot of simmillar countries in it, demographically spreaking. The corellation shows as the GDP increases,  
                         the mean of the population rate decreases. This makes sense because in the short run, it is difficult for an economy to change its GDP, but as the population increases,
                         the net GDP is divided among more people, resulting in lower per Capita GDP. This trend is not the same across all regions because the reaons for population chnage may vary.
                         Often Immigration is the cause for population change and most often, these immigrants directly contribute to the workforce and therefore, the GDP")
                       
)

############################################################################################################

page_five <- tabPanel("Question 4",
                      titlePanel("What is the correlation between Population annual rate of increase (percent) and various health statistics? 
"), 
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons(inputId = "year", label = "Pick a year", choices = c(2010,2015 )),
                          
                          selectInput( inputId = "stat", label = "Choose a Health Statistic:", 
                                       choices = c("Life expectancy in years"= "Life_expectancy","Fertility Rate" ="Fertility_rate","Maternal mortallity ratio"= "Maternal_mortality_ratio","Infant Mortallity"="Infant_mortality" )),
                          selectInput( inputId = "region_type", label = "Filter by region:", 
                                       choices = c("All","Africa", "Americas","Asia", "Europe", "Oceania"))),
                        
                        mainPanel(
                          plotlyOutput(outputId = "question4" ),
                          textOutput(outputId = "q4text")
                        )
                      ),
                      strong("4. Annual population rate of change vs. life expectancy"),
                      p("Based on the graphical representation and the statistical analysis I can include that
                        population annual rate of increase is effected by some health statistics."), 
                      p("When selecting fertility rate a the health statistic for the year 2010 in all
                        countries. I can see that Fertility rate has a strong correlation with the population 
                        annual rate of increase since a large percent of countries population rate is above 
                        average when the fertility rate is also above average and a a large percentage ,  and thus I can deduce that fertility rate positively correlates with the population annual rate of increase. While there may be other factors effecting the population change fertility rate is the
                        average children per mother and thus is likely to correlate strongly with the population change."), 
                      p("When selecting life expectancy as the health statistic for the year 2010 
                        in all countries. I can see that an increase in life expectancy does not necessarily 
                        effect the population rate of change. This is because a small percentage of countries 
                        population change is above the average population change while also having an above average 
                        life expectancy. The same is true for countries with a below average change in population and
                        a below average life expectancy, there is a small number of countries with a below average in both. T
                        herefore an change in population is unlikely to be effected by an increase in life expectancy. 
                        This might be because if people live longer there is less of an incentive to have more children and 
                        increase the population. "),
                      p("When selecting life expectancy as the health statistic
                        for the year 2010 in all countries. I can see that there is some relationship
                        between Maternal mortality ratio and population change but not enough a correlation to 
                        signify causation. About 20%  percentage of countries have an above average population change and above average maternal mortality ratio. 
                        And about 34.8% of countries had a below average Maternal mortality ratio and population change. Therefore maternal mortality ratio is unlikely to effect population rate. 
                        This may be due to the fact that maternal mortality rate is likely linked to the quality of healthcare in a nation. "),
                      p("When selecting infant mortallity as the health statistic for the year 2010 in all countries. I can see that About 24.8% of countries had an average population increase above the mean and also had Infant mortality increase above the mean Infant mortality rate. From this I see that a high infant mortality doesn't necessarily correlate with population change. 
                        Also 36.4% of countries have below average in both population change and infant mortality.")
                      
                      
)                   
################################################################################################################

page_zero <- tabPanel(
  "Introduction", titlePanel("Introduction"),
  p("This application uses GDP and healthcare country data gathered from the United Nations. We pulled data from the UN databases and cleaned the data sets using dplyr functions in order to plot visualizations to perform analysis. 
    There are four visualization tabs aimed to answer four questions we had about healthcare and GDP."),
  p("Question 1 aims to answer how healthcare statistics have changed from 2010 to 2015. The user can toggle between different healthcare stats. Question 2 looks to find the correlation between GDP per capita of a country and 
    its healthcare statistics. Question 3 looks into the relationship between GDP and population growth. Finally, Question 4 studies the the relationship between the population rate of change and life expectancy of countries. In all graphs, there is the opportunity for the user to do further analysis beyond the questions we have asked, by using different filters and statistics.")
)


my_ui <- navbarPage("GDP & Healthcare Analysis", page_zero ,page_one, page_two,page_three,page_five,page_four)



shinyApp(ui = my_ui, server = my_server)
