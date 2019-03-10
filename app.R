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
source("analysis.R")



my_ui <- fluidPage(
  sidebarLayout(
    # interaction panel
    sidebarPanel(
      ##select the features to display
      radioButtons(inputId = "type2", "Data Type",
                   c("yr2010","yr2015", "change")
                   ),
      selectInput(inputId = "type1", label = "trend",
                  unique(gather_pop$trend)
      )), 
    # display panel
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Data Table", textOutput("selected_var2") ,DT::dataTableOutput("mytable")),
                  tabPanel("Data Plot", textOutput("selected_var1") ,plotOutput("plot"))
      )
    )
  )
)



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
    world_pop_map <- world_pop_map %>% 
      mutate(difference = cut(change, breaks=bin_values, labels=c(paste(bin_values_rounded[1],"to",bin_values_rounded[2]), 
                                                                           paste(bin_values_rounded[1],"to",bin_values_rounded[2]), 
                                                                           paste(bin_values_rounded[2],"to",bin_values_rounded[3]), 
                                                                           paste(bin_values_rounded[3],"to",bin_values_rounded[4]), 
                                                                           paste(bin_values_rounded[4],"to",bin_values_rounded[5]))))
    
    
    ggplot(data = world_pop_map) +
      geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = difference)) +
      scale_fill_brewer(palette = "RdYlGn") +
      #labs(title = paste("Change in" , input$type , "between the years" ,input$Years[1] , "and" ,input$Years[2] ) , x = "", y = "" , fill = "change") +
      coord_quickmap() +
      theme(legend.position = "bottom")
    
    
  })
  

shinyApp(ui = my_ui , server = my_server)

