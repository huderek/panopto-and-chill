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

gather_pop <- read.csv("gather_pop.csv", stringsAsFactors = FALSE)


complete_data <- read.csv("all_data_with_regions.csv", stringsAsFactors = FALSE)
selected_complete_data <- select(
  complete_data, -X, -Life.expectancy.at.birth.for.females..years., -Life.expectancy.at.birth.for.males..years.,
  -GDP.real.rates.of.growth..percent., -GDP.in.constant.2010.prices..millions.of.US.dollars., -name, -Region.Country.Area
)

colnames(selected_complete_data) <- c("Region", "Year", "Infant_mortality", "Life_expectancy", "Maternal_mortality_ratio", "Annual_population__rate_of_change", "Fertility_rate", "GDP_millions_of_USD", "GDP_per_capita_USD", "region", "sub_region")

my_server <- function(input, output) {
  output$plot <- renderPlot({
    world_map <- map_data("world") %>%
      mutate(Country.Code = iso.alpha(region, 3))
    ## filtering based on user input
    data_new <- gather_pop %>%
      mutate(Country.Code = iso.alpha(region, 3)) %>%
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
      paste(
        "You are now viewing", conversion_list[input$select_key2], "VS.", conversion_list[input$radio_key], "for year", input$rb_yr,
        "which is colored by", conversion_list[input$type3], "and also filtered by the region of", input$type4, "."
      )
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

    if (input$type2 == "yr2010") {
      column <- as.numeric(world_pop_map$yr2010)
      rakes <- quantile(column, prob = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = T)
      bins <- cut(column, breaks = rakes, labels = c(
        paste(rakes[1], "to", rakes[2]),
        paste(rakes[2], "to", rakes[3]),
        paste(rakes[3], "to", rakes[4]),
        paste(rakes[4], "to", rakes[5]),
        paste(rakes[5], "to", rakes[6])
      ))
      world_pop_map <- mutate(world_pop_map, bins)
    }
    else if (input$type2 == "yr2015") {
      column <- as.numeric(world_pop_map$yr2015)
      rakes <- quantile(column, prob = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = T)
      bins <- cut(column, breaks = rakes, labels = c(
        paste(rakes[1], "to", rakes[2]),
        paste(rakes[2], "to", rakes[3]),
        paste(rakes[3], "to", rakes[4]),
        paste(rakes[4], "to", rakes[5]),
        paste(rakes[5], "to", rakes[6])
      ))
      world_pop_map <- mutate(world_pop_map, bins)
    }
    else {
      column <- as.numeric(world_pop_map$change)
      rakes <- round(quantile(column, prob = c(0, 0.3, 0.6, 0.9, 1), na.rm = T), 4)
      bins <- cut(column, breaks = rakes, labels = c(
        paste(rakes[1], "to", rakes[2]),
        paste(rakes[2], "to", rakes[3]),
        paste(rakes[3], "to", rakes[4]),
        paste(rakes[4], "to", rakes[5])
      ))
      world_pop_map <- mutate(world_pop_map, bins)
    }
    ggplot(data = world_pop_map) +
      geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = bins), color = "black", size = .1) +
      scale_fill_brewer(palette = "RdYlGn") +
      coord_quickmap() +
      theme(legend.position = "bottom") +

      if (input$type2 == "change") {
        labs(title = paste("Change in", input$type1, "from 2010 to 2015"))
      } else {
        labs(title = paste(input$type1, "data from", gsub("yr", "", input$type2)))
      }
  })

  output$graph <- renderPlot({
    by_yr <- filter(selected_complete_data, Year == input$rb_yr)

    if (input$type4 == "All") {
      regioned <- by_yr
    } else {
      regioned <- filter(by_yr, region == input$type4)
    }

    if (input$type3 == "GDP_millions_of_USD") {
      column <- regioned$GDP_millions_of_USD
      rakes <- c(0, 995, 3900, 12055, Inf)
      bins <- cut(column, breaks = rakes, labels = c(
        paste0("$", rakes[1], " to $", rakes[2]),
        paste0("$", rakes[2], " to $", rakes[3]),
        paste0("$", rakes[3], " to $", rakes[4]),
        paste0("Greater than $", rakes[4])
      ))
      regioned <- mutate(regioned, bins)
    }
    else {
      column <- regioned$GDP_per_capita_USD
      rakes <- c(0, 32, 5000, 40000, Inf)
      bins <- cut(column, breaks = rakes, labels = c(
        paste0("$", rakes[1], " to $", rakes[2]),
        paste0("$", rakes[2], " to $", rakes[3]),
        paste0("$", rakes[3], " to $", rakes[4]),
        paste0("Greater than $", rakes[4])
      ))
      regioned <- mutate(regioned, bins)
    }

    thegraph <- ggplot(regioned, na.rm = T) +
      geom_point(mapping = aes_string(y = input$radio_key, x = input$select_key2, color = bins)) +

      if (input$type3 == "GDP_per_capita_USD") {
        labs(colour = "GDP per capita (USD)")
      } else {
        labs(colour = "GDP (USD in millions)")
      }
    thegraph
  })
}

# does the first page of the shiny
page_one <- tabPanel(
  "World Map",
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
# textOutput("selected_var1")
page_two <- tabPanel(
  "Graphs",
  titlePanel("Visualization"),
  sidebarLayout( # lay out the passed content into two columns
    sidebarPanel( # lay out the passed content inside the "sidebar" column
      radioButtons(inputId = "rb_yr", label = "Pick a year", choices = c(2010, 2015)),
      selectInput(
        inputId = "select_key2", label = "Choose the independant variable (x-axis)",
        choices = c("Infant mortality" = "Infant_mortality", "Life expectancy" = "Life_expectancy", "Maternal mortality ratio" = "Maternal_mortality_ratio", "Annual population rate of change" = "Annual_population__rate_of_change", "Fertility rate" = "Fertility_rate")
      ),
      radioButtons(
        inputId = "radio_key", label = "Choose an dependant variable (y-axis)",
        choices = c("Infant mortality" = "Infant_mortality", "Life expectancy" = "Life_expectancy", "Maternal mortality ratio" = "Maternal_mortality_ratio", "Annual population rate of change" = "Annual_population__rate_of_change", "Fertility rate" = "Fertility_rate")
      ),
      selectInput(inputId = "type3", label = "Color by:", choices = c("GDP per capita USD" = "GDP_per_capita_USD", "GDP millions of USD" = "GDP_millions_of_USD")),
      selectInput(inputId = "type4", label = "Filter by region:", choices = c("All", "Africa", "Americas", "Asia", "Europe", "Oceania"))
      # selectInput( inputId = "type5", label = "Filter by sub region:", choices = unique(filter(selected_complete_data, Year == input$rb_yr, region == input$type4) %>% select(sub_region)))
    ),
    mainPanel( # lay out the passed content inside the "main" column
      textOutput(outputId = "messagetwo"),
      span(textOutput("header"), style = "font-size:25px"),
      plotOutput(outputId = "graph"),
      textOutput("sentance")
    )
  )
)

page_three <- tabPanel(
  "Summary", titlePanel("Summary of the Program:"),
  strong("1. Does a higher fertility rate necessarily result in a higher annual rate of population growth?"),
  p("Higher fertility should theoretically correlate with a higher annual rate of population. While there is a linear correlation, 
  the slope of line is much less than 1. This is due to deaths and migration from nations, which wouldn’t allow for a 1:1 ratio of fertility. 
  We also found that countries with higher GDP generally tend to have lower annual population growth and fertility. This may be because wealthier nations
  don’t have to have children for economic reasons."),

  strong("2. How has the life expectancy of countries changed from 2010 to 2015?"),
  p("We thought it would be interesting to see the relationship between life expectancy and the GDP per capita because it represents the income of the average resident. 
  Higher income should logically result in being able to afford better healthcare. Life expectancy has not changed by a large amount between 2010 and 2015. Although the lowest 10% of the changes were negative, 
  most countries performed well, especially in Asia. As expected, the life expectancy in countries with higher GDP per capita tends to be higher than countries in the low income bracket. "),


  strong("3. Infant mortality vs. fertility rates"),
  p("When comparing fertility rates to infant mortality, we found that countries with lower GDP per capita saw higher rates of both fertility and infant mortality. 
  High infant mortality can be attributed to lack of adequate healthcare systems in countries with low GDP per capita. Intuition suggests that lower GDP would result 
  in lower fertility rates, because it’s costly to raise a child. In reality, many countries with lower GDP also can utilize children as a financial asset to do work that requires manual labor. 
  The opposite is true in countries like the US, where children are seen as a financial costs, not an investment. Although morbid, high fertility can be attributed to high infant mortality rate as well,
  as parents may look to replace children that die in infancy. Using our region filter widget, we found that European nations generally tend to have the lowest infant mortality and fertility rates.
  African nations have the highest in both categories."),

  strong("4. Annual population rate of change vs. life expectancy"),
  p("We were curious to see if the population has an effect on the life expectancy of a country. It turns out that there might be a weak linear correlation between the two. 
  Although more advanced analysis is needed to confirm this. It is possible to explain this relationship as follows; countries with lower rate of population change have fewer people and thus more access 
  to healthcare facilities per person.")
)

page_four <- tabPanel(
  "Source", titlePanel("Works Cited"),
  gdp_data <- a("GDP Data Source", href = "http://data.un.org/_Docs/SYB/PDFs/SYB60_T03_Population%20Growth,%20Fertility%20and%20Mortality%20Indicators.pdf"),
  p(life_exp_url <- a("Population Data Source", href = "http://data.un.org/_Docs/SYB/PDFs/SYB61_T13_GDP%20and%20GDP%20Per%20Capita.pdf"))
)

page_zero <- tabPanel(
  "Introduction", titlePanel("Introduction for the webPage:"),
  p("This app application utilizes GDP data and healthcare data gathered from the UN website. There are two visualization tabs. The first one contains a choropleth map on which one 
can plug in a wide range of data for the years 2010 and 2015 as well as the change between those two years. This visualization was used to determine how health statistics around the world
compared as well as changed from 2010 and 2015."),

  p("The other visualization is a graph on which one can plot variables against each other. We primarily use this visualization to answer our questions.
Each data point on the graph represents a country. The data points can be filtered by Regions as defined by the UN. The data points a colored based on GDP or GDP per capita bins. 
The bins are based on UN classifications of Low Income, Lower Middle Income, Higher Middle Income and High Income.
One can plot a variable against itself to see how it is affected by GDP/GDP per capita.")
)


my_ui <- navbarPage("Life Expectancy Analysis", page_zero, page_one, page_two, page_three, page_four)

shinyApp(ui = my_ui, server = my_server)
