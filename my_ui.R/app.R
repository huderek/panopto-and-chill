#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("GDP vs. Population"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        "dist", "Distribution type:", # This needs to be changed
        c(
          "2010" = "", # This needs to be changed
          "2015" = ""
        ) # This needs to be changed
      ),
      sidebarPanel(
        selectInput("", "Y-axis:",
          # This needs to be changed
          choices = colnames(WorldPhones)
        ),
        sidebarPanel(
          selectInput("", "X-axis:",
            # This needs to be changed
            choices = colnames(WorldPhones)
          ),
          sidebarPanel(
            selectInput("region", "Region:",
              # This needs to be changed
              choices = colnames(WorldPhones)
            ),

            # Show a plot of the generated distribution
            mainPanel(
              plotOutput("distPlot")
            )
          )
        )
      )
    )
  )
)


#Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = "darkgray", border = "white")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
