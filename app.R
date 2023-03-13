#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(colourpicker)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          uiOutput(outputId = "select_colors")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  observe(print(reactiveValuesToList(input)))

  data <- reactive(iris)

  distinct_values_for_which_colors_are_required <- reactive(as.character(unique(data()[["Species"]])))

  output$distPlot <- renderPlot({

    input_list <- reactiveValuesToList(input)

    req(all(
      distinct_values_for_which_colors_are_required() %in% names(input_list)
    ))

    color_vec <-
      data.frame("name" = names(input_list[distinct_values_for_which_colors_are_required()]),
                 "color" = unlist(input_list[distinct_values_for_which_colors_are_required()]))

    ggplot(iris) +
      aes(x = Sepal.Length, y = Sepal.Width, colour = Species) +
      geom_point() +
      scale_color_manual(values = color_vec$color, breaks = color_vec$name)

  })

  output$select_colors <- renderUI({
    # distinct_values <- as.character(unique(data()[["Species"]]))
    nvalues <-
      length(distinct_values_for_which_colors_are_required())

    lapply(1:nvalues, function(i) {
      colourInput(
        distinct_values_for_which_colors_are_required()[i],
        paste("Select colour", i) ,
        colors()[i * sample(2:20, size = 1)]
      )

    })
  })

}

# Run the application
shinyApp(ui = ui, server = server)
