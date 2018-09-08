#hw1-SrinJagannath

library(shiny)
library(reshape2)
library(dplyr)
library(plotly)
library(shinythemes)
library(stringr)

# What are we loading here? Why? ;-p
iris.load <- iris %>%
  mutate(sepallength = as.numeric(Sepal.Length),
         sepalwidth = as.numeric(Sepal.Width),
         petallength = as.numeric(Petal.Length),
         petalwidth = as.numeric(Petal.Width),
         species = as.factor(Species))

pdf(NULL)

#Define UI for application that draws a violinplot 
# A comment!
ui <- navbarPage("Iris Navbar",
                 tabPanel("Plot",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("speciesSelect",
                                          "Species",
                                          choices = sort(unique(iris.load$Species)),
                                          multiple = TRUE,
                                          selectize = TRUE,
                                          selected = c("setosa", "virginica")),
                              sliderInput("plengthSelect",
                                          "Petal Length Range",
                                          min = min(iris.load$Petal.Length, na.rm = T),
                                          max = max(iris.load$Petal.Length, na.rm = T),
                                          value = c(min(iris.load$Petal.Length, na.rm = T), max(iris.load$Petal.Length, na.rm = T)))
                            ),
                            #Output plot
                            mainPanel(
                              plotlyOutput("plot")
                            )
                          )
                 ),
                 #Data Table
                 tabPanel("Table", 
                          fluidPage(DT::dataTableOutput("table"))
                 )
)

#Define Server Logic
# I'd like some comments somewhere down here!
server <- function(input, output) {
  swInput <- reactive({
    iris <- iris.load %>%
      filter(Petal.Length >= input$plengthSelect[1] & Petal.Length <= input$plengthSelect[2])
    
    if (length(input$speciesSelect) > 0 ) {
      iris <- subset(iris, Species %in% input$speciesSelect)
    }
  })
  mwInput <- reactive({
    # Humor me in the future and change the names of these guys no points off because it works. Copying IS the sincerest form of flattery.
    swInput() %>%
      melt(id = "Species")
  })
  output$plot <- renderPlotly ({
    dat <- swInput()
    ggplotly (
      ggplot(data = dat, aes(x = Species, y = Petal.Length, fill = Species)) +
        labs(title = "Petal Lengths of Flower Species", x = "Flower Species", Y = "Petal Length") +
        geom_violin() +
        guides(color = FALSE)
      , tooltip = "text")
  })
  output$table <- DT::renderDataTable({
    iris <- swInput()
    
    subset(iris, select = c(Species, Petal.Length, Petal.Width, Sepal.Length, Sepal.Width))
    
  })
}

# Run the application
shinyApp(ui = ui, server = server)