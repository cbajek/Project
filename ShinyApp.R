library(shiny)
library(tidyverse)
library(babynames)

ui <- fluidPage(
  sliderInput(inputId = "years", label = "Year Range",
              min = 2010, max = 2017, value = c(2010, 2017),sep = ""),
  textInput("drug", "Drug", value = "", placeholder = "Oxycodone"),
  selectInput("state", "State", choices = list(KY = "Kentucky", OH = "Ohio", PA = "Pennsylvania", VA = "Virginia", WV = "West Virginia")),
  submitButton(text = "Create Plot"),
  plotOutput(outputId = "timeplot")
)

server <- function(input, output) {
  output$timeplot <- renderPlot({
    babynames %>% 
      filter(name == input$name, sex == input$sex) %>% 
      ggplot() +
      geom_line(aes(x = year, y = n)) +
      scale_x_continuous(limits = input$years) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)