library(shiny)
library(tidyverse)
library(babynames)

# Read the data into R
DF <- read_csv("MCM_NFLIS_Data.csv", 
               col_types = cols(FIPS_Combined = col_character(), 
                                FIPS_State = col_character()))

# Rename Variables
DF <- DF %>%
  rename(
    Year = YYYY,
    County = COUNTY,
    Substance = SubstanceName,
    Opioid_Reports = DrugReports,
    Total_Drug_Reports_County = TotalDrugReportsCounty,
    Total_Drug_Reports_State = TotalDrugReportsState
  )

# Compute total opioid reports for each year and county
Annual_County <- DF %>%
  group_by(Year, FIPS_Combined) %>%
  summarize(Total_Opioid_Reports_County = sum(Opioid_Reports))

# Compute total opioid reports for each year and state
Annual_State <- DF %>%
  group_by(Year, FIPS_State) %>%
  summarize(Total_Opioid_Reports_State = sum(Opioid_Reports))

# Join opioid totals back to original dataset
DF <- DF %>%
  inner_join(Annual_County, by = c("Year", "FIPS_Combined")) %>%
  inner_join(Annual_State, by = c("Year", "FIPS_State"))

# Compute the proportion of opioid reports
DF <- DF %>%
  mutate(Prop_Opioid_Reports_County = Total_Opioid_Reports_County / Total_Drug_Reports_County) %>%
  mutate(Prop_Opioid_Reports_State = Total_Opioid_Reports_State / Total_Drug_Reports_State)

# Reorder Columns
DF <- DF[c(1,2,3,4,5,6,7,8,11,12,9,10,13,14)]


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
    DF %>% 
      filter(drug == input$name, sex == input$sex) %>% 
      ggplot() +
      geom_line(aes(x = year, y = n)) +
      scale_x_continuous(limits = input$years) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)